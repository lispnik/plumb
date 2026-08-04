;;;; channel.lisp -- bounded FIFO with backpressure and two independent closes.
;;;;
;;;; PRODUCER-CLOSED is EOF: "no more data is coming."   -> RECV returns NIL NIL
;;;; CONSUMER-CLOSED is SIGPIPE: "nobody is reading."    -> SEND signals CHANNEL-CLOSED
;;;;
;;;; The two are deliberately separate.  A stage like TAKE terminates by closing
;;;; its *input*, and the resulting CHANNEL-CLOSED from the upstream SEND is what
;;;; tears the rest of the pipeline down, backwards, without any laziness in the
;;;; data representation.

(in-package #:plumb)

(defvar *default-capacity* 64
  "Default channel depth.  This is the only knob that controls backpressure:
a producer runs at most this many objects ahead of its consumer.")

(defstruct (channel (:constructor %make-channel) (:copier nil))
  (name      nil)
  (lock      (sb-thread:make-mutex :name "plumb-channel") :type sb-thread:mutex)
  (not-empty (sb-thread:make-waitqueue)                   :type sb-thread:waitqueue)
  (not-full  (sb-thread:make-waitqueue)                   :type sb-thread:waitqueue)
  (head nil :type list)
  (tail nil :type list)
  (count 0 :type fixnum)
  (capacity *default-capacity* :type fixnum)
  (discard nil)                         ; /dev/null: SEND succeeds, nothing buffered
  ;; How many CLOSE-OUTPUT calls are still owed before this is really EOF.  One
  ;; for an ordinary channel; RUN raises it for the :err port, which every
  ;; stage in a pipeline sends to.
  (producers 1 :type fixnum)
  (producer-closed nil)
  (consumer-closed nil))

(defun make-channel (&key name (capacity *default-capacity*) discard (producers 1))
  (%make-channel :name name :capacity capacity :discard discard :producers producers))

(define-condition channel-closed (error)
  ((channel :initarg :channel :reader channel-closed-channel))
  (:report (lambda (c s)
             (format s "Channel ~a is closed by its consumer."
                     (or (channel-name (channel-closed-channel c)) "#<anonymous>")))))

;;; Queue primitives.  Callers hold the lock.

(declaim (inline %enq %deq))

(defun %enq (ch obj)
  (let ((cell (list obj)))
    (if (channel-tail ch)
        (setf (cdr (channel-tail ch)) cell
              (channel-tail ch) cell)
        (setf (channel-head ch) cell
              (channel-tail ch) cell))
    (incf (channel-count ch))))

(defun %deq (ch)
  (let ((cell (channel-head ch)))
    (setf (channel-head ch) (cdr cell))
    (unless (channel-head ch)
      (setf (channel-tail ch) nil))
    (decf (channel-count ch))
    (car cell)))

;;; Public operations.

(defun send (ch obj)
  "Put OBJ on CH, blocking while the channel is full.
Signals CHANNEL-CLOSED if the consumer has gone away."
  (when (channel-discard ch)
    (return-from send obj))
  (sb-thread:with-mutex ((channel-lock ch))
    (loop
      ;; Re-checked after every wakeup, which is what makes the SIGPIPE path
      ;; work even for a producer that was already parked on a full channel.
      (when (channel-consumer-closed ch)
        (error 'channel-closed :channel ch))
      (when (< (channel-count ch) (channel-capacity ch))
        (return))
      (sb-thread:condition-wait (channel-not-full ch) (channel-lock ch)))
    (%enq ch obj)
    (sb-thread:condition-notify (channel-not-empty ch)))
  obj)

(defun recv (ch)
  "Take the next object from CH, blocking while empty.
Returns (VALUES object T), or (VALUES NIL NIL) at EOF.  Two values, so that
NIL remains a perfectly legal payload."
  (sb-thread:with-mutex ((channel-lock ch))
    (loop
      ;; Buffered items drain before EOF is reported.
      (when (plusp (channel-count ch))
        (let ((obj (%deq ch)))
          (sb-thread:condition-notify (channel-not-full ch))
          (return (values obj t))))
      (when (channel-producer-closed ch)
        (return (values nil nil)))
      (sb-thread:condition-wait (channel-not-empty ch) (channel-lock ch)))))

(defun close-output (ch)
  "Producer side: no more data FROM ME.  Readers drain the buffer, then see EOF
-- but only once the last producer has closed.  The :err port is shared by
every stage in a pipeline, and the first stage to finish must not close it out
from under the others: SEND does not check PRODUCER-CLOSED, so a later stage's
condition would be delivered into a buffer whose reader had already stopped."
  (sb-thread:with-mutex ((channel-lock ch))
    (when (plusp (channel-producers ch))
      (decf (channel-producers ch)))
    (when (zerop (channel-producers ch))
      (setf (channel-producer-closed ch) t)
      (sb-thread:condition-broadcast (channel-not-empty ch))))
  ch)

(defun close-input (ch)
  "Consumer side: stop.  Wakes any producer parked on a full channel so it can
signal CHANNEL-CLOSED.  The buffer is dropped -- those objects may be large and
nothing is ever going to look at them again."
  (sb-thread:with-mutex ((channel-lock ch))
    (setf (channel-consumer-closed ch) t
          (channel-head ch) nil
          (channel-tail ch) nil
          (channel-count ch) 0)
    (sb-thread:condition-broadcast (channel-not-full ch))
    (sb-thread:condition-broadcast (channel-not-empty ch)))
  ch)
