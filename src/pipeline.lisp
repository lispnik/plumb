;;;; pipeline.lisp -- wiring, spawning, and teardown.  Everything difficult
;;;; lives in SPAWN-STAGE's UNWIND-PROTECT.

(in-package #:plumb)

(defstruct (pipeline (:copier nil))
  (stages '())
  (threads '())
  (channels '())
  (branches '())                        ; sub-pipelines fed by named ports
  (sink nil)
  (err nil)
  (failures '())
  (lock (sb-thread:make-mutex :name "plumb-pipeline")))

(define-condition pipeline-error (error)
  ((stage :initarg :stage :reader pipeline-error-stage)
   (cause :initarg :cause :reader pipeline-error-cause))
  (:report (lambda (c s)
             (format s "Stage ~a failed: ~a"
                     (pipeline-error-stage c) (pipeline-error-cause c)))))

(define-condition pipeline-type-error (error)
  ((upstream :initarg :upstream :reader pipeline-type-error-upstream)
   (downstream :initarg :downstream :reader pipeline-type-error-downstream)
   ;; Which port disagreed.  :OUT for the main line; a named port otherwise,
   ;; and then :PRODUCES is the wrong thing to quote -- it describes :OUT.
   (port :initarg :port :initform :out :reader pipeline-type-error-port))
  (:report
   (lambda (c s)
     (let ((up (pipeline-type-error-upstream c))
           (down (pipeline-type-error-downstream c))
           (port (pipeline-type-error-port c)))
       (if (eq port :out)
           (format s "~a produces ~s but ~a consumes ~s."
                   (stage-name up) (stage-produces up)
                   (stage-name down) (stage-consumes down))
           (format s "~a's ~s port carries ~s but ~a consumes ~s."
                   (stage-name up) port (port-type up port)
                   (stage-name down) (stage-consumes down)))))))

(defvar *pipeline* nil "The pipeline the current stage thread belongs to.")

(defun %compatible-p (produced consumed)
  "Can these two be joined?  NIL on the producing side means `produces nothing',
which nothing may follow -- T on the consuming side means any object *type*,
not the absence of one.  Reading it as the latter let a sink follow a sink,
and EXPLAIN reported that pipeline as fine."
  (and produced consumed
       (or (eq produced t) (eq consumed t) (eql produced consumed))))

(defun check-pipeline (stages)
  "Validate a pipeline before a single thread is spawned.  A mismatch here is a
clear error instead of a deadlock or a type error 400 items in."
  (loop for (up down) on stages while down
        do (unless (%compatible-p (stage-produces up) (stage-consumes down))
             (error 'pipeline-type-error :upstream up :downstream down)))
  t)

(defun %record-failure (pipeline stage condition)
  (when pipeline
    (sb-thread:with-mutex ((pipeline-lock pipeline))
      (push (cons (stage-name stage) condition) (pipeline-failures pipeline)))))

(defun spawn-stage (stage in outs &key pipeline (worker 0))
  (sb-thread:make-thread
   (lambda ()
     (let ((*input* in)
           (*outputs* outs)
           (*stage-name* (stage-name stage))
           (*pipeline* pipeline))
       (unwind-protect
            (handler-case
                (catch 'plumb-stage-finish
                  (funcall (stage-thunk stage)))
              ;; Downstream closed on us.  Normal termination, not an error:
              ;; this is the SIGPIPE case.
              (channel-closed () nil)
              (error (c)
                (%record-failure pipeline stage c)
                (let ((err (getf outs :err)))
                  (when err (ignore-errors (send err c))))))
         ;; Runs on every exit path -- normal EOF, (finish), or error.  This is
         ;; what makes teardown propagate in both directions.
         (loop for (nil ch) on outs by #'cddr
               do (unless (or (null ch) (channel-discard ch))
                    (close-output ch)))
         ;; Refcounted on both sides now, so with several workers the last one
         ;; out does the closing and the others just retire.
         (when in (close-input in)))))
   :name (if (plusp worker)
             (format nil "plumb:~a/~d" (stage-name stage) worker)
             (format nil "plumb:~a" (stage-name stage)))))

(defun extra-ports (stage)
  "The output ports beyond :OUT and :ERR that STAGE declared."
  (remove-if (lambda (p) (member p '(:out :err))) (stage-ports stage)))

(defun port-type (stage port)
  "What PORT carries.  :PRODUCES describes :OUT; every other port says so
itself, defaulting to :OBJECTS."
  (if (eq port :out)
      (stage-produces stage)
      (or (cdr (assoc port (stage-port-types stage))) :objects)))

(defun wire-branches (stages ports capacity pipe check)
  "Give every declared port a channel, and start the branch reading it.

This is the graph builder.  A stage declaring (:ports :small :large) gets those
names in its *OUTPUTS*, and each one feeds its own pipeline, supplied by RUN's
:PORTS argument.  A declared port with no branch is wired to a discard channel
rather than left missing -- EMIT to it then succeeds and goes nowhere, which
EXPLAIN reports, instead of erroring inside a thread.

Port names are one flat namespace across a pipeline, so two stages cannot both
declare :LEFT and expect different branches.  One level of demux is what a
shell wants; anything deeper nests by putting a routing stage inside a branch."
  (let ((wiring '()))
    (dolist (stage stages wiring)
      (dolist (port (extra-ports stage))
        (let ((branch (getf ports port)))
          (cond
            (branch
             (when check
               (let ((head (first (remove nil branch))))
                 (unless (%compatible-p (port-type stage port) (stage-consumes head))
                   (error 'pipeline-type-error :upstream stage :downstream head
                                               :port port))))
             (let ((channel (make-channel :capacity capacity
                                          :producers (stage-workers stage)
                                          :name (format nil "~a:~(~a~)"
                                                        (stage-name stage) port))))
               (push (run branch :input channel :capacity capacity :check check)
                     (pipeline-branches pipe))
               (push (cons stage (list port channel)) wiring)))
            (t (push (cons stage (list port (make-channel :discard t
                                                          :name (format nil "~(~a~):discarded"
                                                                        port))))
                     wiring))))))))

(defun run (stages &key sink err input ports (capacity *default-capacity*) (check t))
  "Wire STAGES into a pipeline and start it.  Returns a PIPELINE.
SINK, if given, is a channel receiving the last stage's output; otherwise
output is discarded.  ERR likewise for conditions.  INPUT, if given, is a
channel the FIRST stage reads from -- which is what lets one pipeline feed
another, and so what TEE is built on.  A pipeline given an INPUT starts with a
transform rather than a source, so CHECK-PIPELINE has nothing extra to say.

PORTS is a plist of port name -> branch pipeline, wiring the extra output ports
a stage declared with (:ports ...).  See WIRE-BRANCHES."
  (setf stages (remove nil stages))
  (assert stages () "Empty pipeline.")
  (when check (check-pipeline stages))
  (let* ((n (length stages))
         (workers (mapcar #'stage-workers stages))
         (chans (loop repeat (1- n)
                      for i from 0
                      collect (make-channel :capacity capacity
                                            :name (format nil "~a->~a"
                                                          (stage-name (nth i stages))
                                                          (stage-name (nth (1+ i) stages))))))
         (sink (or sink (make-channel :discard t :name "sink")))
         (err (or err (make-channel :discard t :name "err")))
         (pipe (make-pipeline :stages stages :channels chans :sink sink :err err)))
    ;; Refcounts.  Every thread that will ever SEND to a channel owes it one
    ;; CLOSE-OUTPUT, and every thread that will ever RECV owes it one
    ;; CLOSE-INPUT -- so both counts are thread counts, not stage counts.
    ;; Getting either wrong shows up as a pipeline that hangs or as EOF
    ;; arriving while somebody is still writing.
    (loop for ch in chans
          for i from 0
          do (setf (channel-producers ch) (nth i workers)
                   (channel-consumers ch) (nth (1+ i) workers)))
    (setf (channel-producers sink) (car (last workers)))
    (when input
      (setf (channel-consumers input) (first workers)))
    ;; Every stage may SEND to :err, so it owes one close per *thread*.  RUN
    ;; takes ownership of the channel's producer count; a channel shared
    ;; between two pipelines needs the caller to account for both.
    (setf (channel-producers err) (reduce #'+ workers))
    (let ((wiring (wire-branches stages ports capacity pipe check)))
      (setf (pipeline-threads pipe)
            (loop for s in stages
                  for i from 0
                  append (let ((in (if (zerop i) input (nth (1- i) chans)))
                               (outs (append (list :out (if (= i (1- n)) sink (nth i chans))
                                                   :err err)
                                             ;; SPAWN-STAGE closes every port
                                             ;; it is handed, so a named port
                                             ;; gets its EOF for free.
                                             (loop for (stage . binding) in wiring
                                                   when (eq stage s) append binding))))
                           ;; Every worker of a stage shares one input and one
                           ;; set of outputs.  RECV under the channel mutex is
                           ;; what distributes the work, so there is no
                           ;; scheduler here and none is needed.
                           (loop for w from 1 to (stage-workers s)
                                 collect (spawn-stage s in outs :pipeline pipe
                                                      :worker (if (= 1 (stage-workers s))
                                                                  0 w)))))))
    pipe))

(defun join (pipeline &key errorp)
  "Wait for every stage to finish, branches included.  With ERRORP, resignal
the first failure -- a branch's failures count as the pipeline's, since from
outside there is one pipeline."
  (mapc (lambda (th) (ignore-errors (sb-thread:join-thread th :default nil)))
        (pipeline-threads pipeline))
  (let ((failures (append (reverse (pipeline-failures pipeline))
                          (loop for branch in (pipeline-branches pipeline)
                                append (ignore-errors (join branch))))))
    (when (and errorp failures)
      (error 'pipeline-error :stage (car (first failures)) :cause (cdr (first failures))))
    failures))

(defun cancel (pipeline)
  "Ctrl-C.  Closing the first channel from the consumer side starves the source,
and the teardown cascade does the rest.

ABORT-INPUT rather than CLOSE-INPUT: the latter is refcounted, and against a
stage running eight workers a single decrement would retire one of them and
leave the other seven reading."
  (dolist (ch (pipeline-channels pipeline)) (abort-input ch))
  (abort-input (pipeline-sink pipeline))
  (dolist (branch (pipeline-branches pipeline)) (cancel branch))
  (join pipeline)
  pipeline)

(defun each (stages function &key (capacity *default-capacity*) errorp)
  "Run STAGES, calling FUNCTION on each object the last stage emits.
The calling thread is the consumer, so backpressure reaches all the way back."
  (let* ((sink (make-channel :capacity capacity :name "each"))
         (pipe (run stages :sink sink :capacity capacity)))
    (unwind-protect
         (loop (multiple-value-bind (obj ok) (recv sink)
                 (unless ok (return))
                 (funcall function obj)))
      (close-input sink))
    (join pipe :errorp errorp)
    pipe))

(defun collect-pipeline (stages &key (capacity *default-capacity*) errorp)
  "Run STAGES and return the last stage's output as a list."
  (let ((acc '()))
    (each stages (lambda (x) (push x acc)) :capacity capacity :errorp errorp)
    (nreverse acc)))
