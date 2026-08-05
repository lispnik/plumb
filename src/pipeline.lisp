;;;; pipeline.lisp -- wiring, spawning, and teardown.  Everything difficult
;;;; lives in SPAWN-STAGE's UNWIND-PROTECT.

(in-package #:plumb)

(defstruct (pipeline (:copier nil))
  (stages '())
  (threads '())
  (channels '())
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
   (downstream :initarg :downstream :reader pipeline-type-error-downstream))
  (:report
   (lambda (c s)
     (let ((up (pipeline-type-error-upstream c))
           (down (pipeline-type-error-downstream c)))
       (format s "~a produces ~s but ~a consumes ~s."
               (stage-name up) (stage-produces up)
               (stage-name down) (stage-consumes down))))))

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

(defun spawn-stage (stage in outs &key pipeline)
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
         (when in (close-input in)))))
   :name (format nil "plumb:~a" (stage-name stage))))

(defun run (stages &key sink err (capacity *default-capacity*) (check t))
  "Wire STAGES into a pipeline and start it.  Returns a PIPELINE.
SINK, if given, is a channel receiving the last stage's output; otherwise
output is discarded.  ERR likewise for conditions."
  (setf stages (remove nil stages))
  (assert stages () "Empty pipeline.")
  (when check (check-pipeline stages))
  (let* ((n (length stages))
         (chans (loop repeat (1- n)
                      for i from 0
                      collect (make-channel :capacity capacity
                                            :name (format nil "~a->~a"
                                                          (stage-name (nth i stages))
                                                          (stage-name (nth (1+ i) stages))))))
         (sink (or sink (make-channel :discard t :name "sink")))
         (err (or err (make-channel :discard t :name "err")))
         (pipe (make-pipeline :stages stages :channels chans :sink sink :err err)))
    ;; Every stage may SEND to :err, so it owes N closes before it is EOF.
    ;; RUN takes ownership of the channel's producer count; a channel shared
    ;; between two pipelines needs the caller to account for both.
    (setf (channel-producers err) n)
    (setf (pipeline-threads pipe)
          (loop for s in stages
                for i from 0
                collect (spawn-stage s
                                     (if (zerop i) nil (nth (1- i) chans))
                                     (list :out (if (= i (1- n)) sink (nth i chans))
                                           :err err)
                                     :pipeline pipe)))
    pipe))

(defun join (pipeline &key errorp)
  "Wait for every stage to finish.  With ERRORP, resignal the first failure."
  (mapc (lambda (th) (ignore-errors (sb-thread:join-thread th :default nil)))
        (pipeline-threads pipeline))
  (let ((failures (reverse (pipeline-failures pipeline))))
    (when (and errorp failures)
      (error 'pipeline-error :stage (car (first failures)) :cause (cdr (first failures))))
    failures))

(defun cancel (pipeline)
  "Ctrl-C.  Closing the first channel from the consumer side starves the source,
and the teardown cascade does the rest."
  (dolist (ch (pipeline-channels pipeline)) (close-input ch))
  (close-input (pipeline-sink pipeline))
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
