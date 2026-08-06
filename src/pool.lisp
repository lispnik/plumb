;;;; pool.lisp -- stage threads, leased instead of created.
;;;;
;;;; SB-THREAD:MAKE-THREAD costs about 28us here; handing work to a thread that
;;;; is already parked on a semaphore costs about 2.6us.  A four-stage pipeline
;;;; therefore spends ~113us of its ~214us setup just making threads, and a loop
;;;; that runs a pipeline per file pays that every time.  This is the whole
;;;; reason the pool exists.
;;;;
;;;; It is worth being clear about what it is NOT.  Stage fusion was the
;;;; alternative, and measurement said it buys CPU (~37% less on a long
;;;; pipeline) but no wall-clock latency at all -- stages already run
;;;; concurrently, so the channel cost is paid in parallel.  Pooling attacks the
;;;; cost that measurement actually found, and leaves EMIT, the stage protocol
;;;; and one-thread-per-stage alone.
;;;;
;;;; THE POOL IS ELASTIC AND MUST STAY THAT WAY.  It is a cache of idle threads,
;;;; never a limit on how many stages can run.  Every stage of a pipeline has to
;;;; be running for the pipeline to make progress: a stage waiting for a free
;;;; worker while the stage ahead of it blocks on a full channel is a deadlock,
;;;; not a slow start.  SPAWN never waits.

(in-package #:plumb)

(defparameter *idle-workers* 32
  "How many parked threads to keep.  Surplus workers exit when they finish
rather than lingering, so a burst of pipelines does not leave hundreds of parked
threads behind for the rest of the session.")

(defstruct (worker (:copier nil))
  thread
  (wakeup (sb-thread:make-semaphore) :type sb-thread:semaphore)
  ;; Written before WAKEUP is signalled and read after it is waited on, so the
  ;; semaphore is the whole synchronisation.
  task)

(defstruct (task (:copier nil))
  name
  thunk
  (done (sb-thread:make-semaphore) :type sb-thread:semaphore)
  (finished nil)
  (condition nil))

(defvar *pool-lock* (sb-thread:make-mutex :name "plumb-pool"))
(defvar *pool* '() "Parked workers, most recently used first.")
(defvar *pool-threads* 0 "Threads this pool has alive, idle or working.")
(defvar *pool-created* 0 "Threads it has ever made -- reuse is measured by this.")

(defun pool-statistics ()
  "Threads alive, threads ever created, workers parked.  For the tests, which
have to show that running many pipelines does not keep making threads."
  (sb-thread:with-mutex (*pool-lock*)
    (list :alive *pool-threads* :created *pool-created* :idle (length *pool*))))

(defun %take-idle-worker ()
  (sb-thread:with-mutex (*pool-lock*) (pop *pool*)))

(defun %return-worker (worker)
  "Park WORKER for reuse.  NIL means the cache is full and it should exit."
  (sb-thread:with-mutex (*pool-lock*)
    (cond ((< (length *pool*) *idle-workers*)
           (push worker *pool*)
           t)
          (t (decf *pool-threads*)
             nil))))

(defun %run-task (task)
  "Run one task to completion, whatever it does.

The UNWIND-PROTECT is the contract: DONE is signalled on every exit path, so
AWAIT cannot hang because a stage died in a way nobody anticipated.  SPAWN-STAGE
has its own handler for the conditions a stage is expected to signal; this one
exists for the ones it is not."
  (let ((previous (sb-thread:thread-name sb-thread:*current-thread*)))
    (unwind-protect
         (handler-case
             (progn
               ;; Names are per task, so `plumb:ls` and `plumb:digest/3` still
               ;; show up in backtraces and LIST-ALL-THREADS on a shared worker.
               (ignore-errors
                (setf (sb-thread:thread-name sb-thread:*current-thread*)
                      (task-name task)))
               (funcall (task-thunk task)))
           ;; SERIOUS-CONDITION, not CONDITION: a stage that merely WARNs must
           ;; not be torn down.  SPAWN-STAGE handles what a stage is expected to
           ;; signal; this is the backstop for what it is not, and its job is to
           ;; keep the worker alive rather than to interpret anything.
           (serious-condition (c) (setf (task-condition task) c)))
      (ignore-errors
       (setf (sb-thread:thread-name sb-thread:*current-thread*) previous))
      (setf (task-thunk task) nil        ; the closure may hold a whole pipeline
            (task-finished task) t)
      (sb-thread:signal-semaphore (task-done task)))))

(defun %worker-loop (worker)
  (loop
    (sb-thread:wait-on-semaphore (worker-wakeup worker))
    (let ((task (shiftf (worker-task worker) nil)))
      (unless task (return))            ; NIL task is the retirement signal
      (%run-task task)
      (unless (%return-worker worker) (return)))))

(defun %make-worker ()
  (let ((worker (make-worker)))
    (setf (worker-thread worker)
          (sb-thread:make-thread (lambda () (%worker-loop worker))
                                 :name "plumb:idle"))
    worker))

(defun spawn (name thunk)
  "Run THUNK on a pooled thread under NAME.  Returns a TASK to AWAIT.

Never waits for a worker to become free: if none is parked it makes one.  A
bounded pool would deadlock the moment a pipeline had more stages than workers,
because every stage of a pipeline must be running for any of it to progress."
  (let ((task (make-task :name name :thunk thunk))
        (worker (%take-idle-worker)))
    (unless worker
      (sb-thread:with-mutex (*pool-lock*)
        (incf *pool-threads*)
        (incf *pool-created*))
      (setf worker (%make-worker)))
    (setf (worker-task worker) task)
    (sb-thread:signal-semaphore (worker-wakeup worker))
    task))

(defun await (task &key (timeout nil))
  "Wait for TASK.  Idempotent -- JOIN may be called more than once on the same
pipeline, and CANCEL joins one it has already torn down."
  (when (and task (not (task-finished task)))
    (sb-thread:wait-on-semaphore (task-done task) :timeout timeout)
    ;; Put the count back: DONE is signalled once, and a second AWAIT must not
    ;; consume a permit that never arrives.
    (when (task-finished task)
      (sb-thread:signal-semaphore (task-done task))))
  (task-condition task))

(defun task-live-p (task)
  "Has TASK still not finished?  The pooled answer to THREAD-ALIVE-P: the
*thread* outlives the task by design now, so asking about the thread would
always say yes."
  (and task (not (task-finished task))))

(defun drain-pool ()
  "Retire every parked worker.  Nothing in plumb needs this -- it is for tests
that want to measure from a known state."
  (let ((parked (sb-thread:with-mutex (*pool-lock*)
                  (let ((parked *pool*))
                    ;; Count before clearing -- (length *pool*) after the SETF
                    ;; is zero, and the accounting would never go down.
                    (decf *pool-threads* (length parked))
                    (setf *pool* '())
                    parked))))
    (dolist (worker parked)
      (setf (worker-task worker) nil)
      (sb-thread:signal-semaphore (worker-wakeup worker))
      (ignore-errors (sb-thread:join-thread (worker-thread worker) :default nil))))
  (values))
