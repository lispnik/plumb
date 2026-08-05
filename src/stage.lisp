;;;; stage.lisp -- a stage is a closure plus a type signature.  It contains no
;;;; concurrency: ports arrive through dynamic bindings that the runner
;;;; establishes, so the body of a stage is an ordinary loop.

(in-package #:plumb)

(defvar *input* nil
  "The channel this stage reads from, or NIL for a source.")
(defvar *outputs* '()
  "Plist of port name -> channel.  :OUT and :ERR always present.")
(defvar *stage-name* nil)

(defstruct (stage (:copier nil))
  (name nil)
  (thunk nil :type (or null function))
  (consumes t)                          ; :objects :bytes NIL(=source) or T(=any)
  (produces t)                          ; :objects :bytes NIL(=sink)  or T(=any)
  (ports '(:out :err))
  ;; :PRODUCES describes :OUT alone, so a named port carries its own type or
  ;; the graph would be untyped exactly where it branches.
  (port-types '())
  (barrier nil)                         ; emits nothing until its input EOFs
  (args '()))

(defmethod print-object ((s stage) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a ~s->~s" (stage-name s) (stage-consumes s) (stage-produces s))))

(defun port (&optional (name :out))
  (or (getf *outputs* name)
      (error "Stage ~a has no ~s port." *stage-name* name)))

(defmacro emit (object &optional (port :out))
  "Write OBJECT to one of this stage's output ports."
  `(send (port ,port) ,object))

(defmacro try-emit (object &optional (port :out))
  "EMIT unless that port's reader has gone; T when the object was taken.

EMIT is deliberately strict: a closed reader signals CHANNEL-CLOSED, which
unwinds the stage and is exactly how TAKE stops an infinite source.  A routing
stage wants the opposite -- one branch finishing must not stop the others -- so
it emits with this instead."
  `(handler-case (progn (emit ,object ,port) t)
     (channel-closed () nil)))

(defmacro finish ()
  "Terminate this stage early.  Unwinding closes the input channel, which
propagates CHANNEL-CLOSED backwards through the pipeline."
  '(throw 'plumb-stage-finish nil))

(defmacro do-input ((var &optional (channel '*input*)) &body body)
  "Iterate over the objects arriving on CHANNEL until EOF."
  (let ((ok (gensym "OK")) (ch (gensym "CH")))
    `(let ((,ch ,channel))
       (unless ,ch
         (error "Stage ~a tried to read input but is wired as a source." *stage-name*))
       (loop
         (multiple-value-bind (,var ,ok) (recv ,ch)
           (declare (ignorable ,var))
           (unless ,ok (return))
           ,@body)))))

;;; DEFSTAGE
;;;
;;; Required parameters may be written (NAME TYPE) and get a CHECK-TYPE.
;;; Everything from the first lambda-list keyword onwards is an ordinary
;;; lambda list.  Leading (:consumes X) / (:produces X) / (:ports ...) /
;;; (:barrier X) / (:check FORM...) forms in the body are declarations, not
;;; code -- all but :CHECK, whose forms become the first thing the constructor
;;; runs.

(defun %split-arglist (arglist)
  (let ((pos (position-if (lambda (x) (and (symbolp x) (eql 0 (search "&" (string x)))))
                          arglist)))
    (if pos
        (values (subseq arglist 0 pos) (subseq arglist pos))
        (values arglist '()))))

(defun %parse-stage-body (body)
  (let ((consumes t) (produces t) (ports '(:out :err)) (port-types '())
        (barrier nil) (doc nil) (checks '()))
    (when (and (stringp (car body)) (cdr body))
      (setf doc (pop body)))
    (loop while (and (consp (car body))
                     (member (caar body) '(:consumes :produces :ports :barrier :check)))
          for form = (pop body)
          do (ecase (first form)
               (:consumes (setf consumes (second form)))
               (:produces (setf produces (second form)))
               (:barrier  (setf barrier (second form)))
               ;; Validation that runs in the CONSTRUCTOR, not the thunk -- the
               ;; same reason CHECK-PIPELINE runs before a thread exists.  A
               ;; (NAME TYPE) parameter already gets a CHECK-TYPE; this is for
               ;; the cases where "not of type SUPPORTED-DIGEST" is a worse
               ;; message than the stage can write itself, so these forms run
               ;; first and the declared CHECK-TYPEs are the fallback.
               (:check (setf checks (append checks (rest form))))
               ;; (:ports :yes :no) or (:ports (:yes :bytes) :no).  A bare
               ;; name carries :OBJECTS, which is what a branch almost always
               ;; wants and what :CONSUMES already defaults to elsewhere.
               (:ports
                (let ((specs (rest form)))
                  (setf port-types
                        (loop for spec in specs
                              collect (if (consp spec)
                                          (cons (first spec) (second spec))
                                          (cons spec :objects)))
                        ports
                        (union (mapcar (lambda (s) (if (consp s) (first s) s)) specs)
                               '(:out :err)))))))
    (values consumes produces ports port-types barrier doc checks body)))

;;; The registry behind HELP.  DEFSTAGE knows the type signature and the
;;; docstring at definition time; a STAGE instance only exists once someone has
;;; called the constructor, so this has to be recorded here or not at all.

(defvar *stages* (make-hash-table :test #'eq)
  "Stage name -> STAGE-INFO, for HELP.  Populated by DEFSTAGE.")

(defstruct (stage-info (:conc-name si-) (:copier nil))
  name lambda-list consumes produces ports port-types barrier documentation)

(defun stage-kind (info)
  (cond ((null (si-consumes info)) :source)
        ((null (si-produces info)) :sink)
        (t :transform)))

(defmacro defstage (name arglist &body body)
  "Define a stage constructor.  Calling it returns a STAGE; running a pipeline
is what actually spawns a thread."
  (multiple-value-bind (required rest-of-lambda-list) (%split-arglist arglist)
    (multiple-value-bind (consumes produces ports port-types barrier doc
                          constructor-checks real-body)
        (%parse-stage-body body)
      (let* ((req-names (mapcar (lambda (p) (if (consp p) (first p) p)) required))
             (checks (loop for p in required
                           when (consp p)
                             collect `(check-type ,(first p) ,(second p))))
             (all-names (append req-names
                                (loop for x in rest-of-lambda-list
                                      unless (and (symbolp x)
                                                  (eql 0 (search "&" (string x))))
                                        collect (if (consp x) (first x) x)))))
        `(progn
           (setf (gethash ',name *stages*)
                 (make-stage-info :name ',name
                                  :lambda-list ',arglist
                                  :consumes ,consumes
                                  :produces ,produces
                                  :ports ',ports
                                  :port-types ',port-types
                                  :barrier ,barrier
                                  :documentation ,doc))
           (defun ,name (,@req-names ,@rest-of-lambda-list)
             ,@(when doc (list doc))
             ,@constructor-checks
             ,@checks
             (make-stage :name ',name
                         :consumes ,consumes
                         :produces ,produces
                         :ports ',ports
                         :port-types ',port-types
                         :barrier ,barrier
                         :args (list ,@(loop for n in all-names
                                             append (list (intern (string n) :keyword) n)))
                         :thunk (lambda () ,@real-body))))))))
