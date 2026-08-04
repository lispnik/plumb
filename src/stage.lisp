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
;;; lambda list.  Leading (:consumes X) / (:produces X) / (:ports ...) forms in
;;; the body are declarations, not code.

(defun %split-arglist (arglist)
  (let ((pos (position-if (lambda (x) (and (symbolp x) (eql 0 (search "&" (string x)))))
                          arglist)))
    (if pos
        (values (subseq arglist 0 pos) (subseq arglist pos))
        (values arglist '()))))

(defun %parse-stage-body (body)
  (let ((consumes t) (produces t) (ports '(:out :err)) (doc nil))
    (when (and (stringp (car body)) (cdr body))
      (setf doc (pop body)))
    (loop while (and (consp (car body))
                     (member (caar body) '(:consumes :produces :ports)))
          for form = (pop body)
          do (ecase (first form)
               (:consumes (setf consumes (second form)))
               (:produces (setf produces (second form)))
               (:ports (setf ports (union (rest form) '(:out :err))))))
    (values consumes produces ports doc body)))

;;; The registry behind HELP.  DEFSTAGE knows the type signature and the
;;; docstring at definition time; a STAGE instance only exists once someone has
;;; called the constructor, so this has to be recorded here or not at all.

(defvar *stages* (make-hash-table :test #'eq)
  "Stage name -> STAGE-INFO, for HELP.  Populated by DEFSTAGE.")

(defstruct (stage-info (:conc-name si-) (:copier nil))
  name lambda-list consumes produces ports documentation)

(defun stage-kind (info)
  (cond ((null (si-consumes info)) :source)
        ((null (si-produces info)) :sink)
        (t :transform)))

(defmacro defstage (name arglist &body body)
  "Define a stage constructor.  Calling it returns a STAGE; running a pipeline
is what actually spawns a thread."
  (multiple-value-bind (required rest-of-lambda-list) (%split-arglist arglist)
    (multiple-value-bind (consumes produces ports doc real-body)
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
                                  :documentation ,doc))
           (defun ,name (,@req-names ,@rest-of-lambda-list)
             ,@(when doc (list doc))
             ,@checks
             (make-stage :name ',name
                         :consumes ,consumes
                         :produces ,produces
                         :ports ',ports
                         :args (list ,@(loop for n in all-names
                                             append (list (intern (string n) :keyword) n)))
                         :thunk (lambda () ,@real-body))))))))
