;;;; help.lisp -- the built-in that lists the other built-ins.
;;;;
;;;; HELP is a macro so that (help take) works without quoting: at a shell
;;;; prompt `help take` is what a person types, and a word-mode reader would
;;;; expand to exactly this form.  Bare HELP is a symbol macro for the same
;;;; reason, so `plumb help` works from the command line.
;;;;
;;;; Stages come from *STAGES*, which DEFSTAGE fills in.  Everything else is
;;;; read off the PLUMB package's export list, so a new export shows up in the
;;;; listing without anyone having to remember to add it here.

(in-package #:plumb)

(defparameter +help-groups+
  '((run each collect-pipeline join cancel check-pipeline
     "pipelines" "build one, drain it, take it down")
    (make-channel send recv close-output close-input *default-capacity*
     "channels" "the transport, and the one knob on it")
    (field fields $
     "objects" "reach into an object without knowing its type")
    (defstage emit finish do-input port *input* *outputs*
     "authoring" "write your own stage; all but DEFSTAGE only work inside one")
    (paint color-p visible-width *color*
     "terminal" "colour, honouring NO_COLOR and TERM"))
  "Ordering and grouping for the non-stage built-ins.  Anything exported and
missing from here still appears, under \"also exported\".")

(defun group-names (group) (remove-if-not #'symbolp group))
(defun group-title (group) (first (remove-if-not #'stringp group)))
(defun group-blurb (group) (second (remove-if-not #'stringp group)))

(defun own-symbol-p (symbol)
  "Defined by PLUMB, rather than inherited from CL.  Without this, FIND-SYMBOL
happily resolves LIST or IF and HELP starts speaking for the whole language."
  (eq (symbol-package symbol) (find-package '#:plumb)))

(defun builtin-p (symbol)
  "Names something: a function, a macro, or a variable."
  (or (fboundp symbol) (macro-function symbol) (boundp symbol)))

(defun variable-p (symbol)
  (and (boundp symbol) (not (fboundp symbol)) (not (macro-function symbol))))

(defun exported-builtins ()
  "Every exported symbol of PLUMB that names something and is not a stage."
  (let ((out '()))
    (do-external-symbols (s '#:plumb)
      (when (and (own-symbol-p s) (builtin-p s)
                 (not (gethash s *stages*))
                 (not (eq s 'help)))
        (push s out)))
    (sort out #'string< :key #'symbol-name)))

(defun stage-infos ()
  (sort (loop for info being the hash-values of *stages* collect info)
        #'string< :key (lambda (i) (symbol-name (si-name i)))))

;;; ------------------------------------------------------------------ rendering

(defun first-line (string)
  (when string
    (let ((line (subseq string 0 (or (position #\Newline string) (length string)))))
      (string-trim " " line))))

(defun summary (string &optional (width 58))
  (let ((line (first-line string)))
    (cond ((null line) "")
          ((<= (length line) width) line)
          (t (concatenate 'string (subseq line 0 (- width 1)) "…")))))

(defun down (object)
  (let ((*print-case* :downcase))
    (princ-to-string object)))

(defun down1 (object)
  "Like DOWN but readably, so a keyword keeps its colon."
  (let ((*print-case* :downcase))
    (prin1-to-string object)))

(defun render-type (x)
  (cond ((null x) "nothing")
        ((eq x t) "anything")
        (t (down1 x))))

(defun lambda-keyword-p (x)
  (and (symbolp x) (plusp (length (symbol-name x)))
       (char= (char (symbol-name x) 0) #\&)))

(defun stage-usage (info)
  "Two values: the usage line, and an alist of (parameter . type) for the
required parameters that DEFSTAGE type-checks."
  (let ((words '()) (types '()) (tail nil))
    (dolist (p (si-lambda-list info))
      (cond (tail (push (down p) words))
            ((lambda-keyword-p p) (setf tail t) (push (down p) words))
            ((consp p) (push (down (first p)) words)
                       (push (cons (first p) (second p)) types))
            (t (push (down p) words))))
    (values (format nil "(~a~{ ~a~})" (down (si-name info)) (nreverse words))
            (nreverse types))))

(defun operator-usage (symbol)
  (let ((args (ignore-errors (sb-introspect:function-lambda-list symbol))))
    (if args
        (format nil "(~a~{ ~a~})" (down symbol) (mapcar #'down args))
        (format nil "(~a)" (down symbol)))))

(defun kind-label (kind)
  (ecase kind
    (:source    "sources")
    (:transform "transforms")
    (:sink      "sinks")))

(defun kind-blurb (kind)
  (ecase kind
    (:source    "consume nothing; start a pipeline")
    (:transform "objects in, objects out")
    (:sink      "produce nothing; end a pipeline")))

;;; ------------------------------------------------------------------- listing

(defun list-builtins (&optional (stream *standard-output*))
  (let* ((infos (stage-infos))
         (width (max 12 (reduce #'max infos :key (lambda (i) (length (symbol-name (si-name i))))
                                            :initial-value 0))))
    (format stream "~&~a~2%" (paint "plumb built-ins" :bold))
    (dolist (kind '(:source :transform :sink))
      (let ((group (remove kind infos :key #'stage-kind :test-not #'eq)))
        (when group
          (format stream "~a  ~a~%" (paint (kind-label kind) :bold :cyan)
                  (paint (kind-blurb kind) :grey))
          (dolist (i group)
            (format stream "  ~va  ~a~%" width (down (si-name i))
                    (paint (summary (si-documentation i)) :grey)))
          (terpri stream))))
    ;; Everything that is not a stage, grouped by hand where it helps.
    (let ((seen '()))
      (dolist (group +help-groups+)
        (let ((names (remove-if-not #'builtin-p (group-names group))))
          (when names
            (setf seen (append names seen))
            (format stream "~a  ~a~%    ~{~a~^  ~}~%"
                    (paint (group-title group) :bold :cyan)
                    (paint (group-blurb group) :grey)
                    (mapcar #'down names)))))
      ;; Mostly struct accessors.  They are built-ins and (help NAME) works on
      ;; them, but they are not commands, so they get one wrapped paragraph
      ;; rather than a line each.
      (let ((rest (sort (set-difference (exported-builtins) seen)
                        #'string< :key #'symbol-name)))
        (when rest
          (format stream "~a  ~a~%" (paint "also exported" :bold :cyan)
                  (paint "accessors, predicates, internals" :grey))
          (write-wrapped (mapcar #'down rest) stream))))
    (format stream "~%~a~%" (paint "(help NAME) for detail on any of these." :grey)))
  (values))

(defun write-wrapped (words stream &key (width 76) (indent 4))
  (let ((col 0))
    (dolist (w words)
      (cond ((zerop col) (format stream "~va" indent "") (setf col indent))
            ((> (+ col 2 (length w)) width)
             (format stream "~%~va" indent "") (setf col indent))
            (t (write-string "  " stream) (incf col 2)))
      (write-string w stream)
      (incf col (length w)))
    (terpri stream)))

;;; -------------------------------------------------------------------- detail

(defun describe-builtin (name &optional (stream *standard-output*))
  (let* ((symbol (resolve-builtin name))
         (info (and symbol (gethash symbol *stages*))))
    (cond
      (info (describe-stage info stream))
      ((and symbol (variable-p symbol)) (describe-variable symbol stream))
      ((and symbol (builtin-p symbol)) (describe-operator symbol stream))
      (t (unknown-builtin name stream))))
  (values))

(defun resolve-builtin (name)
  "NAME may be a symbol from any package, a string, or a keyword.  Only symbols
PLUMB itself defines resolve: (help list) is a question about CL, not about a
built-in, and answering it would make the listing and the detail disagree."
  (let ((text (typecase name
                (null nil)
                (symbol (symbol-name name))
                (string (string-upcase name))
                (t (princ-to-string name)))))
    (when text
      (let ((symbol (find-symbol text '#:plumb)))
        (when (and symbol (own-symbol-p symbol)) symbol)))))

(defun describe-stage (info stream)
  (multiple-value-bind (usage types) (stage-usage info)
    (format stream "~&~a  ~a~2%" (paint (down (si-name info)) :bold)
            (paint (format nil "(~a)" (string-right-trim "s" (kind-label (stage-kind info))))
                   :grey))
    (format stream "  ~a~%" (paint usage :cyan))
    (dolist (ty types)
      (format stream "    ~a must be ~a~%" (down (car ty)) (down (cdr ty))))
    (format stream "~%  consumes ~a~%  produces ~a~%  ports    ~{~a~^ ~}~%"
            (render-type (si-consumes info)) (render-type (si-produces info))
            (mapcar #'down1 (si-ports info)))
    (let ((doc (si-documentation info)))
      (if doc
          (format stream "~%~{  ~a~%~}" (split-lines doc))
          (format stream "~%  ~a~%" (paint "(no docstring)" :grey))))))

(defun describe-operator (symbol stream)
  (format stream "~&~a  ~a~2%" (paint (down symbol) :bold)
          (paint (if (macro-function symbol) "(macro)" "(function)") :grey))
  (format stream "  ~a~%" (paint (operator-usage symbol) :cyan))
  (let ((doc (documentation symbol 'function)))
    (if doc
        (format stream "~%~{  ~a~%~}" (split-lines doc))
        (format stream "~%  ~a~%" (paint "(no docstring)" :grey)))))

(defun describe-variable (symbol stream)
  (format stream "~&~a  ~a~2%" (paint (down symbol) :bold)
          (paint (if (constantp symbol) "(constant)" "(variable)") :grey))
  (format stream "  ~a ~a~%" (paint "value" :cyan)
          ;; A value can be arbitrarily large -- *STAGES* is a hash table.
          (let ((*print-length* 8) (*print-level* 3))
            (if (boundp symbol) (down1 (symbol-value symbol)) "unbound")))
  (let ((doc (documentation symbol 'variable)))
    (if doc
        (format stream "~%~{  ~a~%~}" (split-lines doc))
        (format stream "~%  ~a~%" (paint "(no docstring)" :grey)))))

(defun unknown-builtin (name stream)
  (let* ((text (string-upcase (typecase name
                                (symbol (symbol-name name))
                                (t (princ-to-string name)))))
         (near (remove-if-not (lambda (s) (search text (symbol-name s)))
                              (append (mapcar #'si-name (stage-infos))
                                      (exported-builtins)))))
    (format stream "~&No built-in named ~a.~%" (down name))
    (when near
      (format stream "Did you mean: ~{~a~^  ~}~%" (mapcar #'down near)))
    (format stream "~a~%" (paint "(help) lists everything." :grey))))

(defun split-lines (string)
  (loop with start = 0
        for pos = (position #\Newline string :start start)
        collect (string-trim " " (subseq string start pos))
        while pos do (setf start (1+ pos))))

;;; --------------------------------------------------------------- the command

(defmacro help (&optional name)
  "List the built-in commands, or describe one of them.

  (help)        every stage, plus the operators around them
  (help take)   detail for one built-in -- name is not evaluated
  (help \"take\") the same, if you would rather pass a string

Bare HELP works too, so `plumb help` does the right thing from a shell."
  (if name
      `(describe-builtin ',name)
      `(list-builtins)))

(define-symbol-macro help (list-builtins))
