;;;; cli.lisp -- the `plumb` executable.
;;;;
;;;; Two syntaxes, and a leading paren decides which:
;;;;
;;;;   ls src/ | where {(> .size 1024)} | take 5      word mode, src/reader.lisp
;;;;   (list (ls "src/") (take 5))                    Lisp
;;;;
;;;; Word mode is a source-to-source pass, so both arrive at the same place.
;;;; One rule then makes this a pipeline shell rather than a Lisp evaluator
;;;; with extra steps:
;;;;
;;;;   a value that is a STAGE, or a list of STAGEs, is RUN and its output
;;;;   printed; any other value is just printed.
;;;;
;;;; The calling thread is the pipeline's consumer (EACH does this), so ^C and
;;;; a downstream `head` reach all the way back to the source through the same
;;;; backpressure the library already has.

(in-package #:plumb.cli)

(defvar *version* "unknown"
  "Set from the ASDF system version by build.lisp, just before the image is
dumped.  The system definition stays the one source of truth.")

(defvar *eof* (list :eof) "Unique marker; NIL and :EOF are both legal forms.")
(defvar *edit* t "NIL disables line editing (--no-edit), for odd terminals.")

;;; ------------------------------------------------------------------ errors

(define-condition usage-error (error)
  ((text :initarg :text :reader usage-error-text))
  (:report (lambda (c s) (write-string (usage-error-text c) s))))

(defun usage-error (format-control &rest args)
  (error 'usage-error :text (apply #'format nil format-control args)))

(defun report (condition)
  ;; PLE:PAINT decides from *STANDARD-OUTPUT*; this goes to stderr, so ask
  ;; about that stream instead.  Piped stderr still gets clean text.
  (let ((*standard-output* *error-output*))
    (format *error-output* "~&~a ~a~%" (ple:paint "plumb:" :red :bold) condition))
  (force-output *error-output*))

(defun guarded (thunk)
  "Run THUNK, reporting any error on stderr.  Returns T on success, NIL on
failure, so the caller can set an exit status without unwinding."
  (handler-case (progn (funcall thunk) t)
    ;; `plumb ... | head` -- our own reader went away.  This is the SIGPIPE
    ;; that TAKE creates internally, arriving from outside the process instead,
    ;; and it means the same thing: stop, successfully.  Throwing here unwinds
    ;; through EACH, whose UNWIND-PROTECT closes the sink and starts the usual
    ;; teardown cascade.  Must be handled here and not further out: the clauses
    ;; below would otherwise swallow it first.
    (stream-error (c)
      (if (output-stream-p (stream-error-stream c))
          (throw 'output-closed 0)
          (progn (report c) nil)))
    ;; Both of these already report themselves well; a bare ~A is enough.
    (plumb:pipeline-type-error (c) (report c) nil)
    (plumb:pipeline-error (c) (report c) nil)
    (error (c) (report c) nil)))

;;; ------------------------------------------------------- command line

(defstruct (options (:conc-name opt-))
  (jobs '())                            ; (:eval . string) (:file . path) (:stdin)
  (capacity nil)
  (quiet nil)
  (interactive nil)
  (action :run))

(defun parse-capacity (flag string)
  (let ((n (ignore-errors (parse-integer string))))
    (unless (and n (plusp n))
      (usage-error "~a wants a positive integer, got ~s." flag string))
    n))

(defun parse-command-line (argv)
  (let ((o (make-options))
        (pending nil))                  ; the value half of --flag=value
    (labels ((value (flag)
               (or (shiftf pending nil)
                   (pop argv)
                   (usage-error "~a requires an argument." flag)))
             (flag= (flag &rest names)
               (member flag names :test #'string=)))
      (loop while argv
            for arg = (pop argv)
            for flag = arg
            do (let ((split (and (eql 0 (search "--" arg)) (position #\= arg))))
                 (when split
                   (setf flag (subseq arg 0 split)
                         pending (subseq arg (1+ split)))))
               (cond
                 ((flag= flag "-h" "--help")        (setf (opt-action o) :help))
                 ((flag= flag "-V" "--version")     (setf (opt-action o) :version))
                 ((flag= flag "-q" "--quiet")       (setf (opt-quiet o) t))
                 ((flag= flag "-i" "--interactive") (setf (opt-interactive o) t))
                 ((flag= flag "--no-edit")          (setf *edit* nil))
                 ((flag= flag "-e" "--eval")
                  (push (cons :eval (value flag)) (opt-jobs o)))
                 ((flag= flag "-f" "--file")
                  (push (cons :file (value flag)) (opt-jobs o)))
                 ((flag= flag "-c" "--capacity")
                  (setf (opt-capacity o) (parse-capacity flag (value flag))))
                 ((string= flag "-")  (push (cons :stdin nil) (opt-jobs o)))
                 ((string= flag "--") (dolist (a argv) (push (cons :eval a) (opt-jobs o)))
                                      (setf argv '()))
                 ((and (> (length flag) 1) (char= (char flag 0) #\-))
                  (usage-error "Unknown option ~a." flag))
                 (t (push (cons :eval arg) (opt-jobs o))))
               (when pending
                 (usage-error "~a does not take an argument." flag))))
    (setf (opt-jobs o) (nreverse (opt-jobs o)))
    o))

;;; --------------------------------------------------------------- evaluating

(defun run-pipeline (stages)
  "Consume STAGES from this thread, so backpressure reaches the source."
  ;; PLUMB:PRESENT owns the one-object-one-line rule, so this printer and the
  ;; PRINT-ITEMS sink cannot drift apart -- they did once, and `| wc -l` lied.
  (plumb:each stages
              (lambda (object)
                (write-line (plumb:present object))
                (force-output))
              :errorp t))

(defun present (value quiet)
  (cond ((plumb:stage-p value) (run-pipeline (list value)))
        ((and (consp value) (every #'plumb:stage-p value)) (run-pipeline value))
        (quiet nil)
        (t (format t "~s~%" value) (force-output))))

(defun eval-and-present (form quiet)
  "Evaluate FORM and show the result.  A form returning NO values -- (help) is
the one that matters -- prints nothing, rather than a stray NIL."
  (let ((values (multiple-value-list (eval form))))
    (when values (present (first values) quiet))))

(defun slurp (stream)
  (with-output-to-string (out)
    (loop for line = (read-line stream nil nil)
          while line do (write-line line out))))

(defun eval-text (text quiet)
  "Evaluate TEXT as either Lisp forms or word-mode pipelines.  A leading paren
decides, per PLUMB:SHELL-SYNTAX-P -- see CLAUDE.md open work 5."
  (if (plumb:shell-syntax-p text)
      ;; One pipeline per line: word mode has no line continuation.
      (dolist (line (plumb::split-lines text))
        (unless (string= "" (string-trim '(#\Space #\Tab) line))
          (eval-and-present (plumb:read-shell line) quiet)))
      (with-input-from-string (in text)
        (loop for form = (read in nil *eof*)
              until (eq form *eof*)
              do (eval-and-present form quiet)))))

(defun run-job (job quiet)
  (destructuring-bind (kind . datum) job
    (ecase kind
      (:eval  (eval-text datum quiet))
      (:stdin (eval-text (slurp *standard-input*) quiet))
      (:file  (eval-text (with-open-file (in datum) (slurp in)) quiet)))))

;;; ---------------------------------------------------------------- the prompt
;;;
;;; *PROMPT* takes a string or a function of no arguments.  The default shows
;;; the input number, the directory LS would list -- pipelines here are mostly
;;; about files, so it is the one piece of state worth seeing -- and a marker
;;; that turns red when the last form failed.

(defvar *input-number* 1)
(defvar *last-ok* t "Did the previous form evaluate cleanly?")
(defvar *prompt-directory-width* 24
  "Longer than this and the prompt shows only the last two components.")

(defun split-path (string)
  (loop with start = 0
        for pos = (position #\/ string :start start)
        collect (subseq string start pos)
        while pos do (setf start (1+ pos))))

(defun abbreviate-directory (&optional (path *default-pathname-defaults*))
  (let ((s (string-right-trim "/" (namestring path)))
        (home (sb-ext:posix-getenv "HOME")))
    (when (and home (plusp (length home)) (eql 0 (search home s)))
      (setf s (concatenate 'string "~" (subseq s (length home)))))
    (cond ((string= s "") "/")
          ((<= (length s) *prompt-directory-width*) s)
          (t (format nil ".../~{~a~^/~}"
                     (last (remove "" (split-path s) :test #'string=) 2))))))

(defun plumb-prompt ()
  (format nil "~a ~a ~a "
          (ple:paint (princ-to-string *input-number*) :grey)
          (ple:paint (abbreviate-directory) :blue :bold)
          (ple:paint ">" (if *last-ok* :green :red))))

(defun continuation-prompt ()
  (ple:paint "... " :grey))

(setf ple:*prompt* 'plumb-prompt
      ple:*continuation-prompt* 'continuation-prompt)

;;; ------------------------------------------------------------------ the REPL

(defun try-read (text)
  "Read every form in TEXT.  Returns (VALUES FORMS-OR-CONDITION STATUS), where
STATUS is :OK, :INCOMPLETE (still unbalanced -- collect another line) or :ERROR.
READ-FROM-STRING draws exactly that line for us: hitting EOF inside an object
signals, hitting it between objects does not.  A word-mode line is complete at
the newline, but an unclosed { or ( inside one still asks for another line."
  (when (plumb:shell-syntax-p text)
    (return-from try-read
      (handler-case (values (list (plumb:read-shell text)) :ok)
        (end-of-file () (values nil :incomplete))
        (error (c) (values c :error)))))
  (handler-case
      (let ((forms '()) (pos 0))
        (loop
          (multiple-value-bind (form next) (read-from-string text nil *eof* :start pos)
            (when (eq form *eof*) (return))
            (push form forms)
            (setf pos next)))
        (values (nreverse forms) :ok))
    (end-of-file () (values nil :incomplete))
    (error (c) (values c :error))))

(defun eval-forms-reporting (forms quiet)
  (let ((ok t))
    (dolist (form forms ok)
      (unless (guarded (lambda () (eval-and-present form quiet)))
        (setf ok nil)))))

(defun repl-loop (read-a-line quiet)
  "The REPL, once.  READ-A-LINE takes a prompt designator and returns
(VALUES LINE STATUS) with STATUS :LINE, :EOF or :INTERRUPT -- the only thing
the edited and plain paths differ by.  Factored so word mode, history and the
continuation prompt cannot work on one path and not the other; they already
diverged once, when the plain path read Lisp straight off the stream."
  (let ((pending ""))
    (loop
      (multiple-value-bind (line status)
          (funcall read-a-line
                   (if (string= pending "") ple:*prompt* ple:*continuation-prompt*))
        (ecase status
          ;; ^D ends the session on an empty line; mid-form it only abandons.
          (:eof (if (string= pending "") (return) (setf pending "")))
          (:interrupt (setf pending ""))
          (:line
           (setf pending (if (string= pending "")
                             line
                             (format nil "~a~%~a" pending line)))
           (multiple-value-bind (result state) (try-read pending)
             (ecase state
               (:incomplete)            ; keep collecting lines
               (:error
                (ple:add-history pending)
                (report result)
                (setf *last-ok* nil pending ""))
               (:ok
                (ple:add-history pending)
                (setf pending "")
                (when result
                  (incf *input-number*)
                  (setf *last-ok* (eval-forms-reporting result quiet))))))))))))

(defun edited-repl (quiet)
  (repl-loop (lambda (prompt) (ple:read-line-edited :prompt prompt)) quiet))

(defun plain-repl (quiet)
  "No terminal, or --no-edit.  Same prompt and the same reader as the edited
path -- customising *PROMPT* should not stop working just because the terminal
is not one, and neither should word mode."
  (repl-loop (lambda (prompt)
               (format t "~&~a" (ple:prompt-text prompt))
               (force-output)
               (let ((line (read-line *standard-input* nil nil)))
                 (if line (values line :line) (values "" :eof))))
             quiet))

(defun repl (&optional quiet)
  "Also the entry point for `make repl`, so it binds *PACKAGE* itself rather
than relying on RUN-JOBS having done it."
  (let ((*package* (find-package '#:plumb)))
    (if (and *edit* (ple:tty-p))
        (edited-repl quiet)
        (plain-repl quiet))))

(defun run-jobs (o)
  (let ((plumb:*default-capacity* (or (opt-capacity o) plumb:*default-capacity*))
        (*package* (find-package '#:plumb))
        (jobs (opt-jobs o))
        (status 0))
    ;; Nothing to evaluate: read a pipe, or talk to a terminal.
    (when (and (null jobs) (not (opt-interactive o)))
      (if (interactive-stream-p *standard-input*)
          (setf (opt-interactive o) t)
          (setf jobs (list (cons :stdin nil)))))
    (dolist (job jobs)
      (unless (guarded (lambda () (run-job job (opt-quiet o))))
        (setf status 1)))
    (when (opt-interactive o)
      (repl (opt-quiet o)))
    status))

;;; --------------------------------------------------------------------- main

(defun print-usage (stream)
  (format stream "~
plumb ~a -- thread-and-channel pipelines carrying Lisp objects

usage: plumb [options] [form ...]

Input starting with ( is Lisp; anything else is word mode:

  plumb 'ls src/ | where {(> .size 1024)} | sort-by .size :desc | take 5'
  plumb '(list (ls \"src/\") (take 5))'

In word mode | separates stages, a bare word is a string, {...} is a block over
the current object, .name reads a field, *x* is a variable and a trailing
keyword means T.  A value that is a stage, or a list of stages, is run and
every object the last stage emits is printed; any other value is printed as-is.

options:
  -e, --eval FORM     evaluate FORM (repeatable; same as a bare argument)
  -f, --file PATH     evaluate every form in PATH (repeatable)
  -                   evaluate every form on standard input
  -c, --capacity N    channel depth, i.e. the backpressure window (default ~d)
  -q, --quiet         print pipeline output only, not other values
  -i, --interactive   read-eval-print loop once the forms run out
  --no-edit           plain input, no raw-mode line editing
  -h, --help          this text
  -V, --version       version
  --                  treat every remaining argument as a form

Bare arguments are forms, never filenames -- use -f for a file.

This text covers the command line.  For the built-in commands themselves:
  plumb help              list every stage and operator
  plumb '(help take)'     detail for one of them

Single-quote the form: $ is the block macro and the shell would eat it.  For
the same reason write (function f), not #'f -- the apostrophe ends the quoting.

examples:
  plumb 'ls src/ | where {(> .size 1024)} | take 5'
  plumb 'sh \"df -h\" | drop 1 | table'
  plumb 'help take'
  plumb -e '(list (counter) (where (function oddp)) (take 6))'
  plumb -f pipeline.lisp
  echo 'counter :limit 3' | plumb -
  printf 'a\\nb\\n' | plumb '(list (lines *standard-input*) (xform ($ (fld :text))))'

exit status: 0 ok, 1 evaluation or pipeline error, 2 usage error, 130 interrupt
"
          *version* plumb:*default-capacity*))

(defun %main (argv)
  (let ((o (parse-command-line argv)))
    (ecase (opt-action o)
      (:help    (print-usage *standard-output*) 0)
      (:version (format t "plumb ~a~%" *version*) 0)
      (:run     (run-jobs o)))))

(defun main ()
  "Entry point for the dumped image.  Exits; never returns."
  (let ((code (catch 'output-closed
                (handler-case (%main (rest sb-ext:*posix-argv*))
                  (usage-error (c)
                    (report c)
                    (format *error-output* "Try plumb --help.~%")
                    2)
                  (sb-sys:interactive-interrupt ()
                    (terpri *error-output*)
                    130)
                  ;; Backstop for a broken stdout outside GUARDED -- the REPL
                  ;; prompt, or --help.
                  (stream-error () 0)))))
    (ignore-errors (finish-output *standard-output*))
    (ignore-errors (finish-output *error-output*))
    (sb-ext:exit :code code :abort t)))
