;;;; process.lisp -- external commands as pipeline stages.  Unix only: a string
;;;; command is handed to /bin/sh.
;;;;
;;;; `lines` already turns any stream into objects, so a child process could
;;;; always be spliced in by hand.  What was missing is everything around the
;;;; data: nothing killed a child whose reader had gone away, and a command
;;;; that failed was indistinguishable from one that printed nothing.
;;;;
;;;; Both live in WITH-COMMAND's UNWIND-PROTECT, which is the only place a
;;;; child can be reaped -- a downstream TAKE closes our output, the SEND
;;;; inside EMIT-LINES signals CHANNEL-CLOSED, and we unwind through there.
;;;;
;;;; A mid-pipeline filter (objects -> stdin, stdout -> objects) is deliberately
;;;; absent.  It has to write and read the child concurrently or the pipe
;;;; buffers deadlock, and it also blocks in RECV, which is a condition
;;;; variable and cannot be selected on alongside file descriptors.  That means
;;;; a helper thread inside the stage, which is exactly what "a stage contains
;;;; no concurrency" forbids.  See CLAUDE.md.

(in-package #:plumb)

(defconstant +sigterm+ 15
  "Core PLUMB does not depend on sb-posix, so the number is spelled out.")

(define-condition command-failed (error)
  ((command   :initarg :command   :reader command-failed-command)
   (exit-code :initarg :exit-code :reader command-failed-exit-code)
   (stderr    :initarg :stderr    :initform nil :reader command-failed-stderr))
  (:report
   (lambda (c s)
     (format s "command ~s exited ~a~@[: ~a~]"
             (command-failed-command c)
             (command-failed-exit-code c)
             (let ((text (command-failed-stderr c)))
               (when (and text (plusp (length text)))
                 (string-trim '(#\Space #\Newline) text)))))))

(defun %command-argv (command)
  "A string runs under /bin/sh -c; a list is exec'd directly, with no shell to
quote against."
  (etypecase command
    (string (values "/bin/sh" (list "-c" command)))
    (cons   (values (first command) (mapcar #'princ-to-string (rest command))))))

(defun %command-label (command)
  (if (stringp command)
      command
      (format nil "~{~a~^ ~}" command)))

(defun %stderr-file ()
  (merge-pathnames (format nil "plumb-stderr-~36r.txt" (random (expt 2 48)))
                   #p"/tmp/"))

(defun %slurp (path)
  (when (and path (probe-file path))
    (with-open-file (in path :if-does-not-exist nil)
      (when in
        (let ((text (make-string (file-length in))))
          (subseq text 0 (read-sequence text in)))))))

(defun finish-command (proc command stderr-file on-exit)
  "Normal path: let the child finish, then decide what its exit code means."
  ;; Closing stdin first, or `wc -l` waits for an EOF that never comes.
  (let ((in (sb-ext:process-input proc)))
    (when in (ignore-errors (close in))))
  (sb-ext:process-wait proc)
  (let ((code (sb-ext:process-exit-code proc))
        (text (%slurp stderr-file)))
    ;; Captured stderr is still the user's to see, whether or not we signal.
    (when (and text (plusp (length text)))
      (write-string text *error-output*)
      (force-output *error-output*))
    (when (and (eq on-exit :signal) (integerp code) (/= code 0))
      (error 'command-failed :command (%command-label command)
                             :exit-code code :stderr text))
    code))

(defun reap-command (proc stderr-file)
  "Cleanup path, run on every exit.  A child still alive here is one whose
reader went away, so it is killed rather than left writing into a dead pipe.
On the normal path FINISH-COMMAND already waited and this just tidies up."
  (when (ignore-errors (sb-ext:process-alive-p proc))
    (ignore-errors (sb-ext:process-kill proc +sigterm+))
    (ignore-errors (sb-ext:process-wait proc)))
  (ignore-errors (sb-ext:process-close proc))
  (when stderr-file (ignore-errors (delete-file stderr-file))))

(defmacro with-command ((var command run-options &key directory stderr on-exit)
                        &body body)
  "Spawn COMMAND, run BODY with VAR bound to the process, then reap it.
RUN-OPTIONS is a list of extra SB-EXT:RUN-PROGRAM arguments -- the two stages
differ only in whether they want a stdout stream or a stdin one."
  (let ((cmd (gensym "CMD")) (file (gensym "FILE"))
        (prog (gensym "PROG")) (args (gensym "ARGS")))
    `(let* ((,cmd ,command)
            (,file (unless (eq ,stderr :inherit) (%stderr-file))))
       (multiple-value-bind (,prog ,args) (%command-argv ,cmd)
         (let ((,var (apply #'sb-ext:run-program ,prog ,args
                            :wait nil :search t :directory ,directory
                            (append ,run-options
                                    (when ,file
                                      (list :error ,file
                                            :if-error-exists :supersede))))))
           (unwind-protect
                (progn ,@body
                       (finish-command ,var ,cmd ,file ,on-exit))
             (reap-command ,var ,file)))))))

;;; ------------------------------------------------------------------- stages

(defstage sh ((command (or string cons)) &key directory (on-exit :signal) (stderr :capture))
  "Run COMMAND and emit its stdout as LINE objects.

A string runs under /bin/sh, so pipes and globs work; a list is exec'd
directly, which is what you want when an argument came from somewhere else.
A non-zero exit signals COMMAND-FAILED unless ON-EXIT is :IGNORE.  STDERR is
:CAPTURE (replayed on our stderr, and attached to the condition) or :INHERIT
(live on the terminal, but then there is nothing to attach)."
  (:consumes nil) (:produces :objects)
  (with-command (proc command '(:output :stream)
                  :directory directory :stderr stderr :on-exit on-exit)
    (emit-lines (sb-ext:process-output proc) (%command-label command))))

(defstage to-sh ((command (or string cons)) &key directory (on-exit :signal) (stderr :capture))
  "Write each object to COMMAND's stdin, one line each.  A sink; the command's
own stdout is inherited, so `(to-sh \"wc -l\")` prints where you would expect."
  (:consumes t) (:produces nil)
  (with-command (proc command '(:input :stream :output t)
                  :directory directory :stderr stderr :on-exit on-exit)
    ;; A stage thread does not inherit the caller's *PRINT-PRETTY*, and one
    ;; object per line is the contract every downstream tool assumes.
    (let ((*print-pretty* nil)
          (in (sb-ext:process-input proc)))
      (do-input (x)
        (write-line (if (stringp x) x (princ-to-string x)) in)))))
