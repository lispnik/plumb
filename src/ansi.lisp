;;;; ansi.lisp -- terminal colour, shared by the line editor's prompt and by
;;;; HELP.  Kept in the core rather than in PLUMB.LINEEDIT because it is not
;;;; about line editing: anything that writes for a human wants these.

(in-package #:plumb)

(defconstant +esc+ (code-char 27)
  "The ESC character, which every sequence in here starts with.")

(defvar *color* :auto
  "T, NIL, or :AUTO -- which honours NO_COLOR, TERM=dumb, and whether standard
output is a terminal.")

(defparameter +sgr+
  '((:reset . 0) (:bold . 1) (:dim . 2) (:italic . 3) (:underline . 4)
    (:red . 31) (:green . 32) (:yellow . 33) (:blue . 34)
    (:magenta . 35) (:cyan . 36) (:grey . 90)))

(defun color-p ()
  (case *color*
    ((t) t)
    ((nil) nil)
    (t (let ((no-color (sb-ext:posix-getenv "NO_COLOR"))
             (term (sb-ext:posix-getenv "TERM")))
         (and (or (null no-color) (string= no-color ""))
              term (not (string= term "dumb"))
              (interactive-stream-p *standard-output*))))))

(defun paint (string &rest styles)
  "STRING wrapped in SGR codes, or returned unchanged when colour is off."
  (if (or (null styles) (not (color-p)))
      string
      (format nil "~c[~{~d~^;~}m~a~c[0m" +esc+
              (mapcar (lambda (s) (or (cdr (assoc s +sgr+)) 0)) styles)
              string +esc+)))

(defun visible-width (string)
  "Columns STRING occupies once drawn: SGR escapes take none.  Every remaining
character is counted as one column, so CJK and combining marks will mislead it."
  (let ((n 0) (i 0) (len (length string)))
    (loop while (< i len)
          do (let ((c (char string i)))
               (cond ((char= c +esc+)
                      (incf i)
                      (when (and (< i len) (char= (char string i) #\[))
                        (incf i)
                        ;; parameters and intermediates, then one final byte
                        (loop while (and (< i len)
                                         (not (char<= #\@ (char string i) #\~)))
                              do (incf i))
                        (incf i)))
                     (t (incf n) (incf i)))))
    n))

;;; Terminal width.
;;;
;;; The obvious trick -- drive the cursor far right and ask where it landed
;;; with ESC[6n -- is wrong here.  The reply arrives on standard input, mixed
;;; in with whatever the user has already typed, so parsing it eats type-ahead;
;;; and a terminal that never answers eats a keystroke per line instead.  STTY
;;; reads the kernel's window size directly and cannot touch the input queue.
;;; The answer is cached and invalidated by SIGWINCH, so this is one subprocess
;;; per resize rather than one per line.

(defvar *columns* nil "Cached terminal width; NIL means ask again.")
(defvar *watching-resize* nil)

(defun query-columns ()
  (ignore-errors
   (let* ((out (with-output-to-string (s)
                 (sb-ext:run-program "/bin/stty" '("size")
                                     :input t :output s :search nil)))
          (space (position #\Space out)))
     (when space
       (let ((cols (parse-integer out :start (1+ space) :junk-allowed t)))
         ;; A pty with no window size reports 0; so does a very odd terminal.
         (when (and cols (>= cols 20)) cols))))))

(defun watch-for-resize ()
  (unless *watching-resize*
    (setf *watching-resize* t)
    (ignore-errors
     (sb-sys:enable-interrupt sb-posix:sigwinch
                              (lambda (&rest args)
                                (declare (ignore args))
                                (setf *columns* nil))))))

(defun terminal-width (&optional (default 80))
  (watch-for-resize)
  (or *columns* (setf *columns* (query-columns)) default))
