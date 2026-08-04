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
