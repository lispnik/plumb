;;;; present.lisp -- how an object becomes text for a human.
;;;;
;;;; Until now every printing path did its own PRINC-TO-STRING with
;;;; *PRINT-PRETTY* bound off, and each one had to rediscover that a stage
;;;; thread does not inherit the caller's binding.  PRESENT is the single
;;;; place that rule lives, and the single place a type says how it should
;;;; look: a LINE is its text, a FILE-ENTRY is its name.
;;;;
;;;; One object is one line, always.  Everything downstream -- wc -l, head,
;;;; sort, the CLI's own printer -- depends on it.
;;;;
;;;; RENDER-TABLE is the other half: a table needs every row before it can size
;;;; a column, so it is a barrier, and the TABLE stage in stages.lisp is where
;;;; that barrier is declared.

(in-package #:plumb)

(defvar *output-lock* (sb-thread:make-mutex :name "plumb-output")
  "Serialises writes to a shared stream.")

(defmacro with-output-lock (&body body)
  "Hold *OUTPUT-LOCK* around a write.  A CL stream is not thread-safe, and with
fan-out several stages print at once: two PRINT-ITEMS in parallel branches
duplicate and drop each other's lines, differently on every run.  Locking per
line keeps branches interleaved -- which is what a shell does -- but keeps each
line whole.  Recursive, so a stage that presents inside a locked render is fine."
  `(sb-thread:with-recursive-lock ((the sb-thread:mutex *output-lock*)) ,@body))

(defgeneric present (object)
  (:documentation "OBJECT as a single line of text, for a human to read."))

(defmethod present (object)
  ;; A stage runs in its own thread and does not inherit the caller's binding,
  ;; so this cannot be left to whoever is printing.
  (let ((*print-pretty* nil))
    (princ-to-string object)))

(defmethod present ((object string)) object)

(defmethod present ((object null)) "nil")

;;; ------------------------------------------------------------------ tables

(defun table-columns (rows)
  "Field names across ROWS in first-seen order, or (:VALUE) for objects that
have no fields at all -- integers, say."
  (let ((seen '()))
    (dolist (row rows)
      (dolist (key (fields row))
        (pushnew key seen :test #'equal)))
    (or (nreverse seen) '(:value))))

(defun table-value (row column)
  (if (eq column :value) row (field row column)))

(defun table-cell (row column max-width)
  "NIL renders empty rather than \"nil\": in a table an absent value should
look absent."
  (let* ((value (table-value row column))
         (text (if (null value) "" (present value))))
    (if (and max-width (> (length text) max-width))
        (concatenate 'string (subseq text 0 (max 1 (1- max-width))) "…")
        text)))

(defun numeric-column-p (rows column)
  "Right-align a column only when every value in it is a number."
  (and rows
       (every (lambda (r) (let ((v (table-value r column))) (or (null v) (realp v)))) rows)
       (some (lambda (r) (realp (table-value r column))) rows)))

(defun format-table-row (cells widths right-align)
  (string-right-trim " "
                     (with-output-to-string (s)
                       (loop for cell in cells
                             for width in widths
                             for right in right-align
                             for first = t then nil
                             do (unless first (write-string "  " s))
                                (if right
                                    (format s "~v@a" width cell)
                                    (format s "~va" width cell))))))

(defun render-table (rows &key columns (stream *standard-output*) (max-width 40))
  "Print ROWS as an aligned table.  Needs all of ROWS up front."
  (let* ((columns (or columns (table-columns rows)))
         (headers (mapcar (lambda (c) (string-downcase (string c))) columns))
         (cells (mapcar (lambda (row)
                          (mapcar (lambda (c) (table-cell row c max-width)) columns))
                        rows))
         (widths (loop for header in headers
                       for i from 0
                       collect (reduce #'max cells
                                       :key (lambda (row) (length (nth i row)))
                                       :initial-value (length header))))
         (right (mapcar (lambda (c) (numeric-column-p rows c)) columns)))
    ;; The whole table, not each row: another branch's output must not land in
    ;; the middle of it.
    (with-output-lock
      (write-line (paint (format-table-row headers widths right) :bold) stream)
      (dolist (row cells)
        (write-line (format-table-row row widths right) stream))
      (force-output stream)))
  (values))
