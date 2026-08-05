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

(defvar *before-output* nil
  "A function called holding *OUTPUT-LOCK* before anything is written to a
shared stream, or NIL.

WATCH is the only user, and needs exactly this: a panel drawn with cursor
motion has to come down before other output scrolls past the place it is going
to repaint.  A hook can be one line here only because WITH-OUTPUT-LOCK is
already the single point every shared write funnels through -- the same
property that made it the right place to serialise them.")

(defmacro with-output-lock (&body body)
  "Hold *OUTPUT-LOCK* around a write.  A CL stream is not thread-safe, and with
fan-out several stages print at once: two PRINT-ITEMS in parallel branches
duplicate and drop each other's lines, differently on every run.  Locking per
line keeps branches interleaved -- which is what a shell does -- but keeps each
line whole.  Recursive, so a stage that presents inside a locked render is fine."
  `(sb-thread:with-recursive-lock ((the sb-thread:mutex *output-lock*))
     (when *before-output* (funcall *before-output*))
     ,@body))

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

(defun render-transposed (headers cells stream)
  "Field names down the left, one column per record growing rightward.

Each printed line is one field, so FORMAT-TABLE-ROW does the work unchanged --
it is handed (name value-from-record-1 value-from-record-2 ...).

Everything is left-aligned on purpose: a column now holds one *record*, so its
values are heterogeneous -- an integer PID beside a string USER -- and
right-aligning some rows and not others inside one column reads as ragged.
NUMERIC-COLUMN-P simply does not apply to this arrangement."
  (let ((label-width (reduce #'max headers :key #'length :initial-value 0))
        (widths (mapcar (lambda (record)
                          (reduce #'max record :key #'length :initial-value 0))
                        cells)))
    (with-output-lock
      (loop for header in headers
            for i from 0
            do ;; Pad before painting: PAINT adds SGR escapes, and ~va would
               ;; count them as visible characters.  Same trap VISIBLE-WIDTH
               ;; exists for in lineedit.lisp.
               (write-string (paint (format nil "~va" label-width header) :bold) stream)
               (write-string "  " stream)
               (write-line (format-table-row (mapcar (lambda (record) (nth i record)) cells)
                                             widths
                                             (make-list (length cells)))
                           stream))
      (force-output stream)))
  (values))

(defun render-table (rows &key columns (stream *standard-output*) transpose
                            (max-width (if transpose nil 40)))
  "Print ROWS as an aligned table.  Needs all of ROWS up front.

With TRANSPOSE, field names become row headings and each record grows rightward
as its own column -- which is how a wide record becomes readable, and why
MAX-WIDTH then defaults to NIL: transposing is usually how you go to read a
long value in full."
  (when (and transpose (null rows))
    (return-from render-table (values)))   ; nothing to lay out
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
    (when transpose
      (return-from render-table (render-transposed headers cells stream)))
    ;; The whole table, not each row: another branch's output must not land in
    ;; the middle of it.
    (with-output-lock
      (write-line (paint (format-table-row headers widths right) :bold) stream)
      (dolist (row cells)
        (write-line (format-table-row row widths right) stream))
      (force-output stream)))
  (values))
