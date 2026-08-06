;;;; csv.lisp -- CSV in and out, on cl-csv.
;;;;
;;;; The other half of the interchange story next to src/json.lisp: JSON is what
;;;; tools speak, CSV is what spreadsheets and data exports speak.  Same shape as
;;;; that file -- FROM-CSV yields plists so `.name` works, TO-CSV serialises any
;;;; plumb object through FIELDS and FIELD.
;;;;
;;;; Loaded by the PLUMB/CSV system, not the core, and defines into the PLUMB
;;;; package exporting at load time, exactly as src/json.lisp and src/crypto.lisp
;;;; do.  The binary builds with every optional system, so these are always
;;;; there in practice.
;;;;
;;;; cl-csv does the lexing, and that is worth having for one reason: CSV looks
;;;; like split-on-comma and is not.  A field may contain the separator, a
;;;; doubled quote, or a NEWLINE, so a line-at-a-time reader is wrong on real
;;;; exports.  All three are in the test suite.
;;;;
;;;; What is NOT done here is type inference.  Every CSV value is a string, and
;;;; guessing otherwise is the spreadsheet data-corruption bug: a zip code
;;;; 01234 becomes 1234, a version "1.10" becomes 1.1, an id 00042 loses its
;;;; padding.  :NUMBERS asks for conversion explicitly, and even then only when
;;;; the whole field parses and the text round-trips.

(in-package #:plumb)

(defun csv-key (name)
  "A header cell as the keyword FIELD will match -- upcased, as FROM-JSON does,
because FIELD compares field names case-insensitively everywhere else."
  (intern (string-upcase (string-trim " " name)) :keyword))

(defun csv-number (text)
  "TEXT as a number, but only when nothing is lost by saying so.

The round-trip check is the whole point: `01234` reads as 1234, whose printed
form is not `01234`, so it stays a string.  So do `1.10`, `+7` and `1e5` --
anything whose text a reader would not reproduce.  This is the bug every
spreadsheet has, and it silently destroys zip codes, versions and padded ids."
  (let ((trimmed (string-trim " " text)))
    (if (string= trimmed "")
        text
        (let ((value (ignore-errors
                      (let ((*read-eval* nil) (*read-default-float-format* 'double-float))
                        (multiple-value-bind (v end) (read-from-string trimmed)
                          (and (realp v) (eql end (length trimmed)) v))))))
          (if (and value (string= trimmed (princ-to-string value)))
              value
              text)))))

(defun csv-rows (text separator)
  (cl-csv:read-csv text :separator separator))

;;; ------------------------------------------------------------------ writing

(defun csv-cell (value)
  "One field as text.  NIL is an EMPTY cell rather than the string \"nil\":
in a table an absent value should look absent, which is what TABLE-CELL does
for the same reason."
  (if (null value) "" (present value)))

(defun csv-line (values separator)
  ;; WRITE-CSV-ROW ends the row itself; the trailing newline comes off because
  ;; each row is emitted as its own object and TO-FILE writes one line each.
  (string-right-trim '(#\Newline #\Return)
                     (cl-csv:write-csv-row values :separator separator)))

;;; ------------------------------------------------------------------- stages

(defstage from-csv (&key (headers t) (separator #\,) (numbers nil))
  "Parse CSV arriving as text and emit a row per record.

  from-file \"export.csv\" | from-csv | where {(string= .status \"open\")} | table
  sh \"curl -s https://example.com/data.csv\" | from-csv :numbers | sort-by .amount
  from-file \"raw.tsv\" | from-csv :separator #\\Tab

With HEADERS, the default, the first row names the columns and every later row
becomes a plist with upcased keyword keys -- so `.name` works and TABLE finds
its columns, exactly as FROM-JSON does.  Without it each row is a plain list of
strings.

Every value is a STRING unless :NUMBERS is given.  That is deliberate: guessing
types is the bug every spreadsheet has, where a zip code 01234 becomes 1234 and
a version 1.10 becomes 1.1.  Even with :NUMBERS a field converts only when its
text round-trips, so padded and trailing-zero values stay strings.

A barrier: a CSV field may contain a newline, so the document cannot be parsed a
line at a time.

A word-mode note: a bare `nil` is the STRING \"nil\", which is true, so
`to-csv :headers nil` does NOT turn the header off -- write `:headers ()`,
which the reader takes as a Lisp form.  Every bare word being a string is what
keeps globs working, so this is the price of that rule rather than a bug here."
  (:consumes :objects) (:produces :objects) (:barrier t)
  (let ((buffer (make-string-output-stream)))
    (do-input (x)
      (write-string (typecase x
                      (line (line-text x))
                      (string x)
                      (t (present x)))
                    buffer)
      (terpri buffer))
    ;; The trailing newline comes off before parsing.  Input arrives either as
    ;; LINEs, which carry none and get one added above, or as whole text that
    ;; already ends in one -- and then the added newline makes a blank final
    ;; line, which cl-csv correctly reports as one more (empty) record.
    (let ((text (string-right-trim '(#\Newline #\Return) (get-output-stream-string buffer))))
      (unless (string= (string-trim '(#\Space #\Tab #\Newline #\Return) text) "")
        (let ((rows (csv-rows text separator)))
          (flet ((convert (cell) (if numbers (csv-number cell) cell)))
            (if headers
                (let ((keys (mapcar #'csv-key (first rows))))
                  (dolist (row (rest rows))
                    (emit (loop for key in keys
                                for cell in row
                                append (list key (convert cell))))))
                (dolist (row rows)
                  (emit (mapcar #'convert row))))))))))

(defstage to-csv (&key (headers t) (separator #\,))
  "Render the stream as CSV -- one object per row.

  ls \"src/*.lisp\" | to-csv > files.csv
  ps | where {(> .rss 500mb)} | to-csv :separator #\\Tab
  disks | to-csv :headers ()

Columns are the union of FIELDS across every row, in first-seen order, which is
the same rule TABLE uses -- and the reason this works on a FILE-ENTRY, a
PROCESS, a COMMIT or a type you add later without any of them knowing about CSV.
A row missing a column gets an empty cell.

A barrier for that reason: the header cannot be written until the last row has
been seen.  Each row is emitted as its own string, so TO-FILE writes one line
each and PRINT-ITEMS shows them.

Produces :BYTES like TO-JSON and TO-TEXT, so it composes with TO-FILE and TO-SH
rather than ending the pipeline itself.

A word-mode note: a bare `nil` is the STRING \"nil\", which is true, so
`to-csv :headers nil` does NOT turn the header off -- write `:headers ()`,
which the reader takes as a Lisp form.  Every bare word being a string is what
keeps globs working, so this is the price of that rule rather than a bug here."
  (:consumes :objects) (:produces :bytes) (:barrier t)
  (let ((rows (make-array 16 :adjustable t :fill-pointer 0)))
    (do-input (x) (vector-push-extend x rows))
    (let* ((rows (coerce rows 'list))
           ;; The same column rule as TABLE, from the same function.
           (columns (table-columns rows)))
      (when rows
        (when headers
          (emit (csv-line (mapcar (lambda (c) (string-downcase (string c))) columns)
                          separator)))
        (dolist (row rows)
          (emit (csv-line (mapcar (lambda (c) (csv-cell (table-value row c))) columns)
                          separator)))))))

;;; Exported here rather than in package.lisp: these symbols only name anything
;;; once this system is loaded, and HELP lists exported symbols that are FBOUND.

(export '(from-csv to-csv csv-number) '#:plumb)
