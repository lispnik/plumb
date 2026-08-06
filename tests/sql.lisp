;;;; sql.lisp -- tests for the SQL stages.
;;;;
;;;; Same package and the same CHECK/WITH-TIMEOUT as tests.lisp.  Separate file
;;;; and separate system for the reason tests/csv.lisp is: `make test` must keep
;;;; running with no cl-dbi anywhere.
;;;;
;;;; Every test builds its own SQLite file, so nothing depends on an ambient
;;;; database and a failure cannot be someone else's data.

(in-package #:plumb/tests)

(defmacro with-sql-fixture ((path) &body body)
  `(let ((,path "/tmp/plumb-sql-test.db"))
     (unwind-protect (progn (ignore-errors (delete-file ,path)) ,@body)
       (ignore-errors (delete-file ,path)))))

(defun sql-rows (db query &rest params)
  (collect-pipeline (list (from-sql query :database db :params params))))

(defun test-sql-round-trip ()
  "TO-SQL then FROM-SQL, which is the whole feature in one line."
  (with-timeout (60 :sql-round-trip)
    (with-sql-fixture (db)
      (join (run (list (from-list (list (list :name "alpha" :size 10)
                                        (list :name "beta" :size 25)))
                       (to-sql "t" :database db :create t))))
      (let ((rows (sql-rows db "select * from t order by size")))
        (check (= 2 (length rows)) :both-rows-came-back)
        (check (string= "alpha" (field (first rows) :name)) :first-row)
        (check (eql 25 (field (second rows) :size)) :second-row)))))

(defun test-sql-keys-reach-field ()
  "cl-dbi hands back case-PRESERVING keys -- (:|Name| ...) -- and FIELD matches
keywords with EQ against an upcased name.  Without re-interning, `.name` misses
every column while looking like it works."
  (with-timeout (60 :sql-keys)
    (with-sql-fixture (db)
      (join (run (list (from-list (list (list :n 1)))
                       (to-sql "t" :database db :create t))))
      ;; A quoted mixed-case column, which is what the raw driver keys look like.
      (let ((rows (sql-rows db "select 1 as \"Name\", 2 as \"UPPER\", 3 as lower")))
        (check (eql 1 (field (first rows) :name)) :mixed-case-column)
        (check (eql 2 (field (first rows) :upper)) :upper-case-column)
        (check (eql 3 (field (first rows) :lower)) :lower-case-column)
        ;; And FIELDS lists them, which is what TABLE needs.
        (check (equal '(:name :upper :lower) (fields (first rows)))
               :fields-in-column-order)))))

(defun test-sql-types-survive ()
  (with-timeout (60 :sql-types)
    (with-sql-fixture (db)
      (join (run (list (from-list (list (list :i 42 :r 1.5d0 :s "text" :n nil)))
                       (to-sql "t" :database db :create t))))
      (let ((row (first (sql-rows db "select * from t"))))
        (check (eql 42 (field row :i)) :integer-stays-an-integer)
        (check (typep (field row :r) 'double-float) :real-stays-a-double)
        (check (string= "text" (field row :s)) :text-stays-text)
        (check (null (field row :n)) :null-is-nil))
      ;; :CREATE infers the column type from what the column holds.
      (let ((schema (first (sql-rows db "select sql from sqlite_master where name='t'"))))
        (check (search "\"i\" INTEGER" (field schema :sql)) :integer-column)
        (check (search "\"r\" REAL" (field schema :sql)) :real-column)
        (check (search "\"s\" TEXT" (field schema :sql)) :text-column)))))

(defun test-from-sql-streams ()
  "The property that separates this from FROM-JSON and FROM-CSV: cl-dbi fetches
a row at a time, so a downstream TAKE abandons the query instead of reading the
table first.  Checked by count rather than by clock -- a timing assertion would
be flaky on a loaded machine."
  (with-timeout (60 :sql-streams)
    (with-sql-fixture (db)
      (join (run (list (from-list (loop for i from 1 to 500 collect (list :i i)))
                       (to-sql "big" :database db :create t))))
      (check (= 500 (first (collect-pipeline
                            (list (from-sql "select * from big" :database db)
                                  (tally)))))
             :all-rows-are-there)
      ;; TAKE closes the channel; FROM-SQL must unwind and disconnect, not hang.
      (let ((some (collect-pipeline (list (from-sql "select * from big" :database db)
                                          (take 3)))))
        (check (= 3 (length some)) :take-bounds-the-query))
      ;; And the connection really was released: a writer would block otherwise.
      (join (run (list (from-list (list (list :i 9999)))
                       (to-sql "big" :database db))))
      (check (= 501 (first (collect-pipeline
                            (list (from-sql "select * from big" :database db) (tally)))))
             :the-abandoned-query-let-go-of-the-database))))

(defun test-sql-params-bind ()
  "Interpolating a value into the query is how injection happens, and a value
containing a quote breaks the query even with nobody being hostile."
  (with-timeout (60 :sql-params)
    (with-sql-fixture (db)
      (join (run (list (from-list (list (list :v "O'Brien") (list :v "safe")))
                       (to-sql "t" :database db :create t))))
      (let ((rows (sql-rows db "select * from t where v = ?" "O'Brien")))
        (check (= 1 (length rows)) :a-quote-in-the-value-is-fine)
        (check (string= "O'Brien" (field (first rows) :v)) :and-comes-back-whole))
      ;; A parameter can never become SQL, so this matches nothing rather than
      ;; returning every row.
      (let ((rows (sql-rows db "select * from t where v = ?" "x' or 1=1 --")))
        (check (null rows) :a-parameter-is-never-sql)))))

(defun test-to-sql-refuses-a-missing-table ()
  "Without :CREATE the table must exist -- a mistyped name should not silently
become a second table that accepts the insert."
  (with-timeout (60 :sql-no-create)
    (with-sql-fixture (db)
      ;; Make the file exist, so the failure is about the TABLE.
      (join (run (list (from-list (list (list :a 1))) (to-sql "real" :database db :create t))))
      (let ((condition (nth-value 1 (ignore-errors
                                     (join (run (list (from-list (list (list :a 1)))
                                                      (to-sql "nosuch" :database db)))
                                           :errorp t)))))
        (check condition :inserting-into-a-missing-table-fails)
        (check (search "nosuch" (princ-to-string condition)) :and-the-message-names-it)))))

(defun test-to-sql-ragged-rows ()
  "Columns are the union of FIELDS across rows, like TABLE and TO-CSV, so a row
missing one gets NULL rather than a shifted insert."
  (with-timeout (60 :sql-ragged)
    (with-sql-fixture (db)
      (join (run (list (from-list (list (list :a 1) (list :b 2)))
                       (to-sql "t" :database db :create t))))
      (let ((rows (sql-rows db "select * from t")))
        (check (= 2 (length rows)) :both-rows)
        (check (and (eql 1 (field (first rows) :a)) (null (field (first rows) :b)))
               :first-row-has-a-and-null-b)
        (check (and (null (field (second rows) :a)) (eql 2 (field (second rows) :b)))
               :second-row-has-null-a-and-b)))))

(defun test-to-sql-serialises-any-object ()
  "Driven by FIELDS and FIELD, so a struct nobody taught about SQL works --
the same reason TABLE and TO-CSV do."
  (with-timeout (60 :sql-objects)
    (with-sql-fixture (db)
      (join (run (list (ls "src/ansi.lisp") (to-sql "files" :database db :create t))))
      (let ((row (first (sql-rows db "select * from files"))))
        (check (string= "ansi.lisp" (field row :name)) :a-file-entry-inserts)
        (check (integerp (field row :size)) :numbers-stay-numbers)
        ;; A keyword became a string and a pathname its namestring.
        (check (string= "file" (field row :type)) :keyword-becomes-a-string)
        (check (search "ansi.lisp" (field row :path)) :pathname-becomes-a-namestring)))))

(defun run-sql-tests ()
  (let ((*passed* 0) (*failed* '()))
    (dolist (fn '(test-sql-round-trip
                  test-sql-keys-reach-field
                  test-sql-types-survive
                  test-from-sql-streams
                  test-sql-params-bind
                  test-to-sql-refuses-a-missing-table
                  test-to-sql-ragged-rows
                  test-to-sql-serialises-any-object))
      (format t "~&; ~a~%" fn)
      (funcall fn))
    (format t "~&~%~d passed, ~d failed~%" *passed* (length *failed*))
    (dolist (f (reverse *failed*))
      (format t "  FAIL: ~s~%" f))
    (null *failed*)))
