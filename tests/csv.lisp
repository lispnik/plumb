;;;; csv.lisp -- tests for the CSV stages.
;;;;
;;;; Same package and the same CHECK/WITH-TIMEOUT as tests.lisp, so a CSV
;;;; failure counts and prints identically.  Separate file and separate system
;;;; for the reason tests/json.lisp is: `make test` must keep running with no
;;;; cl-csv anywhere.

(in-package #:plumb/tests)

(defparameter +tricky-csv+
  (format nil "name,size,note~%alpha,10,\"has,comma\"~%beta,25,\"has \"\"quotes\"\"\"~%gamma,7,\"has~%newline\"~%")
  "The three things that make CSV not split-on-comma: a field containing the
separator, a field containing doubled quotes, and a field containing a
NEWLINE.  A line-at-a-time reader gets the last one wrong on real exports.")

(defun csv-rows-of (text &rest args)
  (collect-pipeline (list (from-list (split-lines text)) (apply #'from-csv args))))

(defun test-from-csv-handles-what-makes-csv-hard ()
  (with-timeout (20 :from-csv-tricky)
    ;; SPLIT-LINES would cut the embedded newline, so feed the text whole.
    (let ((rows (collect-pipeline (list (from-list (list +tricky-csv+)) (from-csv)))))
      (check (= 3 (length rows)) :three-records-not-four)
      (destructuring-bind (a b c) rows
        (check (string= "alpha" (field a :name)) :header-becomes-a-key)
        (check (string= "has,comma" (field a :note)) :separator-inside-a-quoted-field)
        (check (string= "has \"quotes\"" (field b :note)) :doubled-quotes-unescape)
        (check (find #\Newline (field c :note)) :newline-inside-a-quoted-field)))))

(defun test-from-csv-headers ()
  (with-timeout (20 :from-csv-headers)
    (let ((rows (collect-pipeline (list (from-list (list "a,b" "1,2" "3,4")) (from-csv)))))
      (check (equal '(:a "1" :b "2") (first rows)) :plist-with-keyword-keys)
      (check (equal '(:a :b) (fields (first rows))) :fields-in-column-order))
    ;; Headers off: each row is a plain list, INCLUDING the first.
    (let ((rows (collect-pipeline
                 (list (from-list (list "a,b" "1,2")) (from-csv :headers nil)))))
      (check (equal '(("a" "b") ("1" "2")) rows) :no-headers-gives-lists))
    ;; A header cell is matched the way FIELD matches everywhere else.
    (let ((rows (collect-pipeline (list (from-list (list "Name,SIZE" "x,1")) (from-csv)))))
      (check (string= "x" (field (first rows) :name)) :mixed-case-header)
      (check (string= "1" (field (first rows) :size)) :upper-case-header))))

(defun test-from-csv-does-not-guess-types ()
  "The spreadsheet data-corruption bug, refused by default: a zip code must not
become an integer and a version must not become a float."
  (with-timeout (20 :from-csv-types)
    (let ((row (first (collect-pipeline
                       (list (from-list (list "zip,version,count" "01234,1.10,42"))
                             (from-csv))))))
      (check (string= "01234" (field row :zip)) :everything-is-a-string-by-default)
      (check (string= "1.10" (field row :version)) :including-what-looks-numeric)
      (check (string= "42" (field row :count)) :and-what-really-is))
    ;; With :NUMBERS, only the values whose text round-trips convert.
    (let ((row (first (collect-pipeline
                       (list (from-list (list "zip,version,count" "01234,1.10,42"))
                             (from-csv :numbers t))))))
      (check (string= "01234" (field row :zip)) :a-padded-id-still-stays-a-string)
      (check (string= "1.10" (field row :version)) :so-does-a-trailing-zero)
      (check (eql 42 (field row :count)) :but-a-plain-integer-converts))
    (check (eql 42 (csv-number "42")) :integers)
    (check (eql -7 (csv-number "-7")) :negatives)
    (check (string= "007" (csv-number "007")) :leading-zeros-refused)
    (check (string= "1.10" (csv-number "1.10")) :trailing-zeros-refused)
    (check (string= "+7" (csv-number "+7")) :a-plus-sign-does-not-round-trip)
    (check (string= "" (csv-number "")) :empty-stays-empty)
    (check (string= "abc" (csv-number "abc")) :text-stays-text)))

(defun test-from-csv-separator ()
  (with-timeout (20 :from-csv-separator)
    (let ((rows (collect-pipeline
                 (list (from-list (list "a;b" "1;2")) (from-csv :separator #\;)))))
      (check (equal '(:a "1" :b "2") (first rows)) :semicolons))
    (let ((rows (collect-pipeline
                 (list (from-list (list (format nil "a~cb" #\Tab)
                                        (format nil "1~c2" #\Tab)))
                       (from-csv :separator #\Tab)))))
      (check (equal '(:a "1" :b "2") (first rows)) :tabs))))

(defun test-to-csv ()
  (with-timeout (20 :to-csv)
    (let ((out (collect-pipeline
                (list (from-list (list (list :a 1 :b "x") (list :a 2 :b "y")))
                      (to-csv)))))
      (check (equal '("a,b" "1,x" "2,y") out) :header-then-a-row-each))
    ;; Quoting is the writer's job, and must survive a round trip.
    (let* ((row (list :a "has,comma" :b "has\"quote"))
           (out (collect-pipeline (list (from-list (list row)) (to-csv))))
           (back (first (collect-pipeline
                         (list (from-list out) (from-csv))))))
      (check (equal row back) :round-trip-through-quoting))
    ;; Columns are the union across rows, in first-seen order, like TABLE --
    ;; and a row missing one gets an empty cell rather than a shifted row.
    (let ((out (collect-pipeline
                (list (from-list (list (list :a 1) (list :b 2))) (to-csv)))))
      (check (equal '("a,b" "1," ",2") out) :ragged-rows-line-up))
    ;; Any plumb object, through FIELDS and FIELD -- nothing knows about CSV.
    (let ((out (collect-pipeline
                (list (from-list (list (make-file-entry :name "x" :size 3)))
                      (to-csv)))))
      (check (search "name" (first out)) :a-struct-supplies-its-own-columns)
      (check (search "x" (second out)) :and-its-values))))

(defun test-to-csv-headers-off ()
  "A bare `nil` in word mode is the STRING \"nil\", which is true -- so the
option has to be given as () there.  Worth a test because the failure is
silent: you get a header row you asked not to have."
  (with-timeout (20 :to-csv-headers)
    (let ((out (collect-pipeline
                (list (from-list (list (list :a 1))) (to-csv :headers nil)))))
      (check (equal '("1") out) :no-header-row))
    (check (equal '(to-csv :headers nil) (read-shell "to-csv :headers ()"))
           :parens-are-how-word-mode-says-nil)
    (check (equal '(to-csv :headers "nil") (read-shell "to-csv :headers nil"))
           :a-bare-nil-is-the-string)))

(defun test-csv-composes-as-bytes ()
  (check (check-pipeline (list (ls) (to-csv) (to-file "/tmp/plumb-csv-test.csv")))
         :to-file-can-follow)
  (check (check-pipeline (list (from-file "/tmp/x.csv") (from-csv) (table)))
         :from-csv-feeds-objects))

(defun run-csv-tests ()
  (let ((*passed* 0) (*failed* '()))
    (dolist (fn '(test-from-csv-handles-what-makes-csv-hard
                  test-from-csv-headers
                  test-from-csv-does-not-guess-types
                  test-from-csv-separator
                  test-to-csv
                  test-to-csv-headers-off
                  test-csv-composes-as-bytes))
      (format t "~&; ~a~%" fn)
      (funcall fn))
    (format t "~&~%~d passed, ~d failed~%" *passed* (length *failed*))
    (dolist (f (reverse *failed*))
      (format t "  FAIL: ~s~%" f))
    (null *failed*)))
