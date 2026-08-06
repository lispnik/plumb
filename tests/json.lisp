
;;;; json.lisp -- tests for the JSON stages.
;;;;
;;;; Same package and the same CHECK/WITH-TIMEOUT as tests.lisp, so a JSON
;;;; failure counts and prints identically.  Separate file and separate system
;;;; for the reason tests/crypto.lisp is: `make test` must keep running with no
;;;; jzon anywhere.

(in-package #:plumb/tests)


(defun test-json-scalars-and-structure ()
  (check (equal '(:a 1) (parse-json "{\"a\":1}")) :object-is-a-plist)
  (check (equal '(1 2 3) (parse-json "[1,2,3]")) :array-is-a-list)
  (check (equal '(:a (:b (1 2 (:c t)))) (parse-json "{\"a\":{\"b\":[1,2,{\"c\":true}]}}"))
         :nesting)
  (check (equal '(t nil nil) (parse-json "[true,false,null]")) :literals)
  (check (null (parse-json "[]")) :empty-array)
  (check (null (parse-json "{}")) :empty-object)
  ;; Whitespace anywhere structural.
  (check (equal '(:a 1 :b 2) (parse-json "  { \"a\" : 1 , \"b\" : 2 }  ")) :whitespace))

(defun test-json-numbers-keep-their-precision ()
  "An integer must not become a double.  A 64-bit id would silently lose its
low bits, and these documents are mostly ids."
  (check (eql 12345678901234567890 (parse-json "12345678901234567890")) :big-integers-exact)
  (check (integerp (parse-json "42")) :plain-integers-are-integers)
  (check (eql -7 (parse-json "-7")) :negative)
  (check (= -1500.0d0 (parse-json "-1.5e3")) :exponent)
  (check (typep (parse-json "1.5") 'double-float) :fractions-are-doubles)
  (check (= 0.5d0 (parse-json "0.5")) :leading-zero))

(defun test-json-strings-and-escapes ()
  (check (string= (format nil "a~cb" #\Tab) (parse-json "\"a\\tb\"")) :tab)
  (check (string= (format nil "a~cb" #\Newline) (parse-json "\"a\\nb\"")) :newline)
  (check (string= "\"" (parse-json "\"\\\"\"")) :quote)
  (check (string= "\\" (parse-json "\"\\\\\"")) :backslash)
  (check (string= "/" (parse-json "\"\\/\"")) :solidus)
  (check (string= "é" (parse-json "\"\\u00e9\"")) :basic-plane)
  ;; \uXXXX is UTF-16.  Anything above the basic plane -- every emoji -- arrives
  ;; as a surrogate PAIR, and a parser that decodes each half separately
  ;; produces two broken characters instead of one.
  (check (string= "😀" (parse-json "\"\\ud83d\\ude00\"")) :surrogate-pair)
  (check (= 1 (length (parse-json "\"\\ud83d\\ude00\""))) :and-it-is-one-character))

(defun test-json-refuses-bad-input ()
  "A shell that silently accepted truncated JSON would give wrong answers
rather than no answer, so every one of these has to signal."
  (dolist (bad '("{\"a\":1,}" "[1,2" "{\"a\"}" "{a:1}" "tru" "\"unterminated"
                 "{\"a\":1} trailing" "\"\\q\"" "\"\\ud83d\"" "" "  "))
    (check (nth-value 1 (ignore-errors (parse-json bad))) (list :rejects bad)))
  ;; And the error says where.
  (let ((c (nth-value 1 (ignore-errors (parse-json "{\"a\":1,}")))))
    (check (typep c 'json-error) :signals-json-error)
    ;; jzon reports line and column in its message; that is kept verbatim
    ;; rather than re-derived, so the report has to carry it through.
    (check (search "line" (string-downcase (princ-to-string c)))
           :the-report-says-where)))

(defun test-json-keys-reach-field ()
  "Keys are upcased into keywords so .name works, since FIELD compares field
names case-insensitively everywhere else in the system."
  (let ((o (parse-json "{\"name\":\"a\",\"Size\":2,\"NESTED\":{\"x\":1}}")))
    (check (string= "a" (field o :name)) :lowercase-key)
    (check (eql 2 (field o :size)) :mixed-case-key)
    (check (equal '(:x 1) (field o :nested)) :uppercase-key)
    (check (null (field o :missing)) :absent-key-is-nil)
    ;; And FIELDS lists them, which is what TABLE needs for its columns.
    (check (equal '(:name :size :nested) (fields o)) :fields-in-document-order)))

(defun test-from-json-stage ()
  (with-timeout (30 :from-json)
    ;; A top-level array is spread: one object per element, which is what every
    ;; --json flag produces and what the rest of a pipeline wants.
    (let ((out (collect-pipeline
                (list (from-list (list "[{\"n\":1},{\"n\":2},{\"n\":3}]")) (from-json)))))
      (check (= 3 (length out)) :array-is-spread)
      (check (equal '(1 2 3) (mapcar (lambda (o) (field o :n)) out)) :in-order))
    ;; A top-level object is one object, not spread into its keys.
    (let ((out (collect-pipeline
                (list (from-list (list "{\"a\":1,\"b\":2}")) (from-json)))))
      (check (= 1 (length out)) :object-is-one-object)
      (check (eql 1 (field (first out) :a)) :with-its-fields))
    ;; Input split across several LINEs is one document.
    (let ((out (collect-pipeline
                (list (from-list (list "{\"a\":" "1}")) (from-json)))))
      (check (equal '(:a 1) (first out)) :input-is-joined-before-parsing))
    ;; JSON Lines: one document per line.
    (let ((out (collect-pipeline
                (list (from-list (list "{\"a\":1}" "{\"a\":2}")) (from-json :lines t)))))
      (check (= 2 (length out)) :lines-mode-parses-each-line)
      (check (equal '(1 2) (mapcar (lambda (o) (field o :a)) out)) :lines-in-order))
    ;; Nothing in, nothing out -- not an error.
    (check (null (collect-pipeline (list (from-list (list "")) (from-json))))
           :empty-input-emits-nothing)))


(defun test-to-json ()
  "The other half.  Driven by FIELDS and FIELD, so every plumb object
serialises without knowing anything about JSON."
  (with-timeout (30 :to-json)
    ;; One array for the whole stream, which is what a --json reader expects.
    (let ((out (first (collect-pipeline
                       (list (from-list (list (list :a 1) (list :a 2))) (to-json))))))
      (check (stringp out) :emits-text)
      (check (string= "[{\"a\":1},{\"a\":2}]" out) :one-array-for-the-stream))
    ;; :LINES is one document per object, and streams rather than collecting.
    (let ((out (collect-pipeline
                (list (from-list (list (list :a 1) (list :a 2))) (to-json :lines t)))))
      (check (equal '("{\"a\":1}" "{\"a\":2}") out) :lines-mode))
    ;; A struct nobody taught about JSON: keys lowercased, keyword values
    ;; stringified, pathname to its namestring, NIL to null.
    (let* ((entry (make-file-entry :name "x" :size 3 :type :file :path #p"/tmp/x"))
           (back (parse-json (to-json-string entry))))
      (check (string= "x" (field back :name)) :struct-field)
      (check (eql 3 (field back :size)) :numeric-field)
      (check (string= "file" (field back :type)) :keyword-becomes-a-string)
      (check (string= "/tmp/x" (field back :path)) :pathname-becomes-its-namestring)
      (check (null (field back :dir-p)) :nil-becomes-null))
    ;; Round trip through both halves.
    (let ((back (parse-json (to-json-string (list :a 1 :b "x" :c t :d (list 1 2))))))
      (check (equal '(:a 1 :b "x" :c t :d (1 2)) back) :round-trip))
    ;; Nested plumb objects go down recursively.
    (let ((back (parse-json (to-json-string (list :outer (list :inner 7))))))
      (check (eql 7 (field (field back :outer) :inner)) :nesting))))

(defun test-to-json-composes-as-bytes ()
  "TO-JSON produces :BYTES like TO-TEXT, so it ends in a sink rather than being
one -- which is what lets `| to-file` and `| to-sh` follow it."
  (check (check-pipeline (list (ls) (to-json) (to-file "/tmp/plumb-json-test.json")))
         :to-file-can-follow)
  (check (check-pipeline (list (ls) (to-json) (print-items))) :print-items-can-follow)
  ;; And FROM-JSON consumes objects, not bytes, so the two do not chain --
  ;; correctly: the text it reads arrives as LINEs from SH or FROM-FILE.
  (check (typep (nth-value 1 (ignore-errors
                              (check-pipeline (list (ls) (to-json) (from-json)))))
                'pipeline-type-error)
         :to-json-does-not-feed-from-json))


(defun run-json-tests ()
  (let ((*passed* 0) (*failed* '()))
    (dolist (fn '(test-json-scalars-and-structure
                  test-json-numbers-keep-their-precision
                  test-json-strings-and-escapes
                  test-json-refuses-bad-input
                  test-json-keys-reach-field
                  test-from-json-stage
                  test-to-json
                  test-to-json-composes-as-bytes))
      (format t "~&; ~a~%" fn)
      (funcall fn))
    (format t "~&~%~d passed, ~d failed~%" *passed* (length *failed*))
    (dolist (f (reverse *failed*))
      (format t "  FAIL: ~s~%" f))
    (null *failed*)))
