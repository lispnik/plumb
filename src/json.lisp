;;;; json.lisp -- JSON in, objects out.
;;;;
;;;; This is the force multiplier rather than another source.  Every modern CLI
;;;; already speaks JSON -- gh, docker, kubectl, aws, ip -j, systemd -- so one
;;;; parser turns all of them into sources at once, instead of a stage per tool:
;;;;
;;;;   sh "gh pr list --json number,title,author" | from-json
;;;;     | where {(> .number 100)} | table
;;;;
;;;; Written out rather than pulled in because the core has no dependencies,
;;;; and a JSON reader is a day's work where a dependency is forever.  It is a
;;;; plain recursive descent over a string.
;;;;
;;;; The mapping is chosen for a SHELL, not for round-tripping:
;;;;
;;;;   object  -> plist with upcased keyword keys, so .name works and FIELDS
;;;;              can list them for TABLE
;;;;   array   -> list
;;;;   string  -> string
;;;;   number  -> integer or double
;;;;   true    -> T
;;;;   false   -> NIL
;;;;   null    -> NIL
;;;;
;;;; FALSE and NULL both becoming NIL loses a distinction, and that is
;;;; deliberate: `where {.draft}` should work, and a missing key already reads
;;;; as NIL through FIELD.  Anyone needing to tell them apart is doing
;;;; something a shell is the wrong tool for.

(in-package #:plumb)

(define-condition json-error (error)
  ((message :initarg :message :reader json-error-message)
   (position :initarg :position :initform nil :reader json-error-position))
  (:report (lambda (c s)
             (format s "Invalid JSON~@[ at character ~d~]: ~a"
                     (json-error-position c) (json-error-message c)))))

(defstruct (json-cursor (:conc-name jc-) (:copier nil))
  (text "" :type simple-string)
  (position 0 :type fixnum))

(defun jc-peek (c)
  (when (< (jc-position c) (length (jc-text c)))
    (char (jc-text c) (jc-position c))))

(defun jc-next (c)
  (let ((ch (jc-peek c)))
    (when ch (incf (jc-position c)))
    ch))

(defun jc-fail (c message &rest arguments)
  (error 'json-error :position (jc-position c)
                     :message (apply #'format nil message arguments)))

(defun jc-skip-space (c)
  (loop for ch = (jc-peek c)
        while (and ch (member ch '(#\Space #\Tab #\Newline #\Return)))
        do (incf (jc-position c))))

(defun jc-expect (c char)
  (let ((ch (jc-next c)))
    (unless (eql ch char)
      (jc-fail c "expected ~a but found ~@[~a~]~:[ end of input~;~]" char ch ch))))

;;; ----------------------------------------------------------------- strings

(defun json-read-escape (c)
  "One escape, after the backslash.  \\uXXXX is UTF-16, so a leading surrogate
must consume the trailing one -- otherwise anything outside the basic plane,
which is every emoji, decodes to two broken halves."
  (let ((ch (jc-next c)))
    (case ch
      (#\" #\") (#\\ #\\) (#\/ #\/)
      (#\b #\Backspace) (#\f #\Page) (#\n #\Newline) (#\r #\Return) (#\t #\Tab)
      (#\u (let ((code (json-read-hex4 c)))
             (cond
               ;; Leading surrogate: pair it with the trailing one that must
               ;; follow, and rebuild the code point.
               ((<= #xD800 code #xDBFF)
                (jc-expect c #\\)
                (jc-expect c #\u)
                (let ((low (json-read-hex4 c)))
                  (unless (<= #xDC00 low #xDFFF)
                    (jc-fail c "leading surrogate not followed by a trailing one"))
                  (code-char (+ #x10000
                                (ash (- code #xD800) 10)
                                (- low #xDC00)))))
               ((<= #xDC00 code #xDFFF)
                (jc-fail c "trailing surrogate with nothing before it"))
               (t (code-char code)))))
      (t (jc-fail c "unknown escape \\~@[~a~]" ch)))))

(defun json-read-hex4 (c)
  (let ((value 0))
    (dotimes (i 4 value)
      (declare (ignorable i))
      (let* ((ch (jc-next c))
             (digit (and ch (digit-char-p ch 16))))
        (unless digit (jc-fail c "\\u needs four hex digits"))
        (setf value (+ (* value 16) digit))))))

(defun json-read-string (c)
  (jc-expect c #\")
  (let ((out (make-string-output-stream)))
    (loop
      (let ((ch (jc-next c)))
        (cond ((null ch) (jc-fail c "unterminated string"))
              ((char= ch #\") (return (get-output-stream-string out)))
              ((char= ch #\\) (write-char (json-read-escape c) out))
              (t (write-char ch out)))))))

;;; ----------------------------------------------------------------- numbers

(defun json-read-digits (c)
  "One or more digits, consumed.  JSON requires at least one in each of the
three number parts, which is what makes `1.`, `.1` and `1e` invalid."
  (let ((start (jc-position c)))
    (loop for ch = (jc-peek c)
          while (and ch (digit-char-p ch))
          do (jc-next c))
    (when (= start (jc-position c))
      (jc-fail c "expected a digit"))))

(defun json-read-number (c)
  "Integer when it has no fraction or exponent, double otherwise.  An integer
stays exact -- a 64-bit id turned into a double would silently lose its low
bits, and ids are exactly what these documents are full of.

The grammar is enforced rather than approximated.  Scanning `digits, maybe a
dot, maybe an exponent` and handing the text to READ-FROM-STRING accepts `01`,
`1.`, `.1` and `1e`, none of which are JSON -- and accepting them means
returning a plausible number for input that is actually malformed, which is the
one thing this parser must not do."
  (let ((start (jc-position c))
        (floatp nil))
    (when (eql (jc-peek c) #\-) (jc-next c))
    ;; int := 0 | [1-9][0-9]*   -- a leading zero may not be followed by digits
    (let ((ch (jc-peek c)))
      (cond ((null ch) (jc-fail c "expected a number"))
            ((char= ch #\0)
             (jc-next c)
             (let ((next (jc-peek c)))
               (when (and next (digit-char-p next))
                 (jc-fail c "a number may not have a leading zero"))))
            ((digit-char-p ch) (json-read-digits c))
            (t (jc-fail c "expected a number"))))
    (when (eql (jc-peek c) #\.)
      (setf floatp t)
      (jc-next c)
      (json-read-digits c))
    (when (member (jc-peek c) '(#\e #\E))
      (setf floatp t)
      (jc-next c)
      (when (member (jc-peek c) '(#\+ #\-)) (jc-next c))
      (json-read-digits c))
    (let ((text (subseq (jc-text c) start (jc-position c))))
      (if floatp
          ;; An exponent can overflow a double; that is a malformed *document*
          ;; as far as a caller is concerned, not a floating-point condition to
          ;; leak out of the parser.
          (handler-case
              (let ((*read-default-float-format* 'double-float)
                    (*read-eval* nil))
                (read-from-string text))
            (error () (jc-fail c "number out of range: ~a" text)))
          (parse-integer text)))))

;;; ------------------------------------------------------------ the grammar

(defun json-key (name)
  "An object key as the keyword FIELD will match.  Upcased, because FIELD
compares field names case-insensitively everywhere else and .name has to reach
a key spelled \"name\", \"Name\" or \"NAME\"."
  (intern (string-upcase name) :keyword))

(defun json-read-value (c)
  (jc-skip-space c)
  (let ((ch (jc-peek c)))
    (case ch
      ((nil) (jc-fail c "unexpected end of input"))
      (#\{ (json-read-object c))
      (#\[ (json-read-array c))
      (#\" (json-read-string c))
      (#\t (json-read-literal c "true" t))
      (#\f (json-read-literal c "false" nil))
      (#\n (json-read-literal c "null" nil))
      (t (json-read-number c)))))

(defun json-read-literal (c text value)
  (let ((end (+ (jc-position c) (length text))))
    (unless (and (<= end (length (jc-text c)))
                 (string= text (jc-text c) :start2 (jc-position c) :end2 end))
      (jc-fail c "expected ~a" text))
    (setf (jc-position c) end)
    value))

(defun json-read-object (c)
  (jc-expect c #\{)
  (jc-skip-space c)
  (when (eql (jc-peek c) #\})
    (jc-next c)
    ;; An empty object is an empty plist, which FIELDS reports as no fields.
    (return-from json-read-object '()))
  (let ((plist '()))
    (loop
      (jc-skip-space c)
      (let ((key (json-key (json-read-string c))))
        (jc-skip-space c)
        (jc-expect c #\:)
        (push key plist)
        (push (json-read-value c) plist))
      (jc-skip-space c)
      (case (jc-next c)
        (#\, )
        (#\} (return (nreverse plist)))
        (t (jc-fail c "expected , or } in an object"))))))

(defun json-read-array (c)
  (jc-expect c #\[)
  (jc-skip-space c)
  (when (eql (jc-peek c) #\])
    (jc-next c)
    (return-from json-read-array '()))
  (let ((items '()))
    (loop
      (push (json-read-value c) items)
      (jc-skip-space c)
      (case (jc-next c)
        (#\, )
        (#\] (return (nreverse items)))
        (t (jc-fail c "expected , or ] in an array"))))))

(defun parse-json (text)
  "TEXT as Lisp data.  Signals JSON-ERROR, with the character position, rather
than returning something plausible: a shell that silently accepted truncated
JSON would give wrong answers instead of no answer."
  (let ((c (make-json-cursor :text (coerce text 'simple-string))))
    (prog1 (json-read-value c)
      (jc-skip-space c)
      (when (jc-peek c)
        (jc-fail c "trailing content after the value")))))

;;; ------------------------------------------------------------------ stage

(defun json-text-of (object)
  "The text carried by whatever arrived: LINEs from SH, or plain strings."
  (typecase object
    (line (line-text object))
    (string object)
    (t (present object))))

(defstage from-json (&key (lines nil))
  "Parse JSON arriving as text and emit it as objects.

  sh \"gh pr list --json number,title\" | from-json | where {(> .number 100)}
  sh \"docker ps --format json\" | from-json :lines | table
  from-file \"config.json\" | from-json | table :transpose

A top-level ARRAY is spread -- one object per element, which is what every
`--json` flag produces and what the rest of a pipeline wants.  Anything else is
emitted as the single value it is.

Objects become plists with upcased keyword keys, so .name reaches a key spelled
name, Name or NAME, and TABLE can list the columns.  true is T; false and null
are both NIL, deliberately: `where {.draft}` should work, and a missing key
already reads as NIL through FIELD.

Integers stay exact rather than becoming doubles -- a 64-bit id would silently
lose its low bits, and these documents are full of ids.

By default the whole input is one document, so this is a barrier.  :LINES parses
each input line as its own document instead, which is JSON Lines -- what log
pipelines and `docker ps --format json` emit -- and streams."
  (:consumes :objects) (:produces :objects) (:barrier t)
  (if lines
      (do-input (x)
        (let ((text (string-trim '(#\Space #\Tab #\Return) (json-text-of x))))
          (unless (string= text "")
            (emit (parse-json text)))))
      (let ((buffer (make-string-output-stream)))
        (do-input (x) (write-string (json-text-of x) buffer))
        (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                                 (get-output-stream-string buffer))))
          (unless (string= text "")
            (let ((value (parse-json text)))
              (if (listp value)
                  ;; A JSON array is a list, and so is an object -- but an
                  ;; object is a plist whose first element is a keyword, which
                  ;; an array of objects never is.
                  (if (and value (keywordp (first value)))
                      (emit value)
                      (dolist (item value) (emit item)))
                  (emit value))))))))
