;;;; json.lisp -- JSON in and out, on com.inuoe.jzon.
;;;;
;;;; This is the force multiplier rather than another source.  Every modern CLI
;;;; already speaks JSON -- gh, docker, kubectl, aws, ip -j -- so one reader
;;;; turns all of them into sources at once instead of a stage per tool, and one
;;;; writer sends any pipeline back out to them:
;;;;
;;;;   sh "ip -j addr" | from-json | where {(string= .operstate "UP")} | table
;;;;   ls "src/*.lisp" | to-json > files.json
;;;;
;;;; Loaded by the PLUMB/JSON system, not the core: jzon is an external library
;;;; and `plumb` itself stays free of those.  Like src/crypto.lisp it defines
;;;; into the PLUMB package and exports at load time, because the reader, HELP
;;;; and TAB completion all read that one package -- a stage in a package of its
;;;; own would be a second-class built-in.  The dumped binary loads every
;;;; optional system, so `plumb` on your path always has these.
;;;;
;;;; jzon does the lexing.  What is here is the two things it cannot know: how
;;;; JSON should look as *plumb objects*, and how a plumb object should look as
;;;; JSON.  Both mappings are chosen for a shell rather than for round-tripping,
;;;; and both are lossy in the same deliberate place -- see NIL below.
;;;;
;;;; The streaming event API (WITH-PARSER / PARSE-NEXT) rather than JZON:PARSE,
;;;; for two reasons.  JZON:PARSE returns a hash table, whose iteration order is
;;;; unspecified -- so TABLE's columns would come out in a different order run
;;;; to run.  And its keys are strings exactly as written, where FIELD compares
;;;; names case-insensitively, so `.name` would not reach a key spelled "Name".
;;;; Walking the events builds the right shape directly instead of building the
;;;; wrong one and converting it.

(in-package #:plumb)

(define-condition json-error (error)
  ((message :initarg :message :reader json-error-message)
   (position :initarg :position :initform nil :reader json-error-position))
  (:report (lambda (c s)
             (format s "Invalid JSON~@[ at character ~d~]: ~a"
                     (json-error-position c) (json-error-message c)))))

;;; ------------------------------------------------------------------ reading

(defun json-key (name)
  "An object key as the keyword FIELD will match.  Upcased, because FIELD
compares field names case-insensitively everywhere else and .name has to reach
a key spelled \"name\", \"Name\" or \"NAME\"."
  (intern (string-upcase name) :keyword))

(defun json-scalar (value)
  "One jzon scalar as plumb sees it.

jzon reports JSON null as the symbol NULL, true as T and false as NIL.  Here
null and false BOTH become NIL, deliberately: `where {.draft}` has to work, and
an absent key already reads as NIL through FIELD.  Telling null from false is
something a shell is the wrong tool for."
  (if (eq value 'null) nil value))

(defun json-read-event (parser event value)
  "One event, and everything nested inside it, as a Lisp value."
  (ecase event
    (:value (json-scalar value))
    (:begin-object
     (let ((plist '()))
       (loop
         (multiple-value-bind (event value) (com.inuoe.jzon:parse-next parser)
           (case event
             (:object-key
              (push (json-key value) plist)
              (multiple-value-bind (event value) (com.inuoe.jzon:parse-next parser)
                (push (json-read-event parser event value) plist)))
             (:end-object (return (nreverse plist)))
             (t (error 'json-error :message "malformed object")))))))
    (:begin-array
     (let ((items '()))
       (loop
         (multiple-value-bind (event value) (com.inuoe.jzon:parse-next parser)
           (case event
             (:end-array (return (nreverse items)))
             ((nil) (error 'json-error :message "unterminated array"))
             (t (push (json-read-event parser event value) items)))))))))

(defun parse-json (text)
  "TEXT as Lisp data: objects become plists with keyword keys, arrays lists.

Signals JSON-ERROR rather than returning something plausible -- a shell that
quietly accepted truncated JSON would give wrong answers instead of no answer.
jzon's own message carries the line and column, and is kept verbatim."
  (handler-case
      (com.inuoe.jzon:with-parser (parser text)
        (multiple-value-bind (event value) (com.inuoe.jzon:parse-next parser)
          (unless event
            (error 'json-error :message "no JSON value in the input"))
          (prog1 (json-read-event parser event value)
            ;; jzon stops at the end of the first value; anything after it means
            ;; the document was not one value.
            (when (com.inuoe.jzon:parse-next parser)
              (error 'json-error :message "trailing content after the value")))))
    (json-error (c) (error c))
    (error (c) (error 'json-error :message (princ-to-string c)))))

;;; ------------------------------------------------------------------ writing

(defun plistp (object)
  "A plist as this file makes them: a cons whose first element is a keyword.
An array of anything never looks like that, since only object keys become
keywords -- which is what lets one function tell a parsed object from a parsed
array without tagging either."
  (and (consp object) (keywordp (first object))))

(defun jsonable (object)
  "OBJECT as something jzon can write: hash tables for objects, vectors for
arrays, scalars for everything else.

Driven by FIELDS and FIELD, which every plumb object already answers, so this
serialises a FILE-ENTRY, a PROCESS, a COMMIT or a BLOCK-DEVICE without any of
them knowing about JSON -- the same reason TABLE works on all of them, and the
reason a type you add later needs no work here.

NIL becomes null.  It is also how false and the empty list arrive, so a document
that goes out and comes back is not always identical: the writer is lossy in
exactly the place the reader is, and for the same reason."
  (typecase object
    (null 'null)
    ((eql t) t)
    ((or string real) object)
    (symbol (string-downcase (symbol-name object)))
    (pathname (sb-ext:native-namestring object))
    ;; A condition is what rides the :ERR port and sits in DIGEST's .error;
    ;; its report is the only useful rendering.
    (condition (princ-to-string object))
    (hash-table object)
    (cons (if (plistp object)
              (let ((table (make-hash-table :test #'equal)))
                (loop for (key value) on object by #'cddr
                      do (setf (gethash (string-downcase (string key)) table)
                               (jsonable value)))
                table)
              (map 'vector #'jsonable object)))
    (vector (map 'vector #'jsonable object))
    (t (let ((keys (fields object)))
         (if keys
             (let ((table (make-hash-table :test #'equal)))
               (dolist (key keys table)
                 (setf (gethash (string-downcase (string key)) table)
                       (jsonable (field object key)))))
             (present object))))))

(defun to-json-string (object &key pretty)
  "OBJECT as a JSON document."
  (com.inuoe.jzon:stringify (jsonable object) :pretty pretty))

;;; ------------------------------------------------------------------ stages

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
name, Name or NAME, and TABLE lists the columns in document order.  true is T;
false and null are both NIL, deliberately: `where {.draft}` should work, and a
missing key already reads as NIL through FIELD.

By default the whole input is one document, so this is a barrier.  :LINES parses
each input line as its own document instead -- JSON Lines, what log pipelines
and `docker ps --format json` emit -- and streams."
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
              (if (and (listp value) (not (plistp value)))
                  (dolist (item value) (emit item))
                  (emit value))))))))

(defstage to-json (&key (lines nil) (pretty nil))
  "Render the stream as JSON text -- the other half of FROM-JSON.

  ls \"src/*.lisp\" | to-json > files.json
  ps | where {(> .rss 500mb)} | to-json :pretty
  disks | to-json :lines | to-sh \"jq -r .name\"

By default the whole stream is ONE JSON array, which is what a document wants
and what every --json reader expects, so it is a barrier.  :LINES writes one
document per object instead -- JSON Lines -- and streams.

Any plumb object serialises, because this is driven by FIELDS and FIELD rather
than by knowing the types: a FILE-ENTRY, a PROCESS, a COMMIT and a BLOCK-DEVICE
all work, and so does anything you define later.  Field names are lowercased,
keywords become strings, pathnames their namestrings, and a condition its
report.

Produces :BYTES like TO-TEXT, so it composes with TO-FILE and TO-SH rather than
ending the pipeline itself."
  (:consumes :objects) (:produces :bytes) (:barrier t)
  (if lines
      (do-input (x) (emit (to-json-string x :pretty pretty)))
      (let ((rows (make-array 16 :adjustable t :fill-pointer 0)))
        (do-input (x) (vector-push-extend (jsonable x) rows))
        (emit (com.inuoe.jzon:stringify (coerce rows 'simple-vector)
                                        :pretty pretty)))))

;;; Exported here rather than in package.lisp: these symbols only name anything
;;; once this system is loaded, and HELP lists exported symbols that are FBOUND.

(export '(from-json to-json parse-json to-json-string jsonable
          json-error json-error-message json-error-position)
        '#:plumb)
