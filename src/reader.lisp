;;;; reader.lisp -- word mode: the surface syntax field.lisp was built for.
;;;;
;;;;   ls src/ | where {(> .size 1kb)} | sort-by .size :desc | take 5
;;;;
;;;; becomes
;;;;
;;;;   (list (ls "src/") (where ($ (> (fld :size) 1024)))
;;;;         (sort-by ($ (fld :size)) :desc t) (take 5))
;;;;
;;;; and is handed to the ordinary evaluator, so stages, PRESENT, teardown and
;;;; HELP all work on it unchanged.  This file is a source-to-source pass and
;;;; nothing else; it knows no semantics.
;;;;
;;;; The decisions behind the grammar are in CLAUDE.md under open work 5 --
;;;; briefly: a leading ( means Lisp at every level, {...} is the word/expression
;;;; boundary, earmuffed words are variables and every other bare word is a
;;;; string, .name is shorthand for {.name}, a trailing keyword means T, and
;;;; there is no fallback to an external command.

(in-package #:plumb)

(defun shell-syntax-p (text)
  "Does TEXT want the word reader?  A leading ( means it is Lisp already."
  (let ((trimmed (string-left-trim '(#\Space #\Tab #\Newline) text)))
    (and (plusp (length trimmed))
         (char/= (char trimmed 0) #\())))

;;; ---------------------------------------------------------------- scanning

(defun scan-string-token (text start)
  "From the opening quote to the closing one, honouring backslash escapes."
  (let ((i (1+ start)) (n (length text)))
    (loop while (< i n)
          do (cond ((char= (char text i) #\\) (incf i 2))
                   ((char= (char text i) #\") (return))
                   (t (incf i))))
    (values (subseq text start (min n (1+ i))) (min n (1+ i)))))

(defun scan-balanced-token (text start open close)
  "A {...} or (...) group.  Depth-counted, and string literals inside are
skipped whole -- both delimiters and pipes can legally appear in a string."
  (let ((i start) (n (length text)) (depth 0))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond ((char= c #\") (setf i (nth-value 1 (scan-string-token text i))))
                     ((char= c open) (incf depth) (incf i))
                     ((char= c close) (decf depth) (incf i)
                      (when (zerop depth) (return)))
                     (t (incf i)))))
    (values (subseq text start i) i)))

(defun word-boundary-p (c)
  (or (member c '(#\Space #\Tab #\Newline #\|))
      (member c '(#\{ #\( #\"))))

(defun scan-word-token (text start)
  (let ((i start) (n (length text)))
    (loop while (and (< i n) (not (word-boundary-p (char text i)))) do (incf i))
    (values (subseq text start i) i)))

(defun shell-tokens (text)
  "TEXT as a list of token strings.  A |, a quoted string, a {...} block, a
(...) form, or a run of ordinary characters.  Splitting on | cannot be a
separate first pass: blocks, forms and strings may all contain one."
  (let ((tokens '()) (i 0) (n (length text)))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond
                 ((member c '(#\Space #\Tab #\Newline)) (incf i))
                 ((char= c #\|) (push "|" tokens) (incf i))
                 ((char= c #\")
                  (multiple-value-bind (tok j) (scan-string-token text i)
                    (push tok tokens) (setf i j)))
                 ((char= c #\{)
                  (multiple-value-bind (tok j) (scan-balanced-token text i #\{ #\})
                    (push tok tokens) (setf i j)))
                 ((char= c #\()
                  (multiple-value-bind (tok j) (scan-balanced-token text i #\( #\))
                    (push tok tokens) (setf i j)))
                 ;; #'foo, #(1 2), #p"/tmp", #\a -- let the Lisp reader find
                 ;; where the object ends rather than guessing at delimiters,
                 ;; since several of these contain characters a word stops at.
                 ((char= c #\#)
                  (let ((end (nth-value 1 (read-from-string text nil nil :start i))))
                    (push (subseq text i end) tokens)
                    (setf i end)))
                 (t
                  (multiple-value-bind (tok j) (scan-word-token text i)
                    (push tok tokens) (setf i j))))))
    (nreverse tokens)))

(defun split-on-pipes (tokens)
  (let ((segments '()) (current '()))
    (dolist (tok tokens)
      (if (string= tok "|")
          (progn (push (nreverse current) segments) (setf current '()))
          (push tok current)))
    (push (nreverse current) segments)
    (remove nil (nreverse segments))))

;;; ------------------------------------------------------------- token forms

(defun field-token-p (token)
  (and (> (length token) 1) (char= (char token 0) #\.)))

(defun field-keyword (name)
  "\".size\" or the symbol .SIZE -> :SIZE.  FIELD compares case-insensitively,
so the keyword only has to name the slot, not match its print case."
  (intern (string-upcase (subseq (string name) 1)) :keyword))

(defun field-symbol-p (object)
  (and (symbolp object) object (field-token-p (symbol-name object))))

(defun expand-block-syntax (form)
  "Inside a block, replace .NAME with (FLD :NAME) and 1kb with 1024.  Block
contents are READ as Lisp, so both arrive as symbols rather than as tokens.
A symbol walk, so strings in the tree are left alone -- textual substitution
would corrupt them."
  (cond ((field-symbol-p form) (list 'fld (field-keyword (symbol-name form))))
        ((and (symbolp form) form (suffixed-number (symbol-name form))))
        ((consp form) (cons (expand-block-syntax (car form))
                            (expand-block-syntax (cdr form))))
        (t form)))

(defun earmuffed-p (token)
  "*x* and +x+ name variables; every other bare word is a string.  Both ends
must match, which is what keeps *.lisp and a lone * as globs."
  (and (>= (length token) 3)
       (let ((c (char token 0)))
         (and (member c '(#\* #\+))
              (char= (char token (1- (length token))) c)))))

(defun numeric-token (token)
  "The number TOKEN denotes with no suffix, or NIL.  Must consume the whole
token, so 5kb falls through to SUFFIXED-NUMBER."
  (multiple-value-bind (value end)
      (ignore-errors (let ((*read-eval* nil)) (read-from-string token)))
    (when (and (numberp value) (eql end (length token))) value)))

;;; Suffix literals.
;;;
;;; Sizes are binary, because every tool that prints a size this way -- ls -h,
;;; du -h -- means 1024.  Durations are seconds, so they compose with
;;; GET-UNIVERSAL-TIME, which is what .mtime is measured in.
;;;
;;; Minutes are spelled MIN, not M.  M is megabytes here, and a unit that means
;;; two different things depending on the field it is compared against is a
;;; silent wrong answer rather than an error.

(defparameter +suffix-multipliers+
  '(("k" . 1024) ("kb" . 1024) ("kib" . 1024)
    ("m" . 1048576) ("mb" . 1048576) ("mib" . 1048576)
    ("g" . 1073741824) ("gb" . 1073741824) ("gib" . 1073741824)
    ("t" . 1099511627776) ("tb" . 1099511627776) ("tib" . 1099511627776)
    ("s" . 1) ("sec" . 1) ("min" . 60) ("h" . 3600)
    ("d" . 86400) ("w" . 604800)))

(defun split-numeral-and-suffix (name)
  "\"1kb\" -> \"1\" and \"kb\".  NIL unless NAME starts with a numeral, which
is what keeps symbols like 1+ and x1k out of this."
  (let* ((start (if (and (plusp (length name)) (member (char name 0) '(#\- #\+))) 1 0))
         (i start))
    (loop while (and (< i (length name))
                     (or (digit-char-p (char name i)) (char= (char name i) #\.)))
          do (incf i))
    (when (and (> i start) (< i (length name)))
      (values (subseq name 0 i) (subseq name i)))))

(defun suffixed-number (name)
  "The number \"1kb\" denotes, or NIL.  Case-insensitive, so 1KB works too."
  (multiple-value-bind (numeral suffix) (split-numeral-and-suffix (string name))
    (when numeral
      (let ((multiplier (cdr (assoc suffix +suffix-multipliers+ :test #'string-equal)))
            (n (ignore-errors (let ((*read-eval* nil)) (read-from-string numeral)))))
        (when (and multiplier (realp n))
          (let ((value (* n multiplier)))
            ;; 1.5kb is 1536, not 1536.0 -- a size that lands on a whole
            ;; number should compare as one.
            (if (and (floatp value) (= value (ffloor value)))
                (round value)
                value)))))))

(defun read-lisp-token (token)
  (let ((*package* (find-package '#:plumb)))
    (read-from-string token)))

(defun read-lisp-forms (text)
  (let ((*package* (find-package '#:plumb))
        (forms '()) (pos 0))
    (loop (multiple-value-bind (form next) (read-from-string text nil :eof :start pos)
            (when (eq form :eof) (return (nreverse forms)))
            (push form forms)
            (setf pos next)))))

(defun block-form (token)
  "{...} -> ($ ...), with .name accessors expanded inside."
  (let ((body (read-lisp-forms (subseq token 1 (max 1 (1- (length token)))))))
    `($ ,@(expand-block-syntax body))))

(defun token-form (token)
  (let ((c (char token 0)))
    (cond ((char= c #\") (read-lisp-token token))
          ((char= c #\{) (block-form token))
          ((char= c #\() (read-lisp-token token))
          ((char= c #\#) (read-lisp-token token))
          ((char= c #\:) (read-lisp-token token))
          ;; .name on its own is shorthand for the block {.name}
          ((field-token-p token) (list '$ (list 'fld (field-keyword token))))
          ((earmuffed-p token) (read-lisp-token token))
          ((numeric-token token))
          ((suffixed-number token))
          (t token))))

(defun keyword-token-p (token)
  (and token (plusp (length token)) (char= (char token 0) #\:)))

(defun segment-form (tokens)
  "One | segment as a stage call.  A segment that is a single (...) form is
that form verbatim, which is how a stage the word syntax cannot spell gets in."
  (let ((head (first tokens)))
    (if (and (null (rest tokens)) (char= (char head 0) #\())
        (read-lisp-token head)
        (let ((name (read-lisp-token head))
              (args '()))
          (loop for rest on (rest tokens)
                for token = (car rest)
                do (push (token-form token) args)
                   ;; A shell flag carries no value, so a keyword with nothing
                   ;; after it -- or another keyword -- means :key T.
                   (when (and (keyword-token-p token)
                              (or (null (cdr rest)) (keyword-token-p (cadr rest))))
                     (push t args)))
          (cons name (nreverse args))))))

(defun value-token-p (token)
  "Does TOKEN denote a value rather than a stage name?  Only a bare word names
a stage; a string, number, keyword, block, .field, (form) or earmuffed variable
is something to evaluate."
  (let ((c (char token 0)))
    (or (member c '(#\" #\{ #\( #\: #\#))
        (field-token-p token)
        (earmuffed-p token)
        (and (numeric-token token) t)
        (and (suffixed-number token) t))))

(defparameter +reserved-words+ '("explain")
  "Words that wrap the whole pipeline rather than naming a stage, the way
bash's `time` does.  A closed list of keywords -- not a general prefix
mechanism, which would be the stage-position guessing we ruled out.")

(defun shell-form (segments)
  (cond
      ;; More than one segment is always a pipeline.
      ((rest segments) (cons 'list (mapcar #'segment-form segments)))
      ;; A lone value is that value, not a one-stage pipeline: `*default-capacity*`
      ;; has to keep printing 64 the way it did before word mode existed.
      ((and (null (rest (first segments)))
            (value-token-p (first (first segments))))
       (token-form (first (first segments))))
      ;; A single stage needs no LIST wrapper -- PRESENT runs a bare stage --
      ;; and must not get one: `help` returns no values, and (list (help))
      ;; would turn that into a printed NIL.
      (t (segment-form (first segments)))))

(defun read-shell (text)
  "Word-mode TEXT as a Lisp form."
  (let ((segments (split-on-pipes (shell-tokens text))))
    (unless segments (return-from read-shell nil))
    (let ((head (first (first segments))))
      (if (and (member head +reserved-words+ :test #'string-equal)
               (or (rest segments) (rest (first segments))))
          (list (read-lisp-token (string-downcase head))
                (shell-form (cons (rest (first segments)) (rest segments))))
          (shell-form segments)))))
