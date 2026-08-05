;;;; crypto.lisp -- digests, via Ironclad.  Loaded by the PLUMB/CRYPTO system,
;;;; which is the one part of this project with an external dependency.
;;;;
;;;; The core stays dependency-free on purpose, so this is a separate system
;;;; rather than another component of "plumb".  What it is NOT is a separate
;;;; package: the reader interns in PLUMB, HELP reads PLUMB's export list, and
;;;; the CLI completes PLUMB's symbols, so a stage in a package of its own
;;;; would be a second-class built-in -- invisible to `help`, unresolvable at
;;;; the prompt.  The file therefore defines into PLUMB and exports at load
;;;; time, which is the only difference from src/stages.lisp.
;;;;
;;;; A digest is an object like everything else here, not a line of text:
;;;;
;;;;   ls "src/*.lisp" | digest :sha256 | where {(> .size 1kb)} | table
;;;;
;;;; and PRESENT renders it the way shasum(1) writes a line, so a pipeline
;;;; ending in PRINT-ITEMS is a drop-in for `shasum -a 256`.

(in-package #:plumb)

;;; ------------------------------------------------------------- algorithms

(defun digest-name (designator)
  "DESIGNATOR as the keyword Ironclad knows it by, or NIL if it knows no such
digest.  A string, a keyword and a plain symbol all work, so `digest :sha256`
and `digest sha256` mean the same thing at a word-mode prompt.

FIND-SYMBOL rather than INTERN: a typo should not leave a keyword behind, and
Ironclad has already interned every name it supports."
  (let ((name (typecase designator
                (string designator)
                (symbol (symbol-name designator))
                (t nil))))
    (when name
      (let ((key (find-symbol (string-upcase name) '#:keyword)))
        (when (and key (ignore-errors (ironclad:digest-supported-p key)))
          key)))))

(defun supported-digest-p (x)
  (and (digest-name x) t))

(deftype supported-digest ()
  "A digest this build of Ironclad implements.  (digests) lists them all."
  '(satisfies supported-digest-p))

(define-condition unknown-digest (error)
  ((name :initarg :name :reader unknown-digest-name))
  (:report (lambda (c s)
             (format s "~s is not a digest Ironclad supports; run `digests` for the list."
                     (unknown-digest-name c)))))

;;; ------------------------------------------------------------ the object

(defstruct digest
  algorithm
  hex                                   ; lowercase, as every checksum tool writes it
  bytes                                 ; the raw octets, for comparing without parsing
  source                                ; where it came from: a path, a stream, a line
  size                                  ; octets hashed, when that is knowable
  object                                ; the object this was computed from
  error)                                ; a DIGEST-FAILED, when HEX is NIL

(define-condition digest-failed (error)
  ((source :initarg :source :reader digest-failed-source)
   (algorithm :initarg :algorithm :reader digest-failed-algorithm)
   (cause :initarg :cause :reader digest-failed-cause))
  (:report (lambda (c s)
             (format s "~(~a~) of ~a failed: ~a"
                     (digest-failed-algorithm c)
                     (or (digest-failed-source c) "object")
                     (digest-failed-cause c)))))

(defmethod present ((object digest))
  "The shasum(1) line: hex, two spaces, name.  Byte-identical on purpose --
`ls \"*.lisp\" | digest :sha256 | print-items` should diff clean against
`shasum -a 256 *.lisp`."
  (cond ((digest-error object) (princ-to-string (digest-error object)))
        ((digest-source object)
         (format nil "~a  ~a" (digest-hex object) (digest-source object)))
        (t (digest-hex object))))

;;; ------------------------------------------------------------ computation

(defun hashable-file (entry)
  "The path to hash for ENTRY, or (VALUES NIL reason).

LS uses LSTAT, so a symlink arrives as a symlink; hashing follows it, as
shasum(1) does, but only after one STAT has confirmed the target is a regular
file.  Everything else is refused rather than opened -- a directory and a
device would error, and a FIFO would block this stage forever, which is the
same trap LS itself hit before it stopped calling FILE-LENGTH."
  (let* ((path (sb-ext:native-namestring (file-entry-path entry)))
         ;; TYPE comes from LSTAT; an entry built by hand may only have DIR-P.
         (type (or (file-entry-type entry)
                   (if (file-entry-dir-p entry) :directory :file))))
    (case type
      (:file (values path nil))
      (:symlink
       (let ((stat (ignore-errors (sb-posix:stat path))))
         (if (and stat (sb-posix:s-isreg (sb-posix:stat-mode stat)))
             (values path nil)
             (values nil "symbolic link does not resolve to a regular file"))))
      (t (values nil (format nil "not a regular file (~(~a~))" type))))))

(defun compute-digest (thing algorithm &key object source (external-format :utf-8))
  "Hash THING under ALGORITHM and return a DIGEST.  Never signals: a failure
comes back as the ERROR slot of a DIGEST whose HEX is NIL, so a bad file does
not vanish silently from a listing the way a filtered-out object would.

What gets hashed depends on what THING is:

  FILE-ENTRY   the file's contents          (a directory or FIFO is an error)
  PATHNAME     the file's contents
  LINE         its text, without the newline
  STRING       the string itself, encoded -- content, not a filename
  octet vector the octets
  STREAM       everything left in it

A string is content rather than a path because that is what arrives from SH
and FROM-FILE.  To hash files named by strings, say so with a key:
  digest :sha256 :key #'pathname"
  (let* ((algorithm (or (digest-name algorithm) algorithm))
         (object (or object thing))
         (bytes nil)
         (size nil))
    (flet ((fail (cause)
             ;; SOURCE and SIZE are whatever has been established by now, which
             ;; is why every branch below sets them BEFORE it hashes.  A digest
             ;; that failed still knows what it was of and how big -- LSTAT
             ;; answered that already.  Dropping them made .size NIL on exactly
             ;; the objects a filter was least expecting it, so
             ;; `where {(= .size N)}` died on the first unreadable file.
             (return-from compute-digest
               (make-digest :algorithm algorithm :object object
                            :source source :size size
                            :error (make-condition 'digest-failed
                                                   :algorithm algorithm
                                                   :source source
                                                   :cause cause))))
           (octets (string)
             (sb-ext:string-to-octets string :external-format external-format)))
      (handler-case
          (typecase thing
            (file-entry
             (setf source (or source (sb-ext:native-namestring
                                      (file-entry-path thing)))
                   size (file-entry-size thing))
             (multiple-value-bind (path reason) (hashable-file thing)
               (unless path (fail reason))
               (setf bytes (ironclad:digest-file algorithm path))))
            (line
             (let ((data (octets (line-text thing))))
               (setf source (or source (line-source thing))
                     size (length data))
               (setf bytes (ironclad:digest-sequence algorithm data))))
            (pathname
             (setf source (or source (sb-ext:native-namestring thing)))
             (let ((stat (ignore-errors (file-stat thing))))
               (when stat (setf size (fs-size stat))))
             (setf bytes (ironclad:digest-file algorithm thing)))
            (string
             (let ((data (octets thing)))
               (setf size (length data))
               (setf bytes (ironclad:digest-sequence algorithm data))))
            ((vector (unsigned-byte 8))
             (setf size (length thing))
             (setf bytes (ironclad:digest-sequence algorithm thing)))
            (stream
             (setf source (or source (princ-to-string thing)))
             (setf bytes (ironclad:digest-stream algorithm thing)))
            (t (fail (format nil "no bytes in ~s; pass :key to say what to hash"
                             (type-of thing)))))
        (error (c) (fail c)))
      (make-digest :algorithm algorithm
                   :hex (string-downcase (ironclad:byte-array-to-hex-string bytes))
                   :bytes bytes :size size :source source :object object))))

;;; ----------------------------------------------------------------- stages

(defstage digest ((algorithm supported-digest) &key key (external-format :utf-8))
  "Hash each object and emit a DIGEST carrying the hex, the raw octets, the
source and the original object.

ALGORITHM is any name Ironclad supports -- :sha256, :blake2, :md5, :sha3/256
and fifty-odd others; run `digests` for the list.  It is checked when the stage
is built, not when it runs, so a typo is an error at the prompt rather than a
condition inside a thread four hundred files in.

KEY picks what to hash out of each object, the way SORT-BY and UNIQ take one.
Without it the object itself is hashed: a FILE-ENTRY hashes the file, a LINE
its text, a string its own characters.

:WORKERS runs that many threads over one input.  Hashing is the case this was
built for -- it is CPU-bound and per-object, so eight workers use eight cores --
and it is genuinely safe here because every piece of per-object state is created
inside the stage body.  Output arrives in COMPLETION order, not input order;
add a SORT-BY when that matters.

A file that cannot be read does not disappear -- it comes through as a DIGEST
with a NIL hex and a DIGEST-FAILED in its ERROR slot, and the condition also
goes out the :ERR port.  Filter on .error to separate them.

  ls \"src/*.lisp\" | digest :sha256 | print-items
  ls \"**/*\" | where {.type :file} | digest :md5 | sort-by {.hex} | table"
  (:consumes :objects) (:produces :objects) (:parallel t)
  ;; Before the declared CHECK-TYPE, which would otherwise report a typo as
  ;; "not of type (SATISFIES SUPPORTED-DIGEST-P)" -- true, and no help at all
  ;; to someone who just wants to know what to type instead.
  (:check (unless (supported-digest-p algorithm)
            (error 'unknown-digest :name algorithm)))
  (let ((key (and key (ensure-fn key))))
    (do-input (x)
      (let ((result (compute-digest (if key (funcall key x) x) algorithm
                                    :object x :external-format external-format)))
        (when (digest-error result)
          ;; The default :ERR channel discards, so this is not a substitute for
          ;; the ERROR slot -- it is the same condition offered to whoever
          ;; wired the port.
          (ignore-errors (send (port :err) (digest-error result))))
        (emit result)))))

(defstage digests ()
  "Emit one row per digest Ironclad supports: its name, output length in
octets, and internal block length.  This is the answer to \"what can DIGEST
take\", in the same form as everything else here -- objects, so it sorts and
filters.

  digests | table
  digests | where {(= .length 32)} | table"
  (:consumes nil) (:produces :objects)
  (dolist (name (sort (copy-list (ironclad:list-all-digests)) #'string<))
    ;; DIGEST-LENGTH answers for a name; BLOCK-LENGTH only for an instance.
    (let ((instance (ignore-errors (ironclad:make-digest name))))
      (emit (list :name name
                  :length (ignore-errors (ironclad:digest-length name))
                  :block-length (and instance
                                     (ignore-errors (ironclad:block-length instance))))))))

;;; Exported here rather than in package.lisp: these symbols only name anything
;;; once this system is loaded, and HELP lists exported symbols that are FBOUND.
;;; Exporting them from the core would put DIGEST in `help` on a build that
;;; cannot run it.

(export '(digest digests compute-digest digest-name
          supported-digest supported-digest-p
          make-digest copy-digest digest-p
          digest-algorithm digest-hex digest-bytes digest-source digest-size
          digest-object digest-error
          digest-failed digest-failed-source digest-failed-algorithm
          digest-failed-cause
          unknown-digest unknown-digest-name)
        '#:plumb)
