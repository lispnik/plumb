;;;; crypto.lisp -- tests for the digest stages.
;;;;
;;;; Same package and the same CHECK/WITH-TIMEOUT as tests.lisp, so a crypto
;;;; failure counts and prints identically.  Separate file and separate system
;;;; because `make test` must keep running with no Ironclad anywhere.
;;;;
;;;; The digests themselves are checked against published vectors rather than
;;;; against Ironclad -- a test that hashes with the code under test and then
;;;; compares to the code under test proves nothing.

(in-package #:plumb/tests)

;;; ------------------------------------------------------------ known values

(defparameter +sha256-abc+
  "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  "FIPS 180-2 test vector for the string \"abc\".")

(defparameter +md5-abc+ "900150983cd24fb0d6963f7d28e17f72"
  "RFC 1321 test vector.")

(defparameter +sha256-empty+
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  "The empty string -- the case an off-by-one in the padding gets wrong.")

(defun digest-hex-of (thing algorithm)
  (digest-hex (compute-digest thing algorithm)))

;;; ------------------------------------------------------------ name lookup

(defun test-digest-names ()
  (check (eq :sha256 (digest-name :sha256)) :keyword-name)
  (check (eq :sha256 (digest-name "sha256")) :string-name)
  (check (eq :sha256 (digest-name "SHA256")) :case-insensitive-name)
  ;; `digest sha256` at a word-mode prompt reads as a PLUMB symbol, not a
  ;; keyword, and must mean the same thing.
  (check (eq :sha256 (digest-name (intern "SHA256" '#:plumb))) :plain-symbol-name)
  (check (null (digest-name :sha257)) :unknown-name-is-nil)
  (check (null (digest-name 17)) :non-designator-is-nil)
  (check (supported-digest-p :md5) :supported-p)
  (check (not (supported-digest-p :md6)) :unsupported-p)
  ;; A typo must not leave a keyword behind for the next lookup to find.
  (check (null (find-symbol "SHA999" '#:keyword)) :no-keyword-before)
  (digest-name "sha999")
  (check (null (find-symbol "SHA999" '#:keyword)) :typo-interns-nothing))

(defun test-bad-algorithm-fails-before-any-thread ()
  "The stage is built, not run, so an unknown digest is an error at the prompt
rather than a condition inside a thread four hundred files in."
  (let ((condition (nth-value 1 (ignore-errors (digest :sha257)))))
    (check (typep condition 'unknown-digest) :unknown-algorithm-is-refused)
    ;; The message has to say what to do next, not just restate the type.
    (check (search "digests" (princ-to-string condition)) :message-points-at-digests))
  (check (stage-p (digest :sha256)) :known-algorithm-builds)
  (check (stage-p (digest "md5")) :string-algorithm-builds))

;;; ----------------------------------------------------------- known vectors

(defun test-digest-vectors ()
  (check (string= +sha256-abc+ (digest-hex-of "abc" :sha256)) :sha256-abc)
  (check (string= +md5-abc+ (digest-hex-of "abc" :md5)) :md5-abc)
  (check (string= +sha256-empty+ (digest-hex-of "" :sha256)) :sha256-empty)
  ;; The octets, not the characters: hashing the string and hashing its UTF-8
  ;; encoding must agree, or the encoding is being chosen twice.
  (check (string= (digest-hex-of "abc" :sha256)
                  (digest-hex-of (sb-ext:string-to-octets "abc" :external-format :utf-8)
                                 :sha256))
         :string-and-octets-agree)
  ;; Non-ASCII is where a default external format would show up.
  (check (string= (digest-hex-of "é" :sha256)
                  (digest-hex-of (sb-ext:string-to-octets "é" :external-format :utf-8)
                                 :sha256))
         :utf-8-by-default)
  (check (string= (digest-hex-of "abc" :sha256) (digest-hex-of "abc" "sha256"))
         :string-designator-same-result))

(defun test-digest-bytes-match-hex ()
  "HEX is for reading; BYTES is for comparing.  They must be the same digest."
  (let ((d (compute-digest "abc" :sha256)))
    (check (= 32 (length (digest-bytes d))) :sha256-is-32-octets)
    (check (string= (digest-hex d)
                    (string-downcase (ironclad:byte-array-to-hex-string (digest-bytes d))))
           :bytes-and-hex-agree)
    (check (eq :sha256 (digest-algorithm d)) :algorithm-recorded)
    (check (= 3 (digest-size d)) :size-recorded)
    (check (null (digest-error d)) :no-error)))

;;; ------------------------------------------------------------------ files

(defmacro with-digest-fixture ((dir) &body body)
  "A directory holding the awkward cases: a normal file, an empty one, a
symlink to a file, a symlink to a directory, a FIFO and an unreadable file."
  `(let ((,dir "/tmp/plumb-digest-test/"))
     (flet ((sh (command)
              (sb-ext:run-program "/bin/sh" (list "-c" command) :search nil :wait t)))
       (unwind-protect
            (progn
              (sh (format nil "rm -rf ~a; mkdir -p ~asub" ,dir ,dir))
              (sh (format nil "cd ~a && printf abc > abc.txt && : > empty.txt && ~
ln -s abc.txt link && ln -s sub dirlink && mkfifo pipe && ~
printf secret > noread && chmod 000 noread" ,dir))
              ,@body)
         (sh (format nil "chmod 644 ~anoread 2>/dev/null; rm -rf ~a" ,dir ,dir))))))

(defun entry-named (dir name)
  "The entry for DIR/NAME itself.

Globbing the parent and picking by name, not (ls \"dir/name\"): LS lists a
directory's *members*, so asking it for a symlink that points at a directory
hands back that directory's contents -- NIL here, since sub/ is empty.  The
symlink test passed anyway, because a NIL object also fails to digest."
  (find name (collect-pipeline (list (ls (concatenate 'string dir "*"))))
        :key #'file-entry-name :test #'string=))

(defun test-digest-of-a-file ()
  "The file holds exactly \"abc\", so it must hash to the published vector --
which also proves the file path is being read rather than the file's name."
  (with-timeout (20 :digest-file)
    (with-digest-fixture (dir)
      (let ((d (compute-digest (entry-named dir "abc.txt") :sha256)))
        (check (string= +sha256-abc+ (digest-hex d)) :file-contents-hashed)
        (check (string= (concatenate 'string dir "abc.txt") (digest-source d))
               :source-is-the-path)
        (check (eql 3 (digest-size d)) :size-from-the-stat))
      (check (string= +sha256-empty+ (digest-hex-of (entry-named dir "empty.txt") :sha256))
             :empty-file)
      ;; A pathname is a file; a string is content.  Both are deliberate.
      (check (string= +sha256-abc+
                      (digest-hex-of (pathname (concatenate 'string dir "abc.txt"))
                                     :sha256))
             :pathname-is-a-file)
      (check (not (string= +sha256-abc+
                           (digest-hex-of (concatenate 'string dir "abc.txt") :sha256)))
             :string-is-content-not-a-path))))

(defun test-digest-follows-symlinks-but-not-into-trouble ()
  "LS uses LSTAT, so these arrive as symlinks.  One pointing at a file is
followed, as shasum(1) does; one pointing at a directory is refused."
  (with-timeout (20 :digest-symlinks)
    (with-digest-fixture (dir)
      (let ((link (compute-digest (entry-named dir "link") :sha256))
            (dirlink (compute-digest (entry-named dir "dirlink") :sha256)))
        (check (string= +sha256-abc+ (digest-hex link)) :symlink-to-file-followed)
        (check (null (digest-hex dirlink)) :symlink-to-directory-refused)
        (check (typep (digest-error dirlink) 'digest-failed) :refusal-is-a-condition)))))

(defun test-a-failed-digest-still-knows-its-source-and-size ()
  "A digest that failed keeps every field LSTAT already answered.  Dropping
them made .size NIL on exactly the objects a filter was least expecting it, so
`ls \"**/*.lisp\" | digest :md5 | where {(= .size 896)}` died on the first
directory or broken symlink the glob happened to match."
  (with-timeout (20 :failed-digest-fields)
    (with-digest-fixture (dir)
      (dolist (name '("pipe" "dirlink"))
        (let ((d (compute-digest (entry-named dir name) :sha256)))
          (check (null (digest-hex d)) :failure-has-no-hex)
          (check (integerp (digest-size d)) :failure-still-has-a-size)
          (check (search name (digest-source d)) :failure-still-has-its-source)))
      ;; The point of the above, stated as the pipeline that found it: a filter
      ;; on a numeric field must survive entries the digest could not read.
      (check (null (collect-pipeline
                    (list (ls (concatenate 'string dir "*"))
                          (digest :sha256)
                          (where ($ (= (fld :size) 896))))))
             :numeric-filter-survives-failures))))

(defun test-digest-refuses-a-fifo-instead-of-blocking ()
  "The bug LS had: opening a FIFO with no writer blocks forever.  Nothing
downstream can time this out, so the refusal has to happen before the open.
The WITH-TIMEOUT here is the whole assertion."
  (with-timeout (10 :digest-fifo)
    (with-digest-fixture (dir)
      (let ((d (compute-digest (entry-named dir "pipe") :sha256)))
        (check (null (digest-hex d)) :fifo-not-hashed)
        (check (search "not a regular file" (princ-to-string (digest-error d)))
               :fifo-reason-is-legible)))))

(defun test-unreadable-file-becomes-an-object-not-a-gap ()
  "A file that cannot be opened must still come through, or a checksum listing
silently omits exactly the files someone would want to know about."
  (with-timeout (20 :digest-unreadable)
    (with-digest-fixture (dir)
      ;; Running as root would make this file readable and the test vacuous.
      (when (plusp (sb-posix:getuid))
        (let ((d (compute-digest (entry-named dir "noread") :sha256)))
          (check (null (digest-hex d)) :unreadable-has-no-hex)
          (check (typep (digest-error d) 'digest-failed) :unreadable-has-an-error)
          (check (search "noread" (princ-to-string (digest-error d)))
                 :error-names-the-file))))))

;;; ----------------------------------------------------------------- stages

(defun test-digest-stage ()
  (with-timeout (30 :digest-stage)
    (with-digest-fixture (dir)
      (let* ((results (collect-pipeline
                       (list (ls (concatenate 'string dir "*.txt"))
                             (digest :sha256))))
             (by-name (lambda (name)
                        (find name results :key (lambda (d)
                                                  (file-entry-name (digest-object d)))
                                           :test #'string=))))
        (check (= 2 (length results)) :one-digest-per-file)
        (check (every #'digest-p results) :emits-digests)
        (check (string= +sha256-abc+ (digest-hex (funcall by-name "abc.txt")))
               :stage-hashes-contents)
        ;; The original object rides along, so a later stage can still filter
        ;; on .size or .mtime without a second LS.
        (check (every (lambda (d) (plumb::file-entry-p (digest-object d))) results)
               :original-object-retained)))))

(defun test-digest-stage-key ()
  "KEY says what to hash, the way SORT-BY and UNIQ take one."
  (with-timeout (20 :digest-key)
    (let ((results (collect-pipeline
                    (list (from-list (list "abc" "xyz"))
                          (digest :sha256 :key (lambda (s) (subseq s 0 3)))))))
      (check (string= +sha256-abc+ (digest-hex (first results))) :key-applied)
      ;; OBJECT is the object that arrived, not what the key returned.
      (check (string= "abc" (digest-object (first results))) :object-is-the-input))))

(defun test-digest-of-lines ()
  (with-timeout (20 :digest-lines)
    (let ((d (first (collect-pipeline
                     (list (from-list (list (make-line :text "abc" :number 1
                                                       :source "somewhere")))
                           (digest :sha256))))))
      (check (string= +sha256-abc+ (digest-hex d)) :line-text-hashed)
      (check (string= "somewhere" (digest-source d)) :line-source-carried))))

(defun test-digest-of-something-with-no-bytes ()
  "An integer has no obvious byte sequence.  Refusing beats inventing one."
  (with-timeout (20 :digest-no-bytes)
    (let ((d (first (collect-pipeline (list (from-list (list 42)) (digest :sha256))))))
      (check (null (digest-hex d)) :integer-not-hashed)
      (check (search ":key" (princ-to-string (digest-error d))) :error-suggests-key))))

(defun test-digest-errors-reach-the-err-port ()
  "The ERROR slot keeps the object in the stream; the :ERR port is the same
condition offered to whoever wired that port.  Both, not either."
  (with-timeout (20 :digest-err-port)
    (with-digest-fixture (dir)
      (let* ((err (make-channel :capacity 8))
             (pipe (run (list (ls (concatenate 'string dir "pipe")) (digest :sha256))
                        :err err)))
        (join pipe)
        (close-output err)
        (multiple-value-bind (condition ok) (recv err)
          (check ok :err-port-received-something)
          (check (typep condition 'digest-failed) :err-port-carries-the-condition))))))

(defun test-digest-presentation-matches-shasum ()
  "PRESENT is what PRINT-ITEMS writes, and shasum(1) writes hex, two spaces,
name.  Compared against shasum itself so the claim is not self-referential."
  (with-timeout (30 :digest-present)
    (with-digest-fixture (dir)
      (let* ((path (concatenate 'string dir "abc.txt"))
             (ours (present (compute-digest (entry-named dir "abc.txt") :sha256)))
             (theirs (with-output-to-string (out)
                       (sb-ext:run-program "/bin/sh"
                                           (list "-c" (format nil "shasum -a 256 ~a" path))
                                           :search nil :wait t :output out))))
        (check (string= ours (string-right-trim '(#\Newline) theirs))
               :present-is-a-shasum-line))
      ;; No source to name -- just the hex, with no trailing separator.
      (check (string= +sha256-abc+ (present (compute-digest "abc" :sha256)))
             :sourceless-digest-is-bare-hex)
      ;; A failure presents as its condition rather than as an empty hex.
      (check (search "not a regular file"
                     (present (compute-digest (entry-named dir "pipe") :sha256)))
             :failed-digest-presents-the-reason))))

(defun test-digests-listing ()
  (with-timeout (30 :digests-listing)
    (let ((rows (collect-pipeline (list (digests)))))
      (check (= (length (ironclad:list-all-digests)) (length rows)) :one-row-per-digest)
      (check (find :sha256 rows :key (lambda (r) (field r :name))) :sha256-listed)
      (let ((sha256 (find :sha256 rows :key (lambda (r) (field r :name)))))
        (check (eql 32 (field sha256 :length)) :sha256-length)
        (check (eql 64 (field sha256 :block-length)) :sha256-block-length))
      ;; Sorted, so two runs render the same table.
      (check (equal (mapcar (lambda (r) (field r :name)) rows)
                    (sort (mapcar (lambda (r) (field r :name)) rows) #'string<))
             :listing-is-sorted)
      ;; Every listed name must actually be usable -- the listing is the answer
      ;; to "what can DIGEST take", so a name in it that DIGEST rejects is a lie.
      (check (every (lambda (r) (supported-digest-p (field r :name))) rows)
             :every-listed-digest-is-accepted))))

;;; --------------------------------------------------------- integration

(defun test-digest-under-workers ()
  "DIGEST is the case :WORKERS was built for -- CPU-bound and per-object -- and
it is safe because every piece of per-object state is made inside the stage
body.  What must hold is that the answers do not depend on the worker count."
  (with-timeout (60 :digest-workers)
    (with-digest-fixture (dir)
      (let ((one (collect-pipeline (list (ls (concatenate 'string dir "*.txt"))
                                         (digest :sha256))))
            (many (collect-pipeline (list (ls (concatenate 'string dir "*.txt"))
                                          (digest :sha256 :workers 4)))))
        (check (= (length one) (length many)) :same-count)
        ;; Same set of (source . hex) pairs; the ORDER is explicitly not promised.
        (flet ((pairs (ds) (sort (mapcar (lambda (d) (format nil "~a ~a"
                                                             (digest-source d)
                                                             (digest-hex d)))
                                         ds)
                                 #'string<)))
          (check (equal (pairs one) (pairs many)) :same-digests-whatever-the-worker-count)))
      ;; A file that cannot be hashed still comes through under workers, and
      ;; still does not take the other workers down with it.
      (let ((results (collect-pipeline (list (ls (concatenate 'string dir "*"))
                                             (digest :sha256 :workers 4)))))
        (check (find-if #'digest-error results) :failures-survive-parallelism)
        (check (find-if #'digest-hex results) :successes-survive-alongside-them))))
  (check (stage-parallel (digest :md5)) :digest-declares-parallel))

(defun test-digest-is-a-first-class-builtin ()
  "The reason crypto.lisp defines into PLUMB rather than a package of its own:
HELP, the reader and TAB completion all read PLUMB's registry and export list."
  (check (gethash 'plumb::digest *stages*) :digest-is-registered)
  (check (gethash 'plumb::digests *stages*) :digests-is-registered)
  (check (eq (find-symbol "DIGEST" '#:plumb) 'plumb::digest) :digest-interned-in-plumb)
  (check (search "sha256" (help-output (help digest))) :help-knows-digest)
  (check (member "digest" (plumb.cli::plumb-completions "dig") :test #'string=)
         :tab-completes-digest))

(defun test-digest-typing ()
  "DIGEST consumes and produces objects, so it composes like any other
transform -- and CHECK-PIPELINE says so before a thread exists."
  (check (check-pipeline (list (ls) (digest :md5) (table))) :digest-composes)
  (check (typep (nth-value 1 (ignore-errors
                              (check-pipeline (list (print-items) (digest :md5)))))
                'pipeline-type-error)
         :nothing-follows-a-sink))

;;; ------------------------------------------------------------------ runner

(defun run-crypto-tests ()
  (let ((*passed* 0) (*failed* '()))
    (dolist (fn '(test-digest-names
                  test-bad-algorithm-fails-before-any-thread
                  test-digest-vectors
                  test-digest-bytes-match-hex
                  test-digest-of-a-file
                  test-digest-follows-symlinks-but-not-into-trouble
                  test-digest-refuses-a-fifo-instead-of-blocking
                  test-a-failed-digest-still-knows-its-source-and-size
                  test-unreadable-file-becomes-an-object-not-a-gap
                  test-digest-stage
                  test-digest-stage-key
                  test-digest-of-lines
                  test-digest-of-something-with-no-bytes
                  test-digest-errors-reach-the-err-port
                  test-digest-presentation-matches-shasum
                  test-digests-listing
                  test-digest-under-workers
                  test-digest-is-a-first-class-builtin
                  test-digest-typing))
      (format t "~&; ~a~%" fn)
      (funcall fn))
    (format t "~&~%~d passed, ~d failed~%" *passed* (length *failed*))
    (dolist (f (reverse *failed*))
      (format t "  FAIL: ~s~%" f))
    (null *failed*)))
