;;;; tests.lisp -- no test-framework dependency, deliberately.

(defpackage #:plumb/tests
  (:use #:cl #:plumb)
  (:export #:run-tests))

(in-package #:plumb/tests)

(defvar *passed* 0)
(defvar *failed* '())

(defmacro check (form &optional (label nil))
  `(handler-case
       (if ,form
           (incf *passed*)
           (push (or ,label ',form) *failed*))
     (error (c) (push (list (or ,label ',form) c) *failed*))))

(defmacro with-timeout ((seconds label) &body body)
  "Any hang in this system is a teardown bug, so every test is time-boxed."
  `(handler-case
       (sb-ext:with-timeout ,seconds ,@body)
     (sb-ext:timeout () (push (list ,label :TIMED-OUT) *failed*) :timeout)))

(defmacro help-output (&body body)
  "Capture what a command prints.  A string stream is not interactive, so PAINT
leaves it plain and the assertions can look for bare text."
  `(with-output-to-string (*standard-output*) ,@body))

;;; ------------------------------------------------------------- channels

(defun test-channel-basics ()
  (let ((ch (make-channel :capacity 2)))
    (send ch 1) (send ch 2)
    (check (eql 1 (recv ch)) :fifo-1)
    (check (eql 2 (recv ch)) :fifo-2)
    (close-output ch)
    (check (null (nth-value 1 (recv ch))) :eof-second-value))
  ;; NIL must survive a round trip as a legal payload.
  (let ((ch (make-channel)))
    (send ch nil)
    (multiple-value-bind (obj ok) (recv ch)
      (check (null obj) :nil-payload)
      (check ok :nil-payload-ok))))

(defun test-buffer-drains-before-eof ()
  (let ((ch (make-channel)))
    (send ch :a) (send ch :b)
    (close-output ch)
    (check (eq :a (recv ch)) :drain-1)
    (check (eq :b (recv ch)) :drain-2)
    (check (null (nth-value 1 (recv ch))) :drain-eof)))

(defun test-backpressure ()
  "A producer must not run more than CAPACITY ahead of its consumer."
  (with-timeout (5 :backpressure)
    (let* ((ch (make-channel :capacity 4))
           (produced 0)
           (th (sb-thread:make-thread
                (lambda () (dotimes (i 100) (send ch i) (incf produced))))))
      (sleep 0.2)
      (check (<= produced 5) :producer-blocked)
      (dotimes (i 100) (recv ch))
      (sb-thread:join-thread th)
      (check (= produced 100) :producer-finished))))

(defun test-close-input-wakes-producer ()
  "The SIGPIPE path, including a producer already parked on a full channel."
  (with-timeout (5 :close-input)
    (let* ((ch (make-channel :capacity 2))
           (result :none)
           (th (sb-thread:make-thread
                (lambda ()
                  (handler-case (dotimes (i 1000) (send ch i))
                    (channel-closed () (setf result :closed)))))))
      (sleep 0.2)
      (close-input ch)
      (sb-thread:join-thread th)
      (check (eq result :closed) :producer-saw-close))))

;;; ------------------------------------------------------------ pipelines

(defun test-simple-pipeline ()
  (with-timeout (5 :simple)
    (check (equal '(2 4 6 8 10)
                  (collect-pipeline (list (from-list '(1 2 3 4 5))
                                          (xform (lambda (n) (* 2 n))))))
           :map)
    (check (equal '(2 4)
                  (collect-pipeline (list (from-list '(1 2 3 4 5))
                                          (where #'evenp))))
           :where)))

(defun test-take-tears-down-infinite-source ()
  "The headline property: TAKE against a source that never ends."
  (with-timeout (5 :infinite-take)
    (let ((result (collect-pipeline (list (counter) (take 5)))))
      (check (equal '(0 1 2 3 4) result) :take-values))))

(defun test-teardown-cascades-through-many-stages ()
  (with-timeout (5 :cascade)
    (check (equal '(0 2 4)
                  (collect-pipeline (list (counter)
                                          (where #'evenp)
                                          (xform #'identity)
                                          (take 3))))
           :cascade-values)))

(defun test-source-thread-actually-dies ()
  "Not just 'the right values came back' -- no thread is left running."
  (with-timeout (10 :threads-die)
    (let* ((sink (make-channel :capacity 4))
           (pipe (run (list (counter) (take 3)) :sink sink)))
      (loop (multiple-value-bind (x ok) (recv sink)
              x
              (unless ok (return))))
      (join pipe)
      (check (notany #'sb-thread:thread-alive-p (pipeline-threads pipe))
             :all-threads-dead))))

(defun test-collecting-stage ()
  (with-timeout (5 :sort)
    (check (equal '(1 2 3 5 9)
                  (collect-pipeline (list (from-list '(5 3 9 1 2)) (sort-by #'identity))))
           :sort-asc)
    (check (equal '(9 5 3 2 1)
                  (collect-pipeline (list (from-list '(5 3 9 1 2))
                                          (sort-by #'identity :desc t))))
           :sort-desc)))

(defun test-fields-and-block ()
  (let ((rows '((:name "a" :size 10) (:name "b" :size 3000))))
    (check (equal '("b")
                  (collect-pipeline (list (from-list rows)
                                          (where ($ (> (fld :size) 100)))
                                          (xform ($ (fld :name))))))
           :plist-fields))
  (let ((e (make-file-entry :name "x" :size 42)))
    (check (eql 42 (field e :size)) :struct-field)
    (check (member :size (fields e)) :struct-fields-list)))

(defun test-error-is-recorded-and-tears-down ()
  (with-timeout (5 :errors)
    (let* ((pipe (run (list (counter)
                            (xform (lambda (n) (if (= n 3) (error "boom") n)))))))
      (join pipe)
      (check (= 1 (length (pipeline-failures pipe))) :one-failure)
      (check (eq 'xform (car (first (pipeline-failures pipe)))) :failure-attributed)
      (check (notany #'sb-thread:thread-alive-p (pipeline-threads pipe))
             :error-tears-down))))

(defun test-error-object-reaches-err-port ()
  (with-timeout (5 :err-port)
    (let* ((err (make-channel))
           (pipe (run (list (from-list '(1 2 3))
                            (xform (lambda (n) (if (= n 2) (error "boom") n))))
                      :err err)))
      (join pipe)
      (close-output err)
      (let ((c (recv err)))
        ;; A condition object, not a string on stderr.
        (check (typep c 'simple-error) :condition-object)))))

(defun test-type-check ()
  (check (handler-case
             (progn (run (list (from-list '(1)) (to-text) (where #'evenp))) nil)
           (pipeline-type-error () t))
         :type-mismatch-caught)
  (check (check-pipeline (list (from-list '(1)) (where #'evenp) (print-items)))
         :valid-pipeline-passes))

(defun test-cancel ()
  (with-timeout (5 :cancel)
    (let ((pipe (run (list (counter) (xform #'identity)))))
      (sleep 0.1)
      (cancel pipe)
      (check (notany #'sb-thread:thread-alive-p (pipeline-threads pipe)) :cancelled))))

(defun test-each-backpressure-end-to-end ()
  "A slow consumer must not let the source race ahead unboundedly."
  (with-timeout (10 :each)
    (let ((seen 0) (max-seen 0))
      (each (list (counter :limit 50) (take 20))
            (lambda (x) x (incf seen) (setf max-seen seen)))
      (check (= 20 max-seen) :each-count))))

(defun names (paths) (mapcar #'file-namestring paths))

(defun test-glob ()
  "GLOB backs LS.  Run from the project root, which the test suite is."
  (with-timeout (10 :glob)
    (check (member "channel.lisp" (names (glob "src/*.lisp")) :test #'string=)
           :star-matches-by-type)
    (check (member "field.lisp" (names (glob "src/?ield.lisp")) :test #'string=)
           :question-mark-is-one-character)
    (check (= 3 (length (glob "src/[cf]*.lisp"))) :character-class)
    ;; ** descends, so it finds strictly more than a flat *.
    (check (> (length (glob "**/*.lisp")) (length (glob "*.lisp"))) :double-star-descends)
    ;; Shell * means anything.  CL * means "any name, no type", so an
    ;; untranslated pattern would miss every file with an extension.
    (check (member "README.md" (names (glob "*")) :test #'string=)
           :bare-star-matches-extensions-too)
    ;; A directory lists its members with or without the trailing slash.
    ;; Without the slash this used to merge to *.* carrying no directory
    ;; component, and silently listed the current directory instead.
    (check (equal (names (glob "src/")) (names (glob "src"))) :trailing-slash-optional)
    (check (member "channel.lisp" (names (glob "src")) :test #'string=) :directory-lists-members)
    ;; A plain file is itself; a pattern matching nothing is empty, not an error.
    (check (equal '("README.md") (names (glob "README.md"))) :a-file-names-itself)
    (check (null (glob "*.nosuchtype")) :no-matches-is-empty)
    ;; A dotfile written bare parses as the .name accessor shorthand.  It
    ;; cannot be disambiguated lexically -- .gitignore and .size are the same
    ;; shape -- so the fix is quoting, and the job here is to say so.
    (check (equal '(ls ($ (fld :gitignore))) (read-shell "ls .gitignore"))
           :bare-dotfile-is-read-as-an-accessor)
    (check (equal '(ls ".gitignore") (read-shell "ls \".gitignore\""))
           :quoting-a-dotfile-works)
    (check (search "needs quoting"
                   (handler-case (progn (glob (lambda (it) it)) "")
                     (error (c) (princ-to-string c))))
           :the-error-names-the-fix)
    ;; A lone . is one character, below the accessor threshold, so it stays a
    ;; path and `ls .` means the current directory.
    (check (equal '(ls ".") (read-shell "ls .")) :lone-dot-is-a-path)
    (check (member "README.md" (names (glob ".")) :test #'string=) :dot-is-the-cwd)
    ;; Sorted, so pipelines built on LS are reproducible.
    (let ((got (names (glob "src/*.lisp"))))
      (check (equal got (sort (copy-list got) #'string<)) :results-are-sorted))))

(defun test-sink-ends-the-pipeline ()
  "T on the consuming side means any object *type*, not the absence of one.
Reading it as the latter let a sink follow a sink, and EXPLAIN said fine."
  (check (handler-case
             (progn (check-pipeline (list (counter :limit 1) (print-items) (print-items)))
                    nil)
           (pipeline-type-error () t))
         :nothing-may-follow-a-sink)
  ;; ...but a sink still accepts whatever is upstream, including :BYTES.
  (check (check-pipeline (list (from-list '(1)) (to-text) (print-items)))
         :a-sink-still-consumes-anything))

(defun test-redirection ()
  (let ((path "/tmp/plumb-redirect-test.txt"))
    (ignore-errors (delete-file path))
    (with-timeout (10 :redirection)
      ;; > and >> bind to the whole pipeline, the way a shell means them.
      (check (equal `(list (counter :limit 2) (to-file ,path))
                    (read-shell (format nil "counter :limit 2 > ~a" path)))
             :output-redirection)
      (check (equal `(list (counter :limit 2) (to-file ,path :if-exists :append))
                    (read-shell (format nil "counter :limit 2 >> ~a" path)))
             :append-redirection)
      (check (equal `(list (from-file ,path) (take 1))
                    (read-shell (format nil "< ~a | take 1" path)))
             :input-redirection)
      ;; A > inside a block is greater-than: blocks are scanned whole, so the
      ;; redirection pass never sees inside one.
      (check (equal '(where ($ (> (fld :size) 1024)))
                    (read-shell "where {(> .size 1kb)}"))
             :greater-than-inside-a-block-survives)
      ;; End to end.
      (join (run (eval (read-shell (format nil "counter :limit 3 > ~a" path)))))
      (check (equal '("0" "1" "2")
                    (mapcar #'line-text (collect-pipeline (list (from-file path)))))
             :round-trip)
      (join (run (eval (read-shell (format nil "counter :limit 1 >> ~a" path)))))
      (check (= 4 (length (collect-pipeline (list (from-file path))))) :append-appends)
      (ignore-errors (delete-file path)))))

(defun test-history-persists ()
  (let ((path "/tmp/plumb-history-test.txt"))
    (ignore-errors (delete-file path))
    (let ((ple:*history-file* path)
          (ple:*history* (make-array 0 :adjustable t :fill-pointer 0)))
      (ple:add-history "(+ 1 2)")
      (ple:add-history "ls | take 3")
      ;; A new session starts from an empty vector and reads the file back.
      (setf (fill-pointer ple:*history*) 0)
      (ple:load-history path)
      (check (equal '("(+ 1 2)" "ls | take 3") (coerce ple:*history* 'list))
             :history-survives-a-restart)
      ;; Appending rather than rewriting means a crash keeps what was typed,
      ;; and two sessions interleave instead of clobbering each other.
      (ple:add-history "counter")
      (setf (fill-pointer ple:*history*) 0)
      (ple:load-history path)
      (check (= 3 (length ple:*history*)) :appends-rather-than-rewrites))
    (ignore-errors (delete-file path)))
  ;; A missing file is not an error: losing history never justifies failing.
  (check (eql 0 (let ((ple:*history* (make-array 0 :adjustable t :fill-pointer 0)))
                  (ple:load-history "/tmp/plumb-no-such-history")))
         :missing-history-file-is-fine))

(defun test-completion ()
  (check (string= "to-" (ple::common-prefix '("to-text" "to-sh" "to-file"))) :common-prefix)
  (check (string= "take" (ple::common-prefix '("take"))) :single-candidate)
  (check (string= "" (ple::common-prefix '("take" "where"))) :nothing-in-common)
  ;; The completer reads the same two places HELP does, so they stay in step.
  (let ((hits (plumb.cli::plumb-completions "to-")))
    (check (member "to-text" hits :test #'string=) :completes-stage-names)
    (check (member "to-file" hits :test #'string=) :completes-new-stages))
  (check (member "*default-capacity*" (plumb.cli::plumb-completions "*def") :test #'string=)
         :completes-variables-too)
  (check (null (plumb.cli::plumb-completions "zzzznope")) :no-match-is-empty))

(defun with-awkward-directory (function)
  "A directory holding the file types that used to break LS: a FIFO it hung
on, a file it could not open, a symlink, and an executable."
  (let ((dir "/tmp/plumb-ls-test/"))
    (flet ((sh (command)
             (sb-ext:run-program "/bin/sh" (list "-c" command) :search nil :wait t)))
      (unwind-protect
           (progn
             (sh (format nil "rm -rf ~a; mkdir -p ~a" dir dir))
             (sh (format nil "cd ~a && echo hello > reg && chmod +x reg && ~
ln -s reg link && mkfifo pipe && echo x > noread && chmod 000 noread" dir))
             (funcall function dir))
        (sh (format nil "chmod 644 ~anoread 2>/dev/null; rm -rf ~a" dir dir))))))

(defun test-alien-stat-layout-matches-sb-posix ()
  "The safety property behind src/stat.lisp.  Reading struct stat through
sb-alien means trusting a hand-written layout, so every field SB-POSIX also
knows is compared against it.  A wrong offset shows up here as a mismatch on a
value we know independently, rather than as plausible nonsense in the
nanosecond fields nothing else can check."
  (let* ((path "src/ansi.lisp")
         (mine (file-stat path))
         (theirs (sb-posix:lstat path)))
    (check mine :alien-lstat-succeeded)
    (when mine
      (check (= (fs-size mine) (sb-posix:stat-size theirs)) :size-agrees)
      (check (= (fs-mode mine) (sb-posix:stat-mode theirs)) :mode-agrees)
      (check (= (fs-ino mine) (sb-posix:stat-ino theirs)) :inode-agrees)
      (check (= (fs-uid mine) (sb-posix:stat-uid theirs)) :uid-agrees)
      (check (= (fs-gid mine) (sb-posix:stat-gid theirs)) :gid-agrees)
      (check (= (fs-nlink mine) (sb-posix:stat-nlink theirs)) :nlink-agrees)
      (check (= (fs-dev mine) (sb-posix:stat-dev theirs)) :dev-agrees)
      (check (= (fs-mtime mine) (universal-from-unix (sb-posix:stat-mtime theirs)))
             :mtime-agrees)
      (check (= (fs-atime mine) (universal-from-unix (sb-posix:stat-atime theirs)))
             :atime-agrees))))

(defun test-sub-second-timestamps ()
  "What SB-POSIX cannot reach: nanoseconds, st_blocks, and Darwin's birthtime."
  (with-timeout (20 :nanoseconds)
    (let ((entry (first (collect-pipeline (list (ls "src/ansi.lisp"))))))
      (dolist (nsec (list (file-entry-mtime-nsec entry)
                          (file-entry-atime-nsec entry)
                          (file-entry-ctime-nsec entry)))
        (check (and (integerp nsec) (<= 0 nsec 999999999)) :nanoseconds-are-in-range))
      ;; MTIME itself stays whole seconds, so comparing against
      ;; GET-UNIVERSAL-TIME and DECODE-UNIVERSAL-TIME both still work.
      (check (integerp (file-entry-mtime entry)) :mtime-is-still-whole-seconds)
      (check (plusp (file-entry-blocks entry)) :blocks-allocated)
      (check (plusp (file-entry-blksize entry)) :block-size)
      ;; A file cannot have been created after it was last written.
      (check (<= (file-entry-birthtime entry) (file-entry-mtime entry)) :birthtime))
    ;; PRECISE-TIME is an exact rational: a double cannot hold a universal time
    ;; to nanosecond resolution, so 1e-9 differences would vanish in a float.
    (check (rationalp (precise-time 3994773576 329129261)) :precise-time-is-exact)
    (check (= (precise-time 100 500000000) 201/2) :precise-time-value)
    (check (eql 100 (precise-time 100 nil)) :precise-time-tolerates-no-fraction)
    ;; The point of all this: files written inside one second get an order.
    (with-awkward-directory
      (lambda (dir)
        (let* ((entries (collect-pipeline (list (ls dir))))
               (stamps (mapcar (lambda (e) (precise-time (file-entry-mtime e)
                                                         (file-entry-mtime-nsec e)))
                               entries)))
          (check (= 1 (length (remove-duplicates (mapcar #'file-entry-mtime entries))))
                 :all-written-within-one-second)
          (check (> (length (remove-duplicates stamps)) 1)
                 :but-nanoseconds-tell-them-apart))))))

(defun test-ls-stats-rather-than-opens ()
  "LS used to call FILE-LENGTH on an open stream, which cost open+fstat+close
per file, lost the size of anything unreadable, and blocked forever on a FIFO.
One LSTAT does all of it and cannot block."
  (with-timeout (20 :ls-lstat)
    (with-awkward-directory
      (lambda (dir)
        (let* ((entries (collect-pipeline (list (ls dir))))
               (by-name (lambda (n) (find n entries :key #'file-entry-name :test #'string=))))
          (check (= 4 (length entries)) :listed-everything)
          ;; The regression that matters: a FIFO must not be opened.
          (let ((pipe (funcall by-name "pipe")))
            (check (eq :fifo (file-entry-type pipe)) :fifo-is-a-fifo)
            (check (eql 0 (file-entry-size pipe)) :fifo-has-a-size-not-a-hang))
          ;; A file we cannot open still has a size, because nothing opens it.
          (let ((noread (funcall by-name "noread")))
            (check (eql 2 (file-entry-size noread)) :unreadable-files-keep-their-size)
            (check (eq :file (file-entry-type noread)) :unreadable-files-have-a-type))
          ;; LSTAT, not STAT: the link itself is what GLOB put in the stream.
          (let ((link (funcall by-name "link")))
            (check (eq :symlink (file-entry-type link)) :symlink-is-not-followed)
            (check (string= "reg" (file-entry-target link)) :readlink-gives-the-target))
          (let ((reg (funcall by-name "reg")))
            (check (eql 6 (file-entry-size reg)) :size)
            (check (string= (mode-string (file-entry-mode reg) :file) "-rwxr-xr-x")
                   :mode-string-matches-ls-l)
            (check (eql 1 (file-entry-nlink reg)) :nlink)
            (check (integerp (file-entry-ino reg)) :inode)
            (check (stringp (file-entry-user reg)) :owner-name-was-looked-up)
            ;; MTIME stays a universal time, so pipelines comparing it against
            ;; GET-UNIVERSAL-TIME still work.
            (check (< (abs (- (file-entry-mtime reg) (get-universal-time))) 300)
                   :mtime-is-still-a-universal-time)))))))

;;; --------------------------------------------------- the word-mode reader
;;;
;;; READ-SHELL is a source-to-source pass, so most of it tests by comparing
;;; forms.  The grammar's decisions are recorded in CLAUDE.md, open work 5.

(defun test-reader-dispatch ()
  (check (shell-syntax-p "ls | take 3") :bare-word-is-word-mode)
  (check (shell-syntax-p "   ls") :leading-space-is-fine)
  (check (not (shell-syntax-p "(list (ls))")) :leading-paren-is-lisp)
  (check (not (shell-syntax-p "  (+ 1 2)")) :leading-paren-after-space)
  (check (not (shell-syntax-p "")) :empty-is-not-word-mode))

(defun test-reader-pipeline ()
  (check (equal '(list (ls) (take 3)) (read-shell "ls | take 3")) :two-stages)
  ;; A bare word is a string; a number is a number.
  (check (equal '(ls "src/") (read-shell "ls src/")) :bare-word-is-a-string)
  (check (equal '(take 3) (read-shell "take 3")) :numbers-are-numbers)
  (check (equal '(sh "df -h") (read-shell "sh \"df -h\"")) :quoted-string)
  ;; One stage takes no LIST wrapper: HELP returns no values, and wrapping it
  ;; would turn that into a printed NIL.
  (check (equal '(help) (read-shell "help")) :single-stage-unwrapped))

(defun test-reader-blocks ()
  (check (equal '(where ($ (> (fld :size) 1024)))
                (read-shell "where {(> .size 1024)}"))
         :block-becomes-a-dollar-lambda)
  ;; .name on its own is shorthand for the block {.name}.
  (check (equal '(sort-by ($ (fld :size))) (read-shell "sort-by .size"))
         :bare-field-accessor)
  ;; A trailing keyword is a flag, so it means T.
  (check (equal '(sort-by ($ (fld :size)) :desc t) (read-shell "sort-by .size :desc"))
         :trailing-keyword-means-t)
  (check (equal '(counter :limit 3) (read-shell "counter :limit 3"))
         :keyword-with-a-value-is-left-alone)
  ;; Accessors expand at any depth, and strings in the tree are untouched.
  (check (equal '(where ($ (or (fld :a) (search "." (fld :b)))))
                (read-shell "where {(or .a (search \".\" .b))}"))
         :nested-accessors-and-strings-intact))

(defun test-reader-variables-and-globs ()
  (check (equal '(take *default-capacity*) (read-shell "take *default-capacity*"))
         :earmuffed-argument-is-a-variable)
  (check (eq 'plumb:*default-capacity* (read-shell "*default-capacity*"))
         :a-lone-variable-is-a-value-not-a-stage)
  ;; Earmuffs need both ends, which is what keeps globs as strings.
  (check (equal '(ls "*.lisp") (read-shell "ls *.lisp")) :glob-stays-a-string)
  (check (equal '(ls "*") (read-shell "ls *")) :lone-star-stays-a-string)
  (check (equal '(take 5120) (read-shell "take 5kb")) :suffix-literal-as-an-argument))

(defun test-reader-suffix-literals ()
  ;; Sizes are binary, the way ls -h and du -h mean them.
  (check (equal '(take 1024) (read-shell "take 1kb")) :kb)
  (check (equal '(take 1024) (read-shell "take 1k")) :k)
  (check (equal '(take 1024) (read-shell "take 1KiB")) :case-insensitive)
  (check (equal '(take 1048576) (read-shell "take 1mb")) :mb)
  (check (equal '(take 1073741824) (read-shell "take 1gb")) :gb)
  ;; A fractional size that lands on a whole number stays an integer.
  (check (equal '(take 1536) (read-shell "take 1.5kb")) :fractional-size-is-an-integer)
  ;; Durations are seconds, so they compose with GET-UNIVERSAL-TIME.
  (check (equal '(take 60) (read-shell "take 1min")) :minutes-are-spelled-min)
  (check (equal '(take 3600) (read-shell "take 1h")) :hours)
  (check (equal '(take 86400) (read-shell "take 1d")) :days)
  ;; The documented case: inside a block, where 1kb arrives as a symbol.
  (check (equal '(where ($ (> (fld :size) 1024)))
                (read-shell "where {(> .size 1kb)}"))
         :suffix-inside-a-block)
  ;; A word that merely starts with a digit is not a literal, so the CL symbols
  ;; 1+ and 1- survive a block unharmed.
  (check (null (plumb::suffixed-number "1+")) :one-plus-is-a-symbol)
  (check (null (plumb::suffixed-number "x1k")) :must-start-with-a-numeral)
  (check (equal '(take "9zz") (read-shell "take 9zz")) :unknown-suffix-stays-a-string))

(defun test-reader-lisp-escape ()
  (check (equal '(list (ls) (xform #'identity) (take 2))
                (read-shell "ls | (xform #'identity) | take 2"))
         :lisp-segment-is-verbatim)
  (check (equal '(table :columns (list :name :size))
                (read-shell "table :columns (list :name :size)"))
         :lisp-argument-is-verbatim))

(defun test-reader-pipes-do-not-split-everything ()
  "Blocks, forms and strings may all contain a pipe, which is why splitting on
| cannot be a separate first pass."
  (check (equal '(sh "a | b") (read-shell "sh \"a | b\"")) :pipe-inside-a-string)
  (check (equal '(where ($ (search "|" (fld :text))))
                (read-shell "where {(search \"|\" .text)}"))
         :pipe-inside-a-block)
  (check (equal '(list (ls) (xform #'identity))
                (read-shell "ls | (xform #'identity)"))
         :pipe-still-splits-normally))

(defun test-reader-runs ()
  "End to end: the emitted form is just a form, so EVAL takes it from here."
  (with-timeout (10 :reader-runs)
    (check (equal '(1 3)
                  (collect-pipeline
                   (eval (read-shell "counter :limit 9 | where {(oddp it)} | take 2"))))
           :word-mode-pipeline-runs)
    (check (equal '("A" "B")
                  (collect-pipeline
                   (eval (read-shell "from-list (list \"a\" \"b\") | xform #'string-upcase"))))
           :mixed-word-and-lisp-runs)))

(defun test-explain ()
  "EXPLAIN draws a pipeline without running it -- constructing a stage spawns
nothing, so the metadata is all there while the pipeline is still inert."
  (with-timeout (10 :explain)
    (let ((out (help-output (explain (list (ls "src/") (take 3))))))
      (check (search "2 stages, 1 channel, 2 threads" out) :counts)
      (check (search "source" out) :kind-of-the-first-stage)
      (check (search "nothing → :objects" out) :type-signature)
      ;; DEFSTAGE records the values the constructor was called with.
      (check (search "pattern=\"src/\"" out) :argument-values)
      (check (search "n=3" out) :argument-values-2)
      (check (search "capacity 64" out) :channel-depth)
      (check (search "types check" out) :verdict))
    ;; An invalid pipeline still draws; the bad joint is marked in place, which
    ;; is the point when the mismatch is several stages in.
    (let ((out (help-output (explain (list (from-list '(1)) (to-text) (where #'evenp))))))
      (check (search "✗" out) :mismatch-marked-inline)
      (check (search "1 type error" out) :verdict-counts-problems)
      (check (search "where" out) :still-draws-past-the-error))
    ;; Barriers are declared metadata, not guessed.
    (check (search "barrier" (help-output (explain (list (counter) (sort-by #'identity)))))
           :barrier-is-shown)
    (check (not (search "barrier" (help-output (explain (list (counter) (take 1))))))
           :non-barriers-are-not)
    (check (search "Nothing to explain" (help-output (explain '()))) :empty-pipeline)
    ;; A lone stage is a pipeline of one.
    (check (search "1 stage," (help-output (explain (take 1)))) :single-stage)
    ;; A graph draws as the graph it is, unwired ports included.
    (let ((out (help-output (explain (list (counter :limit 1) (route #'evenp))
                                     :ports (list :yes (list (tally)))))))
      (check (search "2 named ports" out) :counts-ports)
      (check (search "yes (:objects) → tally" out) :draws-a-wired-branch)
      (check (search "discarded, no branch" out) :draws-an-unwired-port))))

(defun test-explain-reads-as-a-reserved-word ()
  "EXPLAIN wraps the whole pipeline, the way bash's `time` does."
  (check (equal '(explain (list (ls) (take 3))) (read-shell "explain ls | take 3"))
         :explain-wraps-the-pipeline)
  (check (equal '(explain (ls "src/")) (read-shell "explain ls src/"))
         :explain-wraps-a-single-stage)
  ;; Only as the first word, and only with something to wrap.
  (check (equal '(ls "explain") (read-shell "ls explain")) :not-reserved-elsewhere)
  (check (equal '(explain) (read-shell "explain")) :bare-explain-is-just-a-call))

(defun test-ps ()
  "Assumes a unix ps(1).  The numbers are checked for shape and unit rather
than value, since the process table changes under the test."
  (with-timeout (20 :ps)
    (let ((processes (collect-pipeline (list (ps)))))
      (check (> (length processes) 5) :emits-the-process-table)
      ;; This process must be in it.
      (let ((self (find (sb-posix:getpid) processes :key #'process-pid)))
        (check self :finds-itself)
        (when self
          (check (integerp (process-ppid self)) :ppid-is-an-integer)
          (check (stringp (process-user self)) :user-is-a-string)
          (check (realp (process-pcpu self)) :pcpu-is-a-number)
          ;; RSS is BYTES here, not the kilobytes ps prints: LS reports .size
          ;; in bytes, and one 10mb literal has to mean the same against both.
          (check (> (process-rss self) (* 4 1024 1024)) :rss-is-in-bytes)
          (check (zerop (mod (process-rss self) 1024)) :rss-came-from-kilobytes)
          ;; NAME is COMMAND's basename -- whatever binary is running the
          ;; suite, which is sbcl under `make test` and plumb under the binary.
          (check (and (plusp (length (process-name self)))
                      (not (find #\/ (process-name self))))
                 :name-is-a-basename)
          (check (search (process-name self) (process-command self))
                 :name-comes-from-command)))
      ;; FIELD works on it like any other object, so blocks and .accessors do.
      (check (every (lambda (p) (integerp (field p :pid))) processes) :fields-work)
      (check (member :rss (fields (first processes))) :fields-lists-the-columns))))

(defun test-output-is-serialised ()
  "Fan-out means several stages print at once, and a CL stream is not
thread-safe: without the lock two branches duplicated and dropped each other's
lines, differently on every run."
  (with-timeout (20 :output-lock)
    (let ((results '()))
      (dotimes (trial 5)
        (push (sort (with-output-to-string (out)
                      (join (run (list (counter :limit 6) (route #'evenp))
                                 :ports (list :yes (list (print-items :stream out))
                                              :no  (list (print-items :stream out))))))
                      #'char<)
              results))
      ;; Every run carries the same characters -- nothing duplicated or lost.
      (check (= 1 (length (remove-duplicates results :test #'string=)))
             :parallel-branches-do-not-corrupt-a-shared-stream)
      (check (= 6 (count #\Newline (first results))) :one-line-per-object))))

;;; ------------------------------------------------------------ presenting

(defun test-present ()
  (check (string= "abc" (present "abc")) :string-is-itself)
  (check (string= "hello" (present (make-line :text "hello" :number 1))) :line-is-its-text)
  (check (string= "a.lisp" (present (make-file-entry :name "a.lisp"))) :file-entry-is-its-name)
  (check (string= "src/" (present (make-file-entry :name "src" :dir-p t))) :directories-get-a-slash)
  (check (string= "src/" (present (make-file-entry :name "src" :type :directory))) :type-gives-a-slash-too)
  (check (string= "l@" (present (make-file-entry :name "l" :type :symlink))) :symlinks-get-an-at)
  (check (string= "p|" (present (make-file-entry :name "p" :type :fifo))) :fifos-get-a-bar)
  (check (string= "x*" (present (make-file-entry :name "x" :type :file
                                                 :mode sb-posix:s-ixusr)))
         :executables-get-a-star)
  ;; The one-object-one-line rule, against a hostile binding.  A stage thread
  ;; does not inherit this, which is exactly why PRESENT and not the caller.
  (let ((*print-pretty* t) (*print-right-margin* 10))
    (check (null (find #\Newline (present (make-file-entry :name "x" :path #p"/x"))))
           :never-more-than-one-line)))

(defun test-table ()
  (let* ((rows (list (list :name "a" :size 1) (list :name "bbbb" :size 22)))
         (out (with-output-to-string (s) (render-table rows :stream s)))
         (lines (with-input-from-string (in out)
                  (loop for l = (read-line in nil nil) while l collect l))))
    (check (= 3 (length lines)) :header-plus-a-row-each)
    (check (string= "name  size" (first lines)) :header-from-fields)
    ;; Numbers right-align, text left-aligns, and the column fits the widest.
    (check (string= "a        1" (second lines)) :numeric-column-right-aligned)
    (check (string= "bbbb    22" (third lines)) :width-is-the-widest-cell))
  ;; An object with no fields still tables, under a :VALUE column.
  (check (search "value" (with-output-to-string (s) (render-table '(1 2) :stream s)))
         :fieldless-objects-get-a-value-column)
  ;; NIL is absent, not the text "nil".
  (let ((out (with-output-to-string (s)
               (render-table (list (list :a 1 :b nil)) :stream s))))
    (check (not (search "nil" out)) :nil-cells-render-empty)))

(defun lines-of (text)
  (with-input-from-string (in text)
    (loop for line = (read-line in nil nil) while line collect line)))

(defun test-transposed-table ()
  "Field names down the left, each record growing rightward as its own column."
  (let* ((rows (list (list :name "a" :size 1) (list :name "bbbb" :size 22)))
         (lines (lines-of (with-output-to-string (s)
                            (render-table rows :stream s :transpose t)))))
    ;; One line per FIELD now, not per record.
    (check (= 2 (length lines)) :one-line-per-field)
    (check (string= "name  a  bbbb" (first lines)) :records-grow-rightward)
    (check (string= "size  1  22" (second lines)) :second-field-on-its-own-line)
    (check (string= "size  1  22" (second lines)) :widths-are-per-record-column))
  ;; Left-aligned throughout: a column holds one record, so its values are
  ;; heterogeneous and right-aligning some rows would read as ragged.  A record
  ;; whose widest value is wider than its number makes the padding visible.
  (let ((lines (lines-of (with-output-to-string (s)
                           (render-table (list (list :name "abcd" :size 1)
                                               (list :name "z" :size 9))
                                         :stream s :transpose t)))))
    (check (string= "name  abcd  z" (first lines)) :first-field)
    ;; Left: "1   ".  Were it right-aligned this would read "size     1  9".
    (check (string= "size  1     9" (second lines)) :values-are-left-aligned))
  ;; :COLUMNS selects and orders which fields appear as rows.
  (let ((lines (lines-of (with-output-to-string (s)
                           (render-table (list (list :a 1 :b 2 :c 3))
                                         :stream s :transpose t
                                         :columns (list :c :a))))))
    (check (equal '("c  3" "a  1") lines) :columns-select-and-order-the-rows))
  ;; MAX-WIDTH defaults to NIL here: transposing is how you go to read a long
  ;; value in full.  It still caps when asked.
  (let ((long (make-string 60 :initial-element #\x)))
    (check (search long (with-output-to-string (s)
                          (render-table (list (list :a long)) :stream s :transpose t)))
           :no-truncation-by-default)
    (check (search "…" (with-output-to-string (s)
                         (render-table (list (list :a long)) :stream s
                                       :transpose t :max-width 10)))
           :max-width-still-caps))
  ;; NIL is still absent rather than the word, and no rows lays out nothing.
  (check (not (search "nil" (with-output-to-string (s)
                              (render-table (list (list :a 1 :b nil))
                                            :stream s :transpose t))))
         :nil-cells-still-render-empty)
  (check (string= "" (with-output-to-string (s)
                       (render-table '() :stream s :transpose t)))
         :no-rows-prints-nothing)
  ;; The regression this change could most easily cause.
  (let ((rows (list (list :name "a" :size 1) (list :name "bbbb" :size 22))))
    (check (equal '("name  size" "a        1" "bbbb    22")
                  (lines-of (with-output-to-string (s) (render-table rows :stream s))))
           :untransposed-rendering-is-unchanged)))

(defun test-err-port-is-shared-by-every-stage ()
  "Every stage SENDs to one :err channel, so the first to finish must not close
it for the others.  A consumer draining live -- not after JOIN -- is the case
that catches it."
  (with-timeout (15 :err-refcount)
    (let ((seen 0))
      (dotimes (trial 20)
        (let* ((err (make-channel))
               (reader (sb-thread:make-thread
                        (lambda ()
                          (loop (multiple-value-bind (obj ok) (recv err)
                                  (declare (ignore obj))
                                  (if ok (incf seen) (return))))))))
          ;; Stage 1 finishes at once; stage 2 fails later.
          (join (run (list (from-list '(1))
                           (xform (lambda (n) (sleep 0.002) (/ n 0))))
                     :err err))
          (sb-thread:join-thread reader)))
      (check (= 20 seen) :live-reader-sees-every-condition))
    ;; And the count still reaches zero, so a live reader does terminate.
    (let ((err (make-channel)))
      (join (run (list (from-list '(1)) (xform #'identity)) :err err))
      (check (null (nth-value 1 (recv err))) :err-really-closes-at-the-end))))

(defun test-run-accepts-an-input-channel ()
  "RUN :INPUT is what lets one pipeline feed another, and so what TEE is on."
  (with-timeout (10 :run-input)
    (let* ((head (make-channel))
           (sink (make-channel))
           (pipe (run (list (xform #'1+)) :input head :sink sink)))
      (send head 41)
      (close-output head)
      (check (eql 42 (recv sink)) :input-channel-feeds-the-first-stage)
      (join pipe))))

(defun test-tee-fans-out ()
  (with-timeout (15 :tee)
    ;; Every branch sees everything, and the stream still goes onward.
    (let ((a '()) (b '()))
      (check (equal '(0 1 2)
                    (collect-pipeline
                     (list (counter :limit 3)
                           (tee (list (xform (lambda (x) (push x a) x)))
                                (list (xform (lambda (x) (push x b) x)))))))
             :passes-through)
      (check (equal '(0 1 2) (reverse a)) :first-branch-saw-everything)
      (check (equal '(0 1 2) (reverse b)) :second-branch-saw-everything))
    ;; A branch that stops early is dropped; the others carry on.  That
    ;; independence is the entire point of a fan-out.
    (let ((short '()) (whole '()))
      (collect-pipeline
       (list (counter :limit 6)
             (tee (list (take 2) (xform (lambda (x) (push x short) x)))
                  (list (xform (lambda (x) (push x whole) x))))))
      (check (equal '(0 1) (reverse short)) :short-branch-stopped)
      (check (equal '(0 1 2 3 4 5) (reverse whole)) :other-branch-unaffected))))

(defun test-tee-tears-down ()
  "A TAKE downstream of a TEE has to stop an infinite source through it."
  (with-timeout (15 :tee-teardown)
    (check (equal '(0 1 2)
                  (collect-pipeline (list (counter)
                                          (tee (list (xform #'identity)))
                                          (take 3))))
           :downstream-take-stops-everything)))

(defun test-tee-shares-objects ()
  "Objects are SHARED with the branches, not copied -- nothing can deep-copy an
arbitrary Lisp object correctly.  Identity is the deterministic way to say it:
a branch mutating a shared object is a data *race*, since TEE sends to the
branches and emits onward concurrently, so when the change lands is not
defined.  Copying is a stage when a branch needs one."
  (with-timeout (10 :tee-sharing)
    (let ((from-branch nil) (from-main nil))
      (collect-pipeline
       (list (from-list (list (make-file-entry :name "a" :size 1)))
             (tee (list (xform (lambda (e) (setf from-branch e) e))))
             (xform (lambda (e) (setf from-main e) e))))
      ;; TEE joins its branches on the way out, so both have run by now.
      (check (eq from-branch from-main) :branches-share-the-same-object))
    ;; A copy stage at the head of a branch is the documented fix.
    (let ((from-branch nil) (from-main nil))
      (collect-pipeline
       (list (from-list (list (make-file-entry :name "a" :size 1)))
             (tee (list (xform #'copy-file-entry)
                        (xform (lambda (e) (setf from-branch e) e))))
             (xform (lambda (e) (setf from-main e) e))))
      (check (not (eq from-branch from-main)) :a-copy-stage-isolates-a-branch)
      (check (equal "a" (file-entry-name from-branch)) :and-the-copy-is-faithful))))

(defun test-named-ports ()
  "RUN reads STAGE-PORTS and wires each declared port to its own branch.  That
is the graph builder; TEE is one stream to many, this is many streams out."
  (with-timeout (15 :named-ports)
    (let ((yes '()) (no '()))
      (join (run (list (counter :limit 6) (route #'evenp))
                 :ports (list :yes (list (xform (lambda (x) (push x yes) x)))
                              :no  (list (xform (lambda (x) (push x no) x))))))
      (check (equal '(0 2 4) (reverse yes)) :matching-objects-went-to-yes)
      (check (equal '(1 3 5) (reverse no)) :the-rest-went-to-no))
    ;; A declared port with no branch is discarded rather than missing: EMIT
    ;; succeeds and the objects go nowhere, instead of erroring in a thread.
    (let ((yes '()))
      (join (run (list (counter :limit 4) (route #'evenp))
                 :ports (list :yes (list (xform (lambda (x) (push x yes) x))))))
      (check (equal '(0 2) (reverse yes)) :unwired-port-is-discarded))
    ;; Branch failures are the pipeline's failures -- from outside there is one.
    (let ((pipe (run (list (counter :limit 3) (route #'evenp))
                     :ports (list :yes (list (xform (lambda (x) (declare (ignore x))
                                                      (error "boom"))))))))
      (check (plusp (length (join pipe))) :branch-failures-reach-the-parent))))

(defun test-named-port-types ()
  ":PRODUCES describes :OUT alone, so a named port carries its own type -- or
the graph would be untyped exactly where it branches."
  (check (eq :objects (port-type (route #'evenp) :yes)) :declared-port-type)
  (check (null (port-type (route #'evenp) :out)) :produces-still-describes-out)
  ;; A branch head that cannot take what the port carries is caught before any
  ;; thread starts, and the message names the port rather than quoting
  ;; :PRODUCES, which describes a different port.
  (let ((condition (handler-case
                       (progn (run (list (counter :limit 1) (route #'evenp))
                                   :ports (list :yes (list (counter))))
                              nil)
                     (pipeline-type-error (c) c))))
    (check condition :branch-type-mismatch-is-caught)
    (check (eq :yes (pipeline-type-error-port condition)) :the-error-knows-the-port)
    (check (search ":YES port carries :OBJECTS" (princ-to-string condition))
           :and-says-so)))

(defun test-try-emit ()
  "EMIT is strict, which is how TAKE stops an infinite source.  A routing stage
needs the opposite: one branch ending must leave the others running."
  (with-timeout (15 :try-emit)
    (let ((no '()))
      ;; The :YES branch takes 1 and stops; :NO must still see everything.
      (join (run (list (counter :limit 6) (route #'evenp))
                 :ports (list :yes (list (take 1))
                              :no  (list (xform (lambda (x) (push x no) x))))))
      (check (equal '(1 3 5) (reverse no)) :a-closed-branch-does-not-stop-the-rest))))

;;; --------------------------------------------------- external processes
;;;
;;; These shell out, so they assume a unix /bin/sh with echo, false, cat, yes.

(defun sh-text (stages)
  (mapcar #'line-text (collect-pipeline stages)))

(defun processes-matching (marker)
  "How many live processes mention MARKER.  The bracket keeps the pgrep command
line itself from matching."
  (let ((out (with-output-to-string (s)
               (sb-ext:run-program
                "/bin/sh"
                (list "-c" (format nil "pgrep -f '[~a]~a' | wc -l"
                                   (char marker 0) (subseq marker 1)))
                :output s :search nil))))
    (or (parse-integer out :junk-allowed t) 0)))

(defun test-sh-source ()
  (with-timeout (10 :sh-source)
    (check (equal '("hi") (sh-text (list (sh "echo hi")))) :sh-stdout)
    ;; A list is exec'd directly: no shell sees the argument, so the run of
    ;; spaces survives instead of being re-split.
    (check (equal '("a b   c") (sh-text (list (sh (list "echo" "a b   c")))))
           :sh-argv-list-bypasses-the-shell)
    (check (equal '("one" "two") (sh-text (list (sh "printf 'one\\ntwo\\n'"))))
           :sh-multiple-lines)
    ;; LINE-SOURCE says which command produced the line.
    (check (equal '("echo hi")
                  (mapcar #'plumb::line-source (collect-pipeline (list (sh "echo hi")))))
           :sh-labels-its-lines)))

(defun test-sh-exit-status ()
  (with-timeout (10 :sh-exit)
    (let* ((err (make-channel))
           (pipe (run (list (sh "false")) :err err)))
      (join pipe)
      (close-output err)
      (let ((failures (pipeline-failures pipe))
            (from-port (recv err)))
        (check (= 1 (length failures)) :one-failure)
        (check (typep (cdr (first failures)) 'command-failed) :condition-type)
        (check (eql 1 (command-failed-exit-code (cdr (first failures)))) :exit-code)
        ;; The same live condition also travels out the :err port.
        (check (typep from-port 'command-failed) :reaches-err-port)))
    ;; :IGNORE means the exit code is simply not our business.
    (let ((pipe (run (list (sh "false" :on-exit :ignore)))))
      (join pipe)
      (check (null (pipeline-failures pipe)) :on-exit-ignore))
    ;; stderr is captured and hung on the condition.
    (let ((pipe (run (list (sh "ls /nonexistent-plumb-path" :stderr :capture)))))
      (join pipe)
      (let ((c (cdr (first (pipeline-failures pipe)))))
        (check (and (command-failed-stderr c)
                    (search "nonexistent" (command-failed-stderr c)))
               :stderr-attached)))))

(defun test-sh-teardown-kills-the-child ()
  "The reason this exists.  An endless command with a bounded consumer has to
stop, and the child has to be gone -- not merely reaped later by the OS when
the whole image exits."
  (with-timeout (15 :sh-teardown)
    (let ((marker "plumbteardownmarker"))
      (check (equal '("x" "x" "x")
                    (sh-text (list (sh (format nil "yes x # ~a" marker)) (take 3))))
             :take-against-an-endless-command)
      (sleep 0.3)
      (check (zerop (processes-matching marker)) :child-is-dead))))

(defun test-to-sh-sink ()
  (with-timeout (10 :to-sh)
    (let ((path "/tmp/plumb-to-sh-test.txt"))
      (ignore-errors (delete-file path))
      (join (run (list (from-list '("alpha" "beta" "gamma"))
                       (to-sh (format nil "cat > ~a" path)))))
      (check (equal '("alpha" "beta" "gamma")
                    (with-open-file (in path :if-does-not-exist nil)
                      (when in (loop for l = (read-line in nil nil) while l collect l))))
             :sink-wrote-every-object)
      (ignore-errors (delete-file path)))))

(defun test-lines-still-works ()
  "EMIT-LINES was factored out of LINES so SH could share it."
  (with-timeout (5 :lines)
    (check (equal '("a" "b")
                  (with-input-from-string (in "a
b")
                    (mapcar #'line-text (collect-pipeline (list (lines in))))))
           :lines-unchanged)))

;;; --------------------------------------------------------- line editing
;;;
;;; The editor's state machine is ordinary code over a struct, so it tests
;;; without a terminal.  Raw mode, key decoding and redisplay need a pty and
;;; are not covered here.

(defun edit (string point &rest ops)
  "Build an editor holding STRING with the cursor at POINT, apply OPS, and
return the resulting text and point."
  (let ((ed (plumb.lineedit::make-editor)))
    (plumb.lineedit::ed-insert ed string)
    (setf (plumb.lineedit::ed-point ed) point)
    (dolist (op ops) (funcall op ed))
    (values (plumb.lineedit::ed-string ed) (plumb.lineedit::ed-point ed))))

(defun test-editor-text-operations ()
  (check (string= "abXcd" (edit "abcd" 2 (lambda (e) (plumb.lineedit::ed-insert e "X"))))
         :insert-at-point)
  (check (string= "ad" (edit "abcd" 0 (lambda (e) (plumb.lineedit::ed-delete e 1 3))))
         :delete-range)
  ;; Point follows the text when the deletion is behind it.
  (check (eql 1 (nth-value 1 (edit "abcd" 3 (lambda (e) (plumb.lineedit::ed-delete e 1 3)))))
         :delete-moves-point)
  ;; C-t at end of line transposes the last two, as readline does.
  (check (string= "12" (edit "21" 2 (lambda (e)
                                      (let ((p (plumb.lineedit::ed-point e))
                                            (tx (plumb.lineedit::ed-text e)))
                                        (rotatef (char tx (- p 2)) (char tx (1- p)))))))
         :transpose))

(defun test-editor-word-motion ()
  ;; Symbol-aware, unlike readline: *default-capacity* is one word, not three.
  (check (eql 5 (nth-value 1 (edit "(foo *default-capacity*)" 23
                                   (lambda (e)
                                     (setf (plumb.lineedit::ed-point e)
                                           (plumb.lineedit::backward-word-pos e))))))
         :backward-word-is-symbol-aware)
  (check (eql 5 (nth-value 1 (edit "(+ 1 22)" 8
                                   (lambda (e)
                                     (setf (plumb.lineedit::ed-point e)
                                           (plumb.lineedit::backward-word-pos e))))))
         :backward-word)
  (check (eql 2 (nth-value 1 (edit "(+ 1 2)" 0
                                   (lambda (e)
                                     (setf (plumb.lineedit::ed-point e)
                                           (plumb.lineedit::forward-word-pos e))))))
         :forward-word))

(defun test-editor-kill-and-yank ()
  (let ((ed (plumb.lineedit::make-editor)))
    (plumb.lineedit::ed-insert ed "(+ 1 2)tail")
    (setf (plumb.lineedit::ed-point ed) 7)
    (plumb.lineedit::ed-delete ed 7 11 :kill t)
    (check (string= "(+ 1 2)" (plumb.lineedit::ed-string ed)) :kill-to-end)
    (check (string= "tail" (plumb.lineedit::ed-kill ed)) :kill-ring)
    (plumb.lineedit::ed-insert ed (plumb.lineedit::ed-kill ed))
    (check (string= "(+ 1 2)tail" (plumb.lineedit::ed-string ed)) :yank)))

(defun test-editor-history ()
  (let* ((h (make-array 2 :adjustable t :fill-pointer 2
                          :initial-contents '("(first)" "(second)")))
         (ed (plumb.lineedit::make-editor :history h)))
    (plumb.lineedit::ed-insert ed "live")
    (plumb.lineedit::history-move ed -1)
    (check (string= "(second)" (plumb.lineedit::ed-string ed)) :history-back)
    (plumb.lineedit::history-move ed -1)
    (check (string= "(first)" (plumb.lineedit::ed-string ed)) :history-back-twice)
    ;; Walking off the recent end restores the line that was being typed.
    (plumb.lineedit::history-move ed 1)
    (plumb.lineedit::history-move ed 1)
    (check (string= "live" (plumb.lineedit::ed-string ed)) :history-restores-live-line)))

(defun test-visible-width-ignores-colour ()
  (check (eql 3 (plumb.lineedit:visible-width
                 (let ((plumb.lineedit:*color* t)) (plumb.lineedit:paint "abc" :red))))
         :sgr-costs-no-columns)
  (check (eql 3 (plumb.lineedit:visible-width "abc")) :plain-width)
  (let ((plumb.lineedit:*color* nil))
    (check (string= "abc" (plumb.lineedit:paint "abc" :red)) :colour-off-is-plain)))

(defun test-incremental-read ()
  ;; The REPL leans on this to decide when to show a continuation prompt.
  (check (eq :incomplete (nth-value 1 (plumb.cli::try-read "(list (counter)"))) :incomplete)
  (check (eq :ok (nth-value 1 (plumb.cli::try-read "(list (counter))"))) :complete)
  (check (eq :error (nth-value 1 (plumb.cli::try-read ")"))) :reader-error)
  (check (= 2 (length (plumb.cli::try-read "(+ 1 2) (+ 3 4)"))) :two-forms-one-line)
  (check (null (plumb.cli::try-read "   ")) :blank-is-no-forms))

(defun test-sink-prints-one-line-per-object ()
  ;; A stage thread does not inherit the caller's *PRINT-PRETTY*, so binding it
  ;; hostilely here is exactly the situation PRINT-ITEMS has to survive.
  (with-timeout (5 :sink-line-per-object)
    (let* ((rows (list (make-file-entry :name "a" :size 1 :path #p"/a")
                       (make-file-entry :name "b" :size 2 :path #p"/b")))
           (out (with-output-to-string (s)
                  (let ((*print-pretty* t) (*print-right-margin* 20))
                    (join (run (list (from-list rows) (print-items :stream s))))))))
      (check (= 2 (count #\Newline out)) :one-line-per-object))))

;;; --------------------------------------------------------------------- help

(defun stage-named (name) (gethash name plumb::*stages*))

(defun test-help-registry ()
  (check (= 23 (hash-table-count plumb::*stages*)) :every-stage-registered)
  (check (eq :source (plumb::stage-kind (stage-named 'counter))) :counter-is-a-source)
  (check (eq :transform (plumb::stage-kind (stage-named 'where))) :where-is-a-transform)
  (check (eq :sink (plumb::stage-kind (stage-named 'print-items))) :print-items-is-a-sink)
  ;; TO-TEXT produces :BYTES, which is still producing something.
  (check (eq :transform (plumb::stage-kind (stage-named 'to-text))) :to-text-is-a-transform)
  (check (loop for i being the hash-values of plumb::*stages*
               always (plumb::si-documentation i))
         :every-stage-has-a-docstring))

(defun test-help-listing ()
  (let ((out (help-output (help))))
    (check (search "counter" out) :lists-sources)
    (check (search "print-items" out) :lists-sinks)
    (check (search "collect-pipeline" out) :lists-operators)
    (check (search "make-channel" out) :lists-channel-operators))
  ;; No values, so the CLI prints no stray NIL after (help).
  (let ((values :unset))
    (help-output (setf values (multiple-value-list (help))))
    (check (null values) :help-returns-no-values)))

(defun test-help-detail ()
  (let ((out (help-output (help take))))
    (check (search "(take n)" out) :usage-line)
    (check (search "(integer 0)" out) :parameter-type)
    (check (search ":objects" out) :type-signature)
    (check (search "stop the whole upstream" out) :docstring))
  ;; A string names the same thing as a symbol.
  (check (string= (help-output (help take)) (help-output (help "take"))) :string-designator)
  ;; Non-stage built-ins come from SB-INTROSPECT instead of the registry.
  (let ((out (help-output (help run))))
    (check (search "&key" out) :operator-lambda-list)
    (check (search "Wire STAGES" out) :operator-docstring))
  (let ((out (help-output (help emit))))
    (check (search "(macro)" out) :macro-is-labelled)))

(defun test-help-variables ()
  (let ((out (help-output (help *default-capacity*))))
    (check (search "(variable)" out) :variable-is-labelled)
    (check (search "value 64" out) :variable-shows-its-value)
    (check (search "only knob" out) :variable-docstring))
  (check (search "*default-capacity*" (help-output (help))) :variables-are-listed))

(defun test-help-speaks-only-for-plumb ()
  ;; FIND-SYMBOL sees everything PLUMB inherits from CL.  HELP must not, or the
  ;; listing and the detail disagree about what a built-in is.
  (check (search "No built-in named" (help-output (help list))) :cl-function-rejected)
  (check (search "No built-in named" (help-output (help if))) :special-operator-rejected)
  (check (search "(take n)" (help-output (help take))) :plumb-symbol-still-resolves))

(defun test-prompt-designators ()
  "PLAIN-REPL and the editor resolve *PROMPT* through the same function."
  (check (string= "> " (ple:prompt-text "> ")) :string-prompt)
  (check (string= "x> " (ple:prompt-text (lambda () "x> "))) :function-prompt)
  (check (string= "" (ple:prompt-text nil)) :null-prompt)
  (check (stringp (ple:prompt-text 'plumb.cli::plumb-prompt)) :symbol-names-a-function))

(defun test-help-unknown ()
  (let ((out (help-output (help stage))))
    (check (search "No built-in named" out) :unknown-name)
    (check (search "defstage" out) :suggests-near-matches)))

;;; ------------------------------------------------------------------ main

(defun run-tests ()
  (let ((*passed* 0) (*failed* '()))
    (dolist (fn '(test-channel-basics
                  test-buffer-drains-before-eof
                  test-backpressure
                  test-close-input-wakes-producer
                  test-simple-pipeline
                  test-take-tears-down-infinite-source
                  test-teardown-cascades-through-many-stages
                  test-source-thread-actually-dies
                  test-collecting-stage
                  test-fields-and-block
                  test-error-is-recorded-and-tears-down
                  test-error-object-reaches-err-port
                  test-type-check
                  test-cancel
                  test-each-backpressure-end-to-end
                  test-sink-ends-the-pipeline
                  test-run-accepts-an-input-channel
                  test-tee-fans-out
                  test-tee-tears-down
                  test-tee-shares-objects
                  test-named-ports
                  test-named-port-types
                  test-try-emit
                  test-redirection
                  test-history-persists
                  test-completion
                  test-glob
                  test-ls-stats-rather-than-opens
                  test-alien-stat-layout-matches-sb-posix
                  test-sub-second-timestamps
                  test-reader-dispatch
                  test-reader-pipeline
                  test-reader-blocks
                  test-reader-variables-and-globs
                  test-reader-suffix-literals
                  test-reader-lisp-escape
                  test-reader-pipes-do-not-split-everything
                  test-reader-runs
                  test-explain
                  test-explain-reads-as-a-reserved-word
                  test-ps
                  test-output-is-serialised
                  test-present
                  test-table
                  test-transposed-table
                  test-err-port-is-shared-by-every-stage
                  test-sh-source
                  test-sh-exit-status
                  test-sh-teardown-kills-the-child
                  test-to-sh-sink
                  test-lines-still-works
                  test-editor-text-operations
                  test-editor-word-motion
                  test-editor-kill-and-yank
                  test-editor-history
                  test-visible-width-ignores-colour
                  test-incremental-read
                  test-sink-prints-one-line-per-object
                  test-help-registry
                  test-help-listing
                  test-help-detail
                  test-help-unknown
                  test-help-variables
                  test-help-speaks-only-for-plumb
                  test-prompt-designators))
      (format t "~&; ~a~%" fn)
      (funcall fn))
    (format t "~&~%~d passed, ~d failed~%" *passed* (length *failed*))
    (dolist (f (reverse *failed*))
      (format t "  FAIL: ~s~%" f))
    (null *failed*)))
