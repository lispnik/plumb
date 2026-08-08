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

(defun split-lines (text)
  (let ((lines '()) (start 0))
    (loop for pos = (position #\Newline text :start start)
          do (push (subseq text start (or pos (length text))) lines)
             (if pos (setf start (1+ pos)) (return)))
    (remove "" (nreverse lines) :test #'string=)))

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
      (check (notany #'task-live-p (pipeline-tasks pipe))
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
      (check (notany #'task-live-p (pipeline-tasks pipe))
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
      (check (notany #'task-live-p (pipeline-tasks pipe)) :cancelled))))

(defun test-each-backpressure-end-to-end ()
  "A slow consumer must not let the source race ahead unboundedly."
  (with-timeout (10 :each)
    (let ((seen 0) (max-seen 0))
      (each (list (counter :limit 50) (take 20))
            (lambda (x) x (incf seen) (setf max-seen seen)))
      (check (= 20 max-seen) :each-count))))

(defun names (paths) (mapcar #'file-namestring paths))

(defun try-glob (pattern name) (and (glob-match pattern name) t))

(defun test-glob-posix-classes ()
  "POSIX bracket expressions: character classes, collating symbols and
equivalence classes.  The last two degenerate to the literal character, there
being no collating locale here."
  (check (try-glob "a[[:digit:]].txt" "a1.txt") :digit-class)
  (check (not (try-glob "a[[:digit:]].txt" "ab.txt")) :digit-class-excludes)
  (check (try-glob "a[[:alpha:]].txt" "ab.txt") :alpha-class)
  (check (try-glob "[[:upper:]]*" "Abc") :upper-class)
  (check (not (try-glob "[[:upper:]]*" "abc")) :upper-class-excludes)
  (check (try-glob "[![:digit:]]*" "abc") :negated-class)
  (check (not (try-glob "[![:digit:]]*" "1bc")) :negated-class-excludes)
  (check (try-glob "[[:xdigit:]]*" "fed") :xdigit-class)
  (check (try-glob "[[:space:]]" " ") :space-class)
  (check (try-glob "[[:punct:]]" "!") :punct-class)
  (check (try-glob "[[.a.]]bc" "abc") :collating-symbol)
  (check (try-glob "[[=a=]]bc" "abc") :equivalence-class)
  ;; The extensions are gone, so these are literal text and match nothing.
  (check (not (try-glob "(a|b).txt" "a.txt")) :no-alternation)
  (check (not (try-glob "!(a).txt" "b.txt")) :no-extglob)
  (check (not (try-glob "ab#c" "abbbc")) :no-closure)
  (check (not (try-glob "^*.lisp" "a.txt")) :no-caret-negation)
  (check (not (try-glob "<1-9>" "5")) :no-numeric-ranges)
  ;; An unknown class is its characters, and an unterminated [ is literal.
  (check (try-glob "[abc" "[abc") :unterminated-set-is-literal))

(defun test-glob-matching ()
  "The matcher itself.  CL pathname patterns got three of these silently
wrong, which is why globbing no longer goes through them."
  ;; Ranges.  [a-c] used to be the literal set {a,-,c}, so it skipped b.
  (check (glob-match "[a-c].txt" "b.txt") :ranges)
  (check (not (glob-match "[a-c].txt" "d.txt")) :ranges-exclude)
  ;; Negation.  [!a] used to be the set {!,a}, so it matched the opposite.
  (check (glob-match "[!a]*.txt" "b.txt") :negation-with-bang)
  (check (not (glob-match "[!a]*.txt" "a.txt")) :negation-actually-negates)
  (check (glob-match "[^ab]x.txt" "cx.txt") :negation-with-caret)
  ;; A ] first in the set is literal, as in every shell.
  (check (glob-match "[]]" "]") :closing-bracket-first-is-literal)
  ;; An unterminated [ is a literal [.
  (check (glob-match "[abc" "[abc") :unterminated-set-is-literal)
  ;; The leading-dot rule: * must not find a dotfile, as in a shell.
  (check (not (glob-match "*" ".hidden")) :star-skips-dotfiles)
  (check (not (glob-match "*.txt" ".a.txt")) :star-skips-dotfiles-with-a-type)
  (check (glob-match ".*" ".hidden") :an-explicit-dot-finds-them)
  ;; Escapes, which is how a name holding a metacharacter is written.
  (check (glob-match "star\\*.txt" "star*.txt") :escaped-star-is-literal)
  (check (not (glob-match "star\\*.txt" "starry.txt")) :escaped-star-does-not-glob)
  (check (glob-match "br\\[a\\].txt" "br[a].txt") :escaped-brackets)
  ;; Ordinary cases.
  (check (glob-match "*" "anything") :star)
  (check (glob-match "a*c" "abbbc") :star-in-the-middle)
  (check (glob-match "?.txt" "a.txt") :question-mark)
  (check (not (glob-match "?.txt" "ab.txt")) :question-mark-is-exactly-one)
  (check (glob-pattern-p "a*b") :pattern-detected)
  (check (not (glob-pattern-p "a\\*b")) :escaped-metacharacter-is-not-a-pattern))

(defun test-glob-finds-awkward-names ()
  "The bug this replaced: DIRECTORY handed back pathnames whose name held * as
a pattern object, FILE-NAMESTRING re-escaped it, LSTAT on the escaped path
failed, and LS dropped the entry without a word."
  (with-timeout (20 :awkward-names)
    (let ((dir "/tmp/plumb-glob-test/"))
      (unwind-protect
           (flet ((sh (c) (sb-ext:run-program "/bin/sh" (list "-c" c) :search nil :wait t)))
             (sh (format nil "rm -rf ~a; mkdir -p ~a" dir dir))
             (sh (format nil "cd ~a && : > 'star*.txt' && : > 'br[a].txt' && ~
: > plain.txt && : > .hidden" dir))
             (let ((names (mapcar #'file-entry-name (collect-pipeline (list (ls dir))))))
               ;; All four, with their real names -- none escaped, none missing.
               (check (member "star*.txt" names :test #'string=) :star-in-a-name-survives)
               (check (member "br[a].txt" names :test #'string=) :brackets-in-a-name-survive)
               (check (= 4 (length names)) :a-directory-listing-shows-everything))
             ;; A pattern, though, follows the shell dotfile rule.
             (let ((names (mapcar #'file-entry-name
                                  (collect-pipeline (list (ls (concatenate 'string dir "*")))))))
               (check (not (member ".hidden" names :test #'string=)) :a-pattern-skips-dotfiles)
               (check (member "star*.txt" names :test #'string=) :and-still-finds-awkward-names)))
        (sb-ext:run-program "/bin/sh" (list "-c" (format nil "rm -rf ~a" dir))
                            :search nil :wait t)))))

(defun test-ls-streams ()
  "LS emits as it walks rather than globbing the tree first, so a downstream
TAKE stops the walk.  Before this, take 3 over /usr/share cost 380ms -- the
same as listing all 15,732 entries."
  (with-timeout (25 :streaming)
    (let ((dir "/tmp/plumb-stream-test/"))
      (unwind-protect
           (flet ((sh (c) (sb-ext:run-program "/bin/sh" (list "-c" c) :search nil :wait t)))
             (sh (format nil "rm -rf ~a; mkdir -p ~a" dir dir))
             ;; Enough entries that visiting them all would be visible.
             (sh (format nil "cd ~a && for i in $(seq 1 300); do : > f$i.txt; done" dir))
             ;; MAP-GLOB stops when its callback transfers control out, which
             ;; is what EMIT signalling CHANNEL-CLOSED does.
             (let ((visited 0))
               (block early
                 (map-glob (concatenate 'string dir "*")
                           (lambda (path)
                             (declare (ignore path))
                             (incf visited)
                             (when (= visited 3) (return-from early)))))
               (check (= 3 visited) :the-walk-stops-when-the-caller-does))
             ;; End to end: TAKE gets three without the stage seeing 300.
             (let ((seen 0))
               (check (= 3 (length (collect-pipeline
                                    (list (ls (concatenate 'string dir "*"))
                                          (xform (lambda (x) (incf seen) x))
                                          (take 3)))))
                      :take-three-from-three-hundred)
               ;; A channel of capacity 64 may run ahead, but nothing like 300.
               (check (< seen 100) :the-source-did-not-walk-everything)))
        (sb-ext:run-program "/bin/sh" (list "-c" (format nil "rm -rf ~a" dir))
                            :search nil :wait t)))))

(defun test-glob-order-is-depth-first ()
  "Ordering now comes from sorting each directory as the walk reaches it, since
a streamed result has no end at which to sort.

** interleaves its two cases -- the rest of the pattern starting here, and **
consuming this level -- per entry.  Running them one after the other emits
every sibling before descending into any, which a final sort used to hide."
  (with-timeout (25 :ordering)
    (let ((dir "/tmp/plumb-order-test/"))
      (unwind-protect
           (flet ((sh (c) (sb-ext:run-program "/bin/sh" (list "-c" c) :search nil :wait t))
                  (names (pattern)
                    (mapcar (lambda (p) (basename (sb-ext:native-namestring p)))
                            (glob pattern))))
             (sh (format nil "rm -rf ~a; mkdir -p ~aa1 ~aa2" dir dir dir))
             (sh (format nil "cd ~a && : > b.txt && : > a1/x.txt && : > a2/y.txt" dir))
             ;; Depth first: a1 and its contents before a2, not a1 a2 then both.
             (check (equal '("a1" "x.txt" "a2" "y.txt" "b.txt")
                           (names (concatenate 'string dir "**/*")))
                    :double-star-is-depth-first)
             ;; LS and GLOB cannot disagree: both come from MAP-GLOB.
             (check (equal (names (concatenate 'string dir "**/*"))
                           (mapcar #'file-entry-name
                                   (collect-pipeline
                                    (list (ls (concatenate 'string dir "**/*"))))))
                    :ls-and-glob-agree))
        (sb-ext:run-program "/bin/sh" (list "-c" (format nil "rm -rf ~a" dir))
                            :search nil :wait t)))))

(defun test-glob-and-symlinks ()
  "Descending a named component follows symlinks, as a shell does; ** does not,
so a link pointing back up cannot recurse forever.  /tmp is itself a symlink on
macOS, so getting the first half wrong made every pattern under it match
nothing."
  (with-timeout (25 :glob-symlinks)
    (let ((dir "/tmp/plumb-glob-link-test/"))
      (unwind-protect
           (flet ((sh (c) (sb-ext:run-program "/bin/sh" (list "-c" c) :search nil :wait t)))
             (sh (format nil "rm -rf ~a; mkdir -p ~asub" dir dir))
             (sh (format nil "cd ~a && : > top.txt && : > sub/deep.txt && ln -s .. sub/loop"
                         dir))
             ;; /tmp is a symlink; a pattern under it must still match.
             (check (glob (concatenate 'string dir "*.txt")) :descends-through-a-symlinked-parent)
             ;; ** must terminate despite sub/loop pointing back up.
             (let ((names (mapcar (lambda (p) (basename (sb-ext:native-namestring p)))
                                  (glob (concatenate 'string dir "**/*.txt")))))
               (check (member "top.txt" names :test #'string=) :double-star-finds-the-top)
               (check (member "deep.txt" names :test #'string=) :double-star-descends)
               ;; Each file once: following the loop would repeat them.
               (check (= (length names) (length (remove-duplicates names :test #'string=)))
                      :double-star-does-not-follow-a-loop)))
        (sb-ext:run-program "/bin/sh" (list "-c" (format nil "rm -rf ~a" dir))
                            :search nil :wait t)))))

(defun test-glob ()
  "GLOB backs LS.  Run from the project root, which the test suite is."
  (with-timeout (10 :glob)
    (check (member "channel.lisp" (names (glob "src/*.lisp")) :test #'string=)
           :star-matches-by-type)
    (check (member "field.lisp" (names (glob "src/?ield.lisp")) :test #'string=)
           :question-mark-is-one-character)
    ;; The property, not a census of src/: this used to assert a count, and
    ;; adding one source file to the project failed a globbing test.
    (let ((matched (names (glob "src/[cf]*.lisp"))))
      (check (member "channel.lisp" matched :test #'string=) :character-class-first-branch)
      (check (member "field.lisp" matched :test #'string=) :character-class-second-branch)
      (check (every (lambda (n) (find (char n 0) "cf")) matched) :character-class-excludes)
      (check (notany (lambda (n) (string= n "stage.lisp")) matched) :character-class-is-a-class))
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
             (sh (format nil "cd ~a && echo hello > reg && chmod 755 reg && ~
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
  "What SB-POSIX cannot reach: nanoseconds, st_blocks, and a birth time --
Darwin's st_birthtime, and on Linux STATX_BTIME, which struct stat has no field
for at all."
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
      ;; Birth time is a real timestamp.  NOT birthtime <= mtime, which this
      ;; asserted and which is simply false for a *copied* file: cp -p and
      ;; rsync -a create a new file now and put the old mtime back on it, so
      ;; the birth time is legitimately later.  A checkout is a copy.
      (check (integerp (file-entry-birthtime entry)) :birthtime-is-a-timestamp)
      (check (< (encode-universal-time 0 0 0 1 1 1990 0)
                (file-entry-birthtime entry)
                (+ (get-universal-time) 86400))
             :birthtime-is-plausible))
    ;; The ordering does hold for a file created in place, which is the only
    ;; case where "created before last written" is guaranteed -- so test it
    ;; there rather than on whatever the working tree happens to contain.
    (let ((path "/tmp/plumb-birthtime-test"))
      (unwind-protect
           (with-timeout (10 :birthtime-ordering)
             (with-open-file (out path :direction :output :if-exists :supersede)
               (write-line "hello" out))
             (let ((fresh (first (collect-pipeline (list (ls path))))))
               (check (<= (file-entry-birthtime fresh) (file-entry-mtime fresh))
                      :created-no-later-than-last-written)
               (check (<= (abs (- (file-entry-birthtime fresh) (get-universal-time))) 60)
                      :and-it-was-just-now)))
        (ignore-errors (delete-file path))))
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

(defun test-a-keyword-in-a-required-position-is-a-value ()
  "The flag rule -- a keyword with nothing after it means :KEY T -- has to stop
at the required arguments, or a stage whose first argument IS a keyword gets an
odd number of &KEY arguments.  (digest :sha256) in plumb/crypto is why; SORT-BY
is the core stage that can show it, since its KEY is declared (or function
symbol) and a keyword is a symbol."
  (check (equal '(sort-by :size) (read-shell "sort-by :size"))
         :required-keyword-keeps-its-value)
  ;; Past the required arguments the rule still applies, in the same call.
  (check (equal '(sort-by :size :desc t) (read-shell "sort-by :size :desc"))
         :flags-after-a-required-keyword)
  ;; And a stage with no required arguments is unaffected.
  (check (equal '(table :transpose t) (read-shell "table :transpose"))
         :flag-on-a-stage-with-no-required-arguments)
  (check (= 1 (plumb::required-argument-count 'take)) :one-required-argument)
  (check (= 2 (plumb::required-argument-count 'accumulate)) :two-required-arguments)
  (check (= 0 (plumb::required-argument-count 'table)) :no-required-arguments)
  (check (= 0 (plumb::required-argument-count 'tee)) :rest-args-are-not-required)
  ;; A word that names no stage keeps the rule it always had.
  (check (= 0 (plumb::required-argument-count 'no-such-stage)) :unknown-word-is-zero))

(defun test-a-leading-comment-is-still-lisp ()
  "SHELL-SYNTAX-P trimmed whitespace only, so a file opening with a ;;;; banner
-- which every Lisp file does -- was handed to the WORD reader.  `plumb -f
stages.lisp` then died with an end-of-file inside a string instead of loading
the file, which is the one thing -f exists for."
  (check (not (shell-syntax-p (format nil ";; a comment~%(foo)"))) :line-comment-then-lisp)
  (check (not (shell-syntax-p (format nil ";;;; banner~%;;;;~%(defstage x ())")))
         :several-comment-lines)
  (check (not (shell-syntax-p "#|block|# (foo)")) :block-comment)
  (check (not (shell-syntax-p ";; nothing but a comment")) :comment-only-is-not-word-mode)
  ;; And none of that may capture ordinary word mode.
  (check (shell-syntax-p "ls | take 5") :a-pipeline-is-still-word-mode)
  (check (shell-syntax-p "  ls src/") :leading-space-is-still-word-mode)
  ;; # is NOT always a comment: #'f and #(1 2) are word-mode tokens.
  (check (shell-syntax-p "#'oddp") :sharp-quote-stays-word-mode)
  (check (shell-syntax-p "#(1 2 3)") :sharp-vector-stays-word-mode)
  ;; A comment above a WORD-mode pipeline leaves it word mode: the comment is
  ;; skipped, then the ordinary leading-paren rule applies to what follows.
  (check (shell-syntax-p (format nil ";; comment~%ls src/"))
         :a-comment-above-a-pipeline-is-still-word-mode))

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

;;; ------------------------------------------------------------- sh-filter
;;;
;;; The stage that runs a thread, so the tests that matter are the ones about
;;; the two ways that can hang: a pipe buffer with nobody draining it, and a
;;; feeder left parked when the stage has gone.  Every one is time-boxed, since
;;; the failure mode here is a hang rather than a wrong answer.

(defun test-stderr-files-are-private ()
  "A captured stderr is attached to COMMAND-FAILED, so sharing the file between
two commands makes one report the other's diagnostics.  The old name came from
RANDOM against SBCL's initial *RANDOM-STATE*, which is identical in every
image, so separate processes generated the same one."
  (with-timeout (10 :stderr-file)
    (let ((a (plumb::%stderr-file))
          (b (plumb::%stderr-file)))
      (unwind-protect
           (progn
             (check (not (equal a b)) :successive-calls-differ)
             (check (probe-file a) :created-not-merely-named)
             ;; The name is not evidence of a free file.  Pids are recycled and
             ;; a hard kill leaves files behind, so an existing one has to be
             ;; refused rather than adopted -- which is the property the
             ;; exclusive create buys and a random name never did.
             (check (null (open a :direction :output :if-exists nil
                                  :if-does-not-exist :create))
                    :an-existing-file-is-refused))
        (ignore-errors (delete-file a))
        (ignore-errors (delete-file b))))
    ;; End to end: the condition carries this command's stderr.
    (let* ((noise (make-string-output-stream))
           (pipe (let ((*error-output* noise))
                   (let ((p (run (list (sh "sh -c 'echo mine >&2; exit 1'")))))
                     (join p)
                     p)))
           (failure (cdr (first (pipeline-failures pipe)))))
      (check (search "mine" (or (command-failed-stderr failure) ""))
             :stderr-belongs-to-its-own-command))))

(defun test-sh-filter-round-trip ()
  (with-timeout (15 :sh-filter)
    (check (equal '("ALPHA" "BETA")
                  (sh-text (list (from-list '("alpha" "beta"))
                                 (sh-filter "tr a-z A-Z"))))
           :objects-through-a-command-and-back)
    ;; A list is exec'd directly here too, with no shell to re-split it.
    (check (equal '("a b   c")
                  (sh-text (list (from-list '("a b   c")) (sh-filter (list "cat")))))
           :argv-list-bypasses-the-shell)
    ;; PRESENT, not PRINC-TO-STRING: the command sees the line PRINT-ITEMS
    ;; would have shown.  :AS overrides it.
    (check (equal '("XX")
                  (sh-text (list (from-list '("ignored"))
                                 (sh-filter "cat" :as (lambda (x) (declare (ignore x)) "XX")))))
           :as-overrides-the-rendering)))

(defun test-sh-filter-needs-stdin-eof ()
  "A command that produces nothing until stdin closes.  If the feeder did not
close it, this hangs -- which is why the close is on both of its exit paths."
  (with-timeout (15 :sh-filter-eof)
    (check (equal '("3")
                  (sh-text (list (from-list '("a" "b" "c"))
                                 (sh-filter "wc -l | tr -d ' '"))))
           :wc-l-terminated)
    ;; ...and a command that reorders, so it cannot emit before the last line.
    (check (equal '("a" "b" "c")
                  (sh-text (list (from-list '("c" "a" "b")) (sh-filter "sort"))))
           :sort-is-a-barrier-and-still-works)))

(defun test-sh-filter-survives-a-full-pipe ()
  "THE test.  More data than a pipe buffer holds in either direction: writing
it all before reading would fill the kernel's 64K and stop both sides for good.
This passing is the only evidence the feeder thread is doing its job."
  (with-timeout (60 :sh-filter-large)
    (let* ((n 20000)
           (objects (loop for i from 1 to n collect (format nil "line-~d-padded-out-to-some-width" i)))
           (out (sh-text (list (from-list objects) (sh-filter "cat")))))
      (check (= n (length out)) :every-line-survived)
      (check (equal (first objects) (first out)) :first-line-intact)
      (check (equal (car (last objects)) (car (last out))) :last-line-intact))))

(defun test-sh-filter-teardown ()
  "A bounded consumer in front of an endless source, with a child in between.
Both the child and the feeder have to stop, and the pipeline has to return."
  (with-timeout (30 :sh-filter-teardown)
    (let ((marker "plumbfiltermarker"))
      (check (equal '("1" "2" "3")
                    (sh-text (list (counter :from 1)
                                   (sh-filter (format nil "cat # ~a" marker))
                                   (take 3))))
             :take-stops-an-endless-filter)
      (sleep 0.3)
      (check (zerop (processes-matching marker)) :child-is-dead))
    ;; The other early exit: the CHILD stops first, while the source is still
    ;; producing.  The feeder must not be left writing into a dead pipe.
    (check (equal '("1")
                  (sh-text (list (counter :from 1)
                                 (sh-filter "head -1" :on-exit :ignore))))
           :child-exiting-early-still-returns)))

(defun test-sh-filter-feeder-errors-are-not-swallowed ()
  "The feeder runs on its own thread, which is exactly how a failure there
escaped SPAWN-STAGE's handler and became a short answer with exit 0.  Both
halves matter: a real error must be reported, and the broken pipe a TAKE causes
must NOT be, or every bounded filter would report a spurious failure."
  (with-timeout (20 :sh-filter-feeder-errors)
    ;; An error inside :AS truncates what the command sees.  That must not look
    ;; like a complete run.
    (multiple-value-bind (out failures)
        (let ((n 0))
          (collect-pipeline
           (list (from-list '("a" "b" "c" "d" "e"))
                 (sh-filter "cat" :as (lambda (x) (incf n) (if (> n 2) (error "boom") x))))
           :errorp nil))
      (declare (ignorable out))
      (check (= 1 (length failures)) :throwing-as-is-recorded)
      (check (eq 'sh-filter (car (first failures))) :recorded-against-the-stage))
    ;; ...and with the default :ERRORP it reaches the caller.
    (check (eq :signalled
               (handler-case
                   (collect-pipeline (list (from-list '("a"))
                                           (sh-filter "cat" :as (lambda (x)
                                                                  (declare (ignore x))
                                                                  (error "boom")))))
                 (pipeline-error () :signalled)))
           :throwing-as-signals)
    ;; A value WRITE-LINE cannot take is the same failure by another route --
    ;; this one silently produced NOTHING AT ALL and reported success.
    (multiple-value-bind (out failures)
        (collect-pipeline (list (from-list '(1 2 3))
                                (sh-filter "cat" :as #'identity))
                          :errorp nil)
      (declare (ignorable out))
      (check (= 1 (length failures)) :non-string-from-as-is-recorded))
    ;; The other half.  A bounded consumer breaks the child's pipe under the
    ;; feeder, and SB-INT:BROKEN-PIPE is a STREAM-ERROR: expected traffic, not
    ;; a fault.  Nothing may be recorded here.
    (let* ((sink (make-channel :capacity 4))
           (pipe (run (list (counter :from 1) (sh-filter "cat") (take 3)) :sink sink)))
      (loop (multiple-value-bind (obj ok) (recv sink)
              (declare (ignore obj))
              (unless ok (return))))
      (close-input sink)
      (join pipe)
      (check (null (pipeline-failures pipe)) :take-path-reports-no-failure))))

(defun test-sh-filter-exit-status ()
  (with-timeout (15 :sh-filter-exit)
    (let ((pipe (run (list (from-list '("x")) (sh-filter "cat >/dev/null; exit 3")))))
      (join pipe)
      (let ((failure (cdr (first (pipeline-failures pipe)))))
        (check (typep failure 'command-failed) :non-zero-exit-is-a-condition)
        (check (eql 3 (command-failed-exit-code failure)) :exit-code)))
    (let ((pipe (run (list (from-list '("x"))
                           (sh-filter "cat >/dev/null; exit 3" :on-exit :ignore)))))
      (join pipe)
      (check (null (pipeline-failures pipe)) :on-exit-ignore))
    ;; Wired as a source it has nothing to read, and says so rather than
    ;; blocking or emitting nothing.
    (let ((pipe (run (list (sh-filter "cat")))))
      (join pipe)
      (check (= 1 (length (pipeline-failures pipe))) :source-position-is-an-error))))

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


;;; -------------------------------------------------------------------- watch

(defun test-channel-counters ()
  "PASSED is what the whole live view is built on, so it has to be exactly the
number of objects that crossed -- checked against a TALLY of the same stream
rather than against itself."
  (with-timeout (15 :channel-counters)
    (let* ((sink (make-channel :capacity 8))
           (pipe (run (list (counter :limit 50) (where (lambda (n) (evenp n))))
                      :sink sink))
           (seen 0))
      (loop (multiple-value-bind (x ok) (recv sink)
              (declare (ignore x))
              (unless ok (return))
              (incf seen)))
      (join pipe)
      (check (= 25 seen) :half-the-integers-arrived)
      (check (= 25 (channel-passed sink)) :sink-passed-matches-the-tally)
      ;; The channel between COUNTER and WHERE saw all 50.
      (check (= 50 (channel-passed (first (pipeline-channels pipe))))
             :upstream-passed-counts-everything))
    ;; A discard sink still counts: SEND increments before returning early,
    ;; which is what makes the last stage of a plain pipeline measurable.
    (let ((pipe (run (list (counter :limit 7) (print-items :stream (make-broadcast-stream))))))
      (join pipe)
      (check (= 7 (channel-passed (first (pipeline-channels pipe))))
             :counted-into-a-sink))))

(defun test-channel-last-is-dropped-with-the-buffer ()
  "LAST retains one object past its natural life on purpose.  CLOSE-INPUT drops
the buffer so a dead channel pins nothing; LAST has to go the same way."
  (let ((ch (make-channel :capacity 4)))
    (send ch :a)
    (check (eq :a (channel-last ch)) :last-records-the-most-recent)
    (send ch :b)
    (check (eq :b (channel-last ch)) :last-updates)
    (close-input ch)
    (check (null (channel-last ch)) :close-input-clears-last)))

(defun watch-panel (stages)
  "Run STAGES watched, returning (VALUES stdout panel)."
  (let* ((panel (make-string-output-stream))
         (out (with-output-to-string (*standard-output*)
                (watch-pipeline stages :interval 0.05 :stream panel))))
    (values out (get-output-stream-string panel))))

(defun test-watch-pipeline ()
  "The pipeline form: objects to stdout, the panel to its own stream."
  (with-timeout (20 :watch-pipeline)
    (multiple-value-bind (out panel) (watch-panel (list (counter :limit 4) (take 3)))
      ;; Objects are printed exactly as they are without WATCH -- one per line,
      ;; so `watch ... | wc -l` is still the count.
      (check (equal '("0" "1" "2") (split-lines out)) :stdout-is-untouched)
      (check (search "counter" panel) :panel-names-the-stages)
      (check (search "take" panel) :panel-names-every-stage)
      (check (search "obj" panel) :panel-shows-throughput))))

(defun test-watch-leaves-nothing-behind ()
  "The hook, the refcount and the registry must all come back to rest, or the
next unwatched command pays for a panel nobody asked for."
  (with-timeout (20 :watch-cleanup)
    (watch-panel (list (counter :limit 3)))
    (check (null plumb::*before-output*) :output-hook-cleared)
    (check (zerop plumb::*watchers*) :watcher-refcount-back-to-zero)
    (check (null plumb::*watch-points*) :registry-emptied)
    (check (null plumb::*watcher-thread*) :watcher-thread-joined)
    ;; And after a pipeline that dies, since that is when it matters.
    (let ((panel (make-string-output-stream)))
      (ignore-errors
       (watch-pipeline (list (counter :limit 5)
                             (xform (lambda (n) (error "boom ~a" n))))
                       :interval 0.05 :stream panel))
      (check (null plumb::*before-output*) :hook-cleared-after-a-failure)
      (check (zerop plumb::*watchers*) :refcount-cleared-after-a-failure)
      (check (null plumb::*watch-points*) :registry-emptied-after-a-failure))))

(defun test-watch-stage-is-a-tap ()
  "The stage form passes everything through unchanged, like PEEK."
  (with-timeout (20 :watch-stage)
    (let* ((panel (make-string-output-stream))
           (result (collect-pipeline (list (counter :limit 5)
                                           (watch :label "mid" :interval 0.05
                                                  :stream panel)
                                           (where (lambda (n) (oddp n)))))))
      (check (equal '(1 3) result) :stream-passes-through-untouched)
      (check (search "mid" (get-output-stream-string panel)) :panel-uses-the-label))
    (check (zerop plumb::*watchers*) :tap-cleans-up)))

(defun test-watch-reads-as-a-reserved-word ()
  "`watch` wraps the whole pipeline, the way `explain` does -- but maps to
WATCH-PIPELINE, because the bare symbol WATCH is the tap stage."
  (check (equal '(watch-pipeline (list (ls) (take 5))) (read-shell "watch ls | take 5"))
         :watch-wraps-the-pipeline)
  (check (equal '(list (ls) (watch) (take 5)) (read-shell "ls | watch | take 5"))
         :watch-in-the-middle-is-the-stage)
  (check (equal '(ls "watch") (read-shell "ls watch")) :not-reserved-as-an-argument)
  ;; The word EXPLAIN must keep mapping to itself.
  (check (equal '(explain (list (ls) (take 5))) (read-shell "explain ls | take 5"))
         :explain-still-maps-to-itself))

(defun test-interrupt-abandons-the-pipeline-not-the-session ()
  "^C at a prompt has to come back to the prompt.  MAIN handles
INTERACTIVE-INTERRUPT by exiting 130, which is right for `plumb 'expr'` and
wrong here; this is the handler that makes the REPL survive it."
  (let ((reported (with-output-to-string (*error-output*)
                    (check (null (plumb.cli::eval-forms-interruptibly
                                  (list '(error 'sb-sys:interactive-interrupt))
                                  nil))
                           :interrupt-is-caught-and-reported-as-failure))))
    (check (search "interrupted" reported) :interrupt-says-so-on-stderr))
  ;; An ordinary error still goes through the normal path.
  (let ((reported (with-output-to-string (*error-output*)
                    (plumb.cli::eval-forms-interruptibly (list '(error "ordinary")) nil))))
    (check (search "ordinary" reported) :other-errors-still-reported)))


;;; ------------------------------------------------------------------ workers

(defun test-close-input-is-refcounted ()
  "The mirror of CLOSE-OUTPUT.  Several workers share one input channel, so the
first to finish must not raise SIGPIPE on the others."
  (let ((ch (make-channel :capacity 4)))
    (setf (channel-consumers ch) 3)
    (close-input ch)
    (check (null (plumb::channel-consumer-closed ch)) :one-of-three-does-not-close)
    (close-input ch)
    (check (null (plumb::channel-consumer-closed ch)) :two-of-three-does-not-close)
    (close-input ch)
    (check (plumb::channel-consumer-closed ch) :the-last-one-closes))
  ;; ABORT-INPUT is CANCEL's tool and ignores the count entirely.
  (let ((ch (make-channel :capacity 4)))
    (setf (channel-consumers ch) 8)
    (send ch :a)
    (abort-input ch)
    (check (plumb::channel-consumer-closed ch) :abort-ignores-the-refcount)
    (check (null (channel-last ch)) :abort-drops-the-buffer)))

(defun test-run-sets-refcounts-from-thread-counts ()
  "Both counts are thread counts, not stage counts.  Getting either wrong is a
hang or an EOF delivered while somebody is still writing."
  (with-timeout (15 :refcounts)
    ;; An INFINITE source and an undrained sink, so every channel fills and no
    ;; stage ever exits: the counts stay where RUN put them.  Against a finite
    ;; pipeline this test races its own teardown, since closing is what
    ;; decrements them.
    (let* ((sink (make-channel :capacity 4))
           (pipe (run (list (counter)
                            (xform #'identity :workers 4)
                            (where #'evenp :workers 2))
                      :sink sink)))
      (unwind-protect
           (let ((first (first (pipeline-channels pipe)))
                 (second (second (pipeline-channels pipe))))
             (check (= 1 (channel-producers first)) :one-producer-for-a-plain-source)
             (check (= 4 (channel-consumers first)) :four-consumers-downstream)
             (check (= 4 (channel-producers second)) :four-producers-upstream)
             (check (= 2 (channel-consumers second)) :two-consumers-downstream)
             (check (= 2 (channel-producers sink)) :sink-counts-the-last-stage)
             ;; :ERR is shared by every thread, not every stage: 1 + 4 + 2.
             (check (= 7 (channel-producers (plumb::pipeline-err pipe)))
                    :err-counts-threads))
        (cancel pipe)))))

(defun test-workers-process-every-object-exactly-once ()
  "RECV under the channel mutex is the whole work distributor.  What it has to
guarantee is that N workers between them see each object once -- no duplicate,
no drop -- however the OS happens to schedule them."
  (with-timeout (20 :workers-distribute)
    (let ((result (collect-pipeline (list (counter :limit 200)
                                          (xform #'1+ :workers 8)))))
      (check (= 200 (length result)) :nothing-lost-or-duplicated)
      (check (equal (loop for i from 1 to 200 collect i) (sort result #'<))
             :same-multiset-out))))

(defun test-workers-do-not-preserve-order ()
  "Stated as a test because it is a promise, not an accident: output is in
completion order.  Sorting is how you get order back."
  (with-timeout (20 :workers-unordered)
    ;; One worker must still be exactly ordered -- the default cannot change.
    (check (equal '(0 1 2 3 4) (collect-pipeline (list (counter :limit 5)
                                                       (xform #'identity))))
           :one-worker-is-ordered)))

(defun test-take-after-a-parallel-stage-tears-everything-down ()
  "The SIGPIPE path with N consumers in the middle.  TAKE closes its input; each
of the four workers must see CHANNEL-CLOSED on its next SEND, unwind, and
between them close the source's channel -- or the infinite COUNTER runs forever
and this test hangs rather than fails."
  (with-timeout (15 :take-through-workers)
    (let ((result (collect-pipeline (list (counter)
                                          (xform #'identity :workers 4)
                                          (take 5)))))
      (check (= 5 (length result)) :take-still-stops-an-infinite-source))))

(defun test-one-worker-failing-leaves-the-others-running ()
  "A worker that dies takes its own thread down, not the stage.  With an
unrefcounted CLOSE-INPUT it took the upstream with it and the surviving workers
starved -- which is the bug the refcount exists to prevent."
  (with-timeout (20 :one-worker-fails)
    ;; :ERRORP NIL because a PARTIAL result is exactly the point here -- one
    ;; worker dies and the rest finish the job.  That is the case the default
    ;; exists to make loud, so this is the one place that opts out of it.
    (let ((result (collect-pipeline
                   (list (counter :limit 100)
                         (xform (lambda (n)
                                  ;; Exactly one worker dies, on one object.
                                  (when (= n 0) (error "worker down"))
                                  n)
                                :workers 4))
                   :errorp nil)))
      ;; 99 of the 100 survive: only the object that signalled is lost, along
      ;; with the one worker that was carrying it.
      (check (= 99 (length result)) :the-other-workers-finished-the-job)
      (check (not (member 0 result)) :the-failing-object-did-not-come-through))))

(defun test-cancel-stops-a-parallel-pipeline ()
  "CANCEL uses ABORT-INPUT, because against eight workers a refcount decrement
retires one of them and leaves seven reading."
  (with-timeout (15 :cancel-parallel)
    (let* ((sink (make-channel :capacity 4))
           (pipe (run (list (counter) (xform #'identity :workers 4)) :sink sink)))
      (sleep 0.2)
      (cancel pipe)
      (check (notany #'task-live-p (pipeline-tasks pipe)) :every-worker-stopped))))

(defun test-workers-need-the-parallel-declaration ()
  "Opt-in, because the unsafe cases fail silently.  TAKE mutates the
constructor's own parameter; there is no :WORKERS key for it to accept."
  (check (stage-parallel (xform #'identity)) :xform-is-parallel)
  (check (stage-parallel (where #'evenp)) :where-is-parallel)
  (check (not (stage-parallel (take 5))) :take-is-not-parallel)
  (check (not (stage-parallel (sort-by #'identity))) :a-barrier-is-not-parallel)
  (check (= 1 (stage-workers (xform #'identity))) :one-worker-by-default)
  (check (= 4 (stage-workers (xform #'identity :workers 4))) :workers-is-recorded)
  ;; A stage that has not declared it does not grow the key.
  (check (nth-value 1 (ignore-errors (take 5 :workers 4))) :take-rejects-workers)
  ;; And the count has to be a real thread count.
  (check (typep (nth-value 1 (ignore-errors (xform #'identity :workers 0))) 'type-error)
         :zero-workers-is-a-type-error))

(defun test-watch-is-unaffected-by-workers ()
  "The claim that WATCH needed no change: a parallel stage still has ONE output
channel, so the counters mean what they meant."
  (with-timeout (20 :watch-with-workers)
    (let* ((sink (make-channel :capacity 8))
           (pipe (run (list (counter :limit 50) (xform #'identity :workers 4))
                      :sink sink))
           (seen 0))
      (loop (multiple-value-bind (x ok) (recv sink)
              (declare (ignore x))
              (unless ok (return))
              (incf seen)))
      (join pipe)
      (check (= 50 seen) :all-fifty-arrived)
      (check (= 50 (channel-passed sink)) :passed-still-counts-every-object))))


;;; ------------------------------------------------------------- block devices

(defun shell-lines (command)
  "Output of COMMAND as a list of lines.  For cross-checking DISKS against the
tools that already know the answer."
  (let ((text (with-output-to-string (out)
                (sb-ext:run-program "/bin/sh" (list "-c" command)
                                    :search nil :wait t :output out))))
    (split-lines text)))

(defun test-disks-emits-something ()
  "The failure mode to guard first: a parser that silently yields nothing makes
every other assertion in this section pass vacuously."
  (with-timeout (60 :disks-nonempty)
    (let ((devices (collect-pipeline (list (disks)))))
      (check (plusp (length devices)) :some-devices-found)
      (check (every #'block-device-p devices) :all-are-block-devices)
      (check (every #'block-device-name devices) :every-device-is-named)
      ;; Bytes, like LS's .size -- not sectors and not a rounded human figure.
      ;; A size in sectors would be ~512x too small and still look plausible.
      (check (some (lambda (d) (and (block-device-size d)
                                    (> (block-device-size d) 1000000000)))
                   devices)
             :sizes-are-in-bytes)
      (check (every (lambda (d) (member (block-device-type d)
                                        '(:disk :partition :volume :loop :ram)))
                    devices)
             :types-are-from-the-coarse-set))))

(defun test-disks-agrees-with-the-system-tool ()
  "Cross-checked against the tool that already knows, the way src/stat.lisp
asserts its hand-written struct against sb-posix -- and for the same reason: a
parser of human-facing output fails by producing *plausible* numbers, which no
self-consistent test would catch."
  (with-timeout (90 :disks-cross-check)
    (let ((devices (collect-pipeline (list (disks)))))
      #+darwin
      (let ((listed (remove-if-not
                     (lambda (l) (plusp (length l)))
                     (mapcar (lambda (l) (string-trim " " l))
                             (shell-lines "diskutil list | awk '/^\\/dev\\// {print $1}'")))))
        ;; Every whole disk diskutil lists must appear, by node.
        (check (every (lambda (node)
                        (find node devices :key #'block-device-node :test #'equal))
                      listed)
               :every-whole-disk-appears)
        ;; And a size we did not compute ourselves.
        (let ((root (find "/" devices :key #'block-device-mount-point :test #'equal)))
          (check root :the-root-volume-is-present)
          (when root
            (let* ((line (first (shell-lines
                                 (format nil "diskutil info ~a | grep -E '(Disk|Volume Total) Size|Container Total'"
                                         (block-device-name root)))))
                   (open (and line (position #\( line)))
                   (bytes (and open (parse-integer line :start (1+ open) :junk-allowed t))))
              (check (and bytes (= bytes (block-device-size root)))
                     :root-size-matches-diskutil)))))
      #+linux
      (progn
        ;; lsblk -b prints bytes, which is what .size is.
        (dolist (line (rest (shell-lines "lsblk -bnro NAME,SIZE")))
          (let* ((f (plumb::split-on-spaces line))
                 (name (first f))
                 (bytes (and (second f) (parse-integer (second f) :junk-allowed t)))
                 (ours (find name devices :key #'block-device-name :test #'equal)))
            (when (and ours bytes)
              (check (eql bytes (block-device-size ours)) :size-matches-lsblk))))
        ;; Partitions know their parent, and mount points come from /proc/mounts.
        (let ((parts (remove :partition devices :key #'block-device-type :test-not #'eq)))
          (check (every #'block-device-parent parts) :every-partition-has-a-parent))
        (dolist (line (shell-lines "grep '^/dev/' /proc/mounts"))
          (let* ((f (plumb::split-on-spaces line))
                 (ours (find (first f) devices :key #'block-device-node :test #'equal)))
            (when ours
              (check (equal (plumb::unescape-mount-field (second f))
                            (block-device-mount-point ours))
                     :mount-point-matches-proc-mounts)
              (check (equal (third f) (block-device-fs-type ours))
                     :fs-type-matches-proc-mounts))))
        ;; major:minor straight out of sysfs.
        (let ((root (find "/" devices :key #'block-device-mount-point :test #'equal)))
          (when root
            (check (block-device-major root) :major-is-populated-on-linux)
            (check (block-device-minor root) :minor-is-populated-on-linux)))))))

(defun test-disks-usage-only-where-mounted ()
  "USED and AVAILABLE are filesystem facts, so an unmounted device must not
carry them -- a leftover from the previous row is exactly the kind of wrong
answer that reads fine."
  (with-timeout (60 :disks-usage)
    (let ((devices (collect-pipeline (list (disks)))))
      (check (every (lambda (d)
                      (or (block-device-mount-point d)
                          (and (null (block-device-used d))
                               (null (block-device-available d)))))
                    devices)
             :usage-only-on-mounted-devices)
      (let ((mounted (remove nil devices :key #'block-device-mount-point)))
        (check (plusp (length mounted)) :something-is-mounted)
        (check (every (lambda (d) (and (block-device-used d)
                                       (block-device-available d)))
                      mounted)
               :mounted-devices-report-usage)))))

(defun test-disks-has-no-selection-options ()
  "Same argument PS makes: narrowing is WHERE.  If DISKS grew a filter it would
be the start of re-implementing lsblk's option set."
  (check (null (plumb::si-lambda-list (gethash 'disks plumb::*stages*)))
         :disks-takes-no-arguments)
  (check (eq :source (plumb::stage-kind (gethash 'disks plumb::*stages*)))
         :disks-is-a-source))


;;; --------------------------------------------------------------- thread pool

(defun test-pool-reuses-threads ()
  "The point of the pool: running many pipelines must stop making threads.
Measured as threads CREATED against stages started, because that is the cost
being removed -- ~28us of MAKE-THREAD per stage."
  (with-timeout (30 :pool-reuse)
    (let ((before (getf (pool-statistics) :created)))
      (dotimes (i 200)
        (collect-pipeline (list (from-list (list 1 2)) (where #'oddp) (tally))))
      (let ((made (- (getf (pool-statistics) :created) before)))
        ;; 600 stages started.  A handful of threads, not hundreds.
        (check (< made 60) :threads-created-is-not-proportional-to-stages)))))

(defun test-pool-never-waits-for-a-free-worker ()
  "The property everything rests on.  Every stage of a pipeline must be running
for any of it to progress, so a pool that made a stage queue for a worker would
deadlock rather than run slowly.  Pinned by shrinking the idle cache to one and
running a pipeline far wider than it."
  (with-timeout (30 :pool-elastic)
    (let ((*idle-workers* 1))
      (let ((stages (append (list (counter :limit 20))
                            (loop repeat 24 collect (xform #'1+))
                            (list (tally)))))
        (check (equal '(20) (collect-pipeline stages)) :a-26-stage-pipeline-completes)))
    ;; And several at once, each wider than the cache.
    (let ((*idle-workers* 2)
          (results (make-array 20 :initial-element nil)))
      (let ((threads (loop for i below 20
                           collect (let ((i i))
                                     (sb-thread:make-thread
                                      (lambda ()
                                        (setf (aref results i)
                                              (collect-pipeline
                                               (list (counter :limit 5)
                                                     (xform #'1+) (xform #'1+)
                                                     (tally))))))))))
        (mapc #'sb-thread:join-thread threads)
        (check (every (lambda (r) (equal '(5) r)) results)
               :twenty-concurrent-pipelines-all-complete)))))

(defun test-a-stuck-stage-does-not-starve-later-pipelines ()
  "A worker held forever by a blocked stage is fine -- it is not idle, so it is
not reused -- but it must not stop anything else from starting."
  (with-timeout (30 :pool-stuck)
    (let* ((sink (make-channel :capacity 1))
           (stuck (run (list (counter) (xform #'identity)) :sink sink)))
      (unwind-protect
           (progn
             (sleep 0.2)                ; both stages now wedged on a full sink
             (check (equal '(1 2) (collect-pipeline (list (from-list (list 1 2)))))
                    :a-later-pipeline-still-runs))
        (cancel stuck)))))

(defun test-a-failing-task-releases-its-worker ()
  "SPAWN-STAGE handles what a stage is expected to signal; the pool's own
unwind-protect is the backstop for what it is not.  A task that dies must still
signal completion -- otherwise AWAIT hangs -- and must leave its worker usable."
  (with-timeout (20 :pool-failure)
    (let ((condition (await (spawn "plumb:test-boom" (lambda () (error "boom"))))))
      (check (typep condition 'error) :the-condition-comes-back-from-await)
      (check (search "boom" (princ-to-string condition)) :and-it-is-the-right-one))
    ;; The pool is still working afterwards.
    (let ((ran nil))
      (await (spawn "plumb:test-after" (lambda () (setf ran t))))
      (check ran :the-pool-still-runs-tasks-after-a-failure))
    ;; A warning is not a death: SERIOUS-CONDITION, not CONDITION.
    (let ((finished nil))
      (await (spawn "plumb:test-warn"
                    (lambda () (handler-bind ((warning #'muffle-warning))
                                 (warn "noise"))
                            (setf finished t))))
      (check finished :a-warning-does-not-abort-a-task))))

(defun test-pooled-threads-still-carry-the-stage-name ()
  "Names are per task, not per thread, so plumb:LS still appears in a backtrace
and in LIST-ALL-THREADS.  Losing that would make every future concurrency bug
harder to read."
  (with-timeout (20 :pool-names)
    (let ((seen nil))
      (await (spawn "plumb:test-name"
                    (lambda () (setf seen (sb-thread:thread-name
                                           sb-thread:*current-thread*)))))
      (check (equal "plumb:test-name" seen) :the-task-name-is-the-thread-name))
    ;; And a real stage gets the name RUN gave it.
    (let ((names '()))
      (join (run (list (counter :limit 3)
                       (xform (lambda (x)
                                (push (sb-thread:thread-name sb-thread:*current-thread*)
                                      names)
                                x)))))
      (check (member "plumb:XFORM" names :test #'string-equal) :stages-are-named))))

(defun test-await-is-idempotent ()
  "JOIN may be called twice on one pipeline -- CANCEL joins one it has already
torn down -- so a second AWAIT must not block waiting for a permit that has
already been taken."
  (with-timeout (20 :pool-await-twice)
    (let ((task (spawn "plumb:test-twice" (lambda () 42))))
      (await task)
      (check (null (await task)) :second-await-returns-immediately)
      (check (not (task-live-p task)) :and-the-task-reads-as-finished))
    (let ((pipe (run (list (counter :limit 3) (tally)))))
      (join pipe)
      (check (null (join pipe)) :joining-a-pipeline-twice-is-fine))))


;;; ----------------------------------------------------------------- env

(defun test-env ()
  "The process's own environment, no subprocess and nobody's output to parse."
  (with-timeout (15 :env)
    (let ((vars (collect-pipeline (list (env)))))
      (check (plusp (length vars)) :some-variables-found)
      (check (every #'env-var-p vars) :all-are-env-vars)
      ;; PATH is set in any environment this can run in.
      (let ((path (find "PATH" vars :key #'env-var-name :test #'string=)))
        (check path :path-is-present)
        (check (equal (sb-ext:posix-getenv "PATH") (env-var-value path))
               :value-matches-getenv))
      ;; The split is on the FIRST equals only: a value may contain more.
      (let ((weird (find-if (lambda (v) (find #\= (or (env-var-value v) ""))) vars)))
        (when weird
          (check (not (find #\= (env-var-name weird))) :name-never-contains-equals))))))

(defun test-a-field-named-value-is-not-the-whole-row ()
  "TABLE-VALUE reads its scalar column as `the row itself`, and that column used
to be the keyword :VALUE -- so any object with a real field called VALUE had
every cell replaced by the object.  ENV-VAR was the first type to have one."
  (let* ((rows (list (make-env-var :name "A" :value "1")
                     (make-env-var :name "B" :value "2")))
         (out (with-output-to-string (s) (render-table rows :stream s)))
         (lines (split-lines out)))
    (check (search "value" (first lines)) :the-column-is-still-called-value)
    (check (search "1" (second lines)) :and-holds-the-field)
    (check (not (search "A=1" out)) :not-the-whole-object))
  ;; A fieldless object still renders under one column called `value`.
  (let* ((out (with-output-to-string (s) (render-table '(1 2) :stream s)))
         (lines (split-lines out)))
    (check (string= "value" (string-trim " " (first lines))) :scalars-keep-the-header)
    (check (string= "1" (string-trim " " (second lines))) :and-print-themselves)))


;;; ------------------------------------------------------------------- git

(defmacro with-git-fixture ((dir) &body body)
  "A repository with the cases that break a naive parser: a path containing a
space, a staged rename, a worktree-only modification and an untracked file."
  `(let ((,dir "/tmp/plumb-git-test/"))
     (flet ((sh (command)
              (sb-ext:run-program "/bin/sh" (list "-c" command) :search nil :wait t)))
       (unwind-protect
            (progn
              (sh (format nil "rm -rf ~a; mkdir -p ~a" ,dir ,dir))
              (sh (format nil "cd ~a && git init -q . && git config user.email t@t && ~
git config user.name Tester && printf 'a\\n' > kept.txt && ~
printf 'b\\n' > 'spaced name.txt' && printf 'c\\n' > renamed-from.txt && ~
git add -A && git commit -qm 'first commit' && ~
printf 'more\\n' >> kept.txt && ~
printf 'x\\n' >> 'spaced name.txt' && git add 'spaced name.txt' && ~
git mv renamed-from.txt renamed-to.txt && printf 'z\\n' > untracked.txt" ,dir))
              ,@body)
         (sh (format nil "rm -rf ~a" ,dir))))))

(defun test-commits ()
  "git log --format is git's own contract, identical on every platform, which
is why this stage has no platform fork at all."
  (with-timeout (60 :commits)
    (with-git-fixture (dir)
      (let ((cs (collect-pipeline (list (commits :directory dir)))))
        (check (= 1 (length cs)) :one-commit-so-far)
        (let ((c (first cs)))
          (check (commit-p c) :emits-commits)
          (check (= 40 (length (commit-hash c))) :full-hash)
          (check (eql 0 (search (commit-short c) (commit-hash c))) :short-is-a-prefix)
          (check (string= "Tester" (commit-author c)) :author)
          (check (string= "t@t" (commit-email c)) :email)
          (check (string= "first commit" (commit-subject c)) :subject)
          ;; The root commit has no parent.
          (check (null (commit-parents c)) :root-has-no-parents)
          ;; A universal time, like LS's .mtime -- not a unix stamp and not a
          ;; string, so one 7d literal compares against either.
          (check (integerp (commit-date c)) :date-is-a-number)
          (check (< (encode-universal-time 0 0 0 1 1 2020 0)
                    (commit-date c)
                    (+ (get-universal-time) 86400))
                 :date-is-a-plausible-universal-time))))))

(defun test-commits-can-be-bounded-by-take ()
  "No :LIMIT option, for the reason PS gives.  TAKE closing the channel has to
stop the git walk, or `commits | take 5` on a large repository would read the
whole history first.

Builds its own repository rather than using the one the suite happens to be run
from: that assumed the working directory is a checkout with commits in it, which
is true here and false anywhere the tree was copied without .git -- it failed
the first time this ran on Linux."
  (with-timeout (60 :commits-take)
    (let ((dir "/tmp/plumb-git-take-test/"))
      (flet ((sh (command)
               (sb-ext:run-program "/bin/sh" (list "-c" command) :search nil :wait t)))
        (unwind-protect
             (progn
               (sh (format nil "rm -rf ~a; mkdir -p ~a" dir dir))
               (sh (format nil "cd ~a && git init -q . && git config user.email t@t && ~
git config user.name Tester && for i in 1 2 3 4 5; do ~
echo $i > f$i.txt && git add -A && git commit -qm \"commit $i\"; done" dir))
               (check (= 5 (length (collect-pipeline (list (commits :directory dir)))))
                      :the-fixture-has-five-commits)
               (let ((cs (collect-pipeline (list (commits :directory dir) (take 3)))))
                 (check (= 3 (length cs)) :take-bounds-the-walk)
                 (check (every #'commit-p cs) :and-they-are-commits)
                 ;; Newest first, which is git log's order and must survive.
                 (check (string= "commit 5" (commit-subject (first cs))) :newest-first)))
          (sh (format nil "rm -rf ~a" dir)))))))

(defun test-changes ()
  "porcelain v2 -- the format git documents as stable for scripts, where the
human-readable one explicitly is not."
  (with-timeout (60 :changes)
    (with-git-fixture (dir)
      (let* ((cs (collect-pipeline (list (changes :directory dir))))
             (by (lambda (p) (find p cs :key #'change-path :test #'string=))))
        (check (= 4 (length cs)) :four-entries)
        ;; Modified in the worktree only: git's second column, not its first.
        (let ((kept (funcall by "kept.txt")))
          (check (eq :modified (change-unstaged kept)) :worktree-modification)
          (check (null (change-staged kept)) :and-nothing-staged))
        ;; Staged, so the other column.
        (let ((spaced (funcall by "spaced name.txt")))
          (check spaced :a-path-containing-a-space-survives)
          (check (eq :modified (change-staged spaced)) :staged-modification))
        ;; A rename carries where it came from.
        (let ((renamed (funcall by "renamed-to.txt")))
          (check (eq :renamed (change-status renamed)) :rename-detected)
          (check (string= "renamed-from.txt" (change-old-path renamed)) :old-path))
        (let ((new (funcall by "untracked.txt")))
          (check (eq :untracked (change-status new)) :untracked))))))

(defun test-git-outside-a-repository-is-a-clear-error ()
  "The failure has to name itself.  GIT exits non-zero and WITH-COMMAND turns
that into COMMAND-FAILED, which rides the :ERR port like any other stage error."
  (with-timeout (30 :git-not-a-repo)
    (let ((condition (nth-value 1 (ignore-errors
                                   (collect-pipeline (list (commits :directory "/tmp/"))
                                                     :errorp t)))))
      (check condition :running-outside-a-repository-fails)
      (check (search "git" (string-downcase (princ-to-string condition)))
             :and-the-message-names-git))))


;;; --------------------------------------------------------------- handles

(defparameter +lsof-sample+
  (format nil "~{~a~%~}"
          '("p100" "cbash" "u501"
            "fcwd" "tDIR" "n/tmp"
            "f3" "tIPv4" "PTCP" "n*:8080" "TST=LISTEN" "TQR=0"
            "p200" "cother"
            "f4" "tREG" "s99" "i7" "n/etc/hosts"))
  "lsof -F is a STREAM of tagged lines, not a table: `p` opens a process, `f`
opens a file inside it, everything else sets a field on whichever is open.  The
three things that can go wrong are all here -- process context carrying to a
second file record, a new `p` flushing the one before it, and the last record
having no terminator but EOF.")

(defun parse-lsof-sample ()
  (let ((out '()))
    (with-input-from-string (in +lsof-sample+)
      (plumb::map-lsof-handles in (lambda (h) (push h out)) (make-hash-table)))
    (nreverse out)))

(defun test-lsof-field-format-state-machine ()
  (let ((hs (parse-lsof-sample)))
    (check (= 3 (length hs)) :three-records-including-the-last)
    (destructuring-bind (cwd sock other) hs
      ;; Process fields carry from the `p`/`c`/`u` lines onto each file.
      (check (eql 100 (handle-pid cwd)) :pid-from-the-process-record)
      (check (string= "bash" (handle-command cwd)) :command-carries)
      (check (eql 501 (handle-uid cwd)) :uid-carries)
      (check (string= "cwd" (handle-fd cwd)) :fd-is-a-string-not-a-number)
      (check (eq :dir (handle-type cwd)) :type-is-a-keyword)
      ;; The second file of the SAME process keeps its context.
      (check (eql 100 (handle-pid sock)) :context-carries-to-the-next-file)
      (check (eq :ipv4 (handle-type sock)) :socket-type)
      (check (eq :tcp (handle-protocol sock)) :protocol)
      ;; T carries several key=value pairs; only ST is the state.
      (check (eq :listen (handle-state sock)) :tcp-state-from-st)
      ;; A new `p` starts a new process and flushes what came before.
      (check (eql 200 (handle-pid other)) :new-process-record)
      (check (string= "other" (handle-command other)) :new-command)
      (check (null (handle-uid other)) :uid-resets-with-the-process)
      (check (eql 99 (handle-size other)) :size-is-a-number)
      (check (eql 7 (handle-inode other)) :inode-is-a-number)
      (check (string= "/etc/hosts" (handle-name other)) :name))))

(defun test-handles-on-our-own-process ()
  "Against a process whose open files we can verify independently."
  (with-timeout (60 :handles)
    (let ((mine (collect-pipeline (list (handles :pid (sb-posix:getpid))))))
      (check (plusp (length mine)) :some-handles-found)
      (check (every #'handle-p mine) :all-are-handles)
      ;; Nothing may lose its process context -- the state machine's one job.
      (check (every (lambda (h) (and (handle-pid h) (handle-command h) (handle-fd h)))
                    mine)
             :every-handle-knows-its-process)
      (check (every (lambda (h) (eql (sb-posix:getpid) (handle-pid h))) mine)
             :pid-selection-was-honoured)
      ;; lsof's cwd for this process is the one we are actually in.
      (let ((cwd (find "cwd" mine :key #'handle-fd :test #'string=)))
        (check cwd :cwd-is-listed)
        (when cwd
          (check (string= (string-right-trim "/" (sb-posix:getcwd))
                          (string-right-trim "/" (handle-name cwd)))
                 :cwd-matches-getcwd))))))



(defun test-line-continuation ()
  "A pipeline laid out over several lines.  Without this, `ls \"src/*\"` on one
line and `| take 5` on the next did NOT compose: the first line is a complete
pipeline on its own, so it ran, and the second started a fresh one.  Silent, and
it cost an afternoon twice -- once with | and once with a multi-line from-sql
that quietly lost its :database."
  ;; A trailing | or \\ means "not finished".
  (check (plumb.cli::continued-line-p "ls |") :trailing-pipe-continues)
  (check (plumb.cli::continued-line-p "ls | take 5 \\") :trailing-backslash-continues)
  (check (plumb.cli::continued-line-p "ls |   ") :trailing-space-does-not-hide-it)
  (check (not (plumb.cli::continued-line-p "ls | take 5")) :a-finished-line-does-not)
  (check (not (plumb.cli::continued-line-p "")) :empty-is-not-a-continuation)
  ;; TRY-READ must therefore report INCOMPLETE, not OK -- `ls |` reads fine as
  ;; a one-stage pipeline, which is exactly the trap.
  (check (eq :incomplete (nth-value 1 (plumb.cli::try-read "counter :limit 3 |")))
         :try-read-asks-for-more)
  (check (eq :ok (nth-value 1 (plumb.cli::try-read "counter :limit 3 | take 1")))
         :and-stops-when-it-is-done)
  ;; A backslash is spliced out; a pipe stays, because it is part of the
  ;; pipeline rather than punctuation about layout.
  (check (equal "a b" (plumb.cli::splice-continuations (format nil "a \\~%b")))
         :backslash-joins-and-vanishes)
  (check (equal (format nil "a |~%b") (plumb.cli::splice-continuations (format nil "a |~%b")))
         :pipe-is-left-alone)
  ;; And the whole thing reads as one pipeline either way.
  (check (equal '(list (counter :limit 3) (take 1))
                (read-shell (plumb.cli::splice-continuations
                             (format nil "counter :limit 3 |~%  take 1"))))
         :reads-as-one-pipeline)
  (check (equal '(list (counter :limit 3) (take 1))
                (read-shell (plumb.cli::splice-continuations
                             (format nil "counter :limit 3 \\~%  | take 1"))))
         :backslash-form-reads-the-same))

(defun test-a-failed-pipeline-is-not-an-empty-one ()
  "COLLECT-PIPELINE used to default to :ERRORP NIL, so a stage that died gave
back NIL -- indistinguishable from a run that simply found nothing.  That is a
wrong answer with no error, and it is how an empty result got mistaken for a
quiet network while writing the ARP demos."
  (with-timeout (20 :failed-is-not-empty)
    (let ((boom (list (counter :limit 3)
                      (xform (lambda (n) (declare (ignore n)) (error "boom"))))))
      ;; Loud by default.
      (let ((condition (nth-value 1 (ignore-errors (collect-pipeline boom)))))
        (check (typep condition 'pipeline-error) :a-dead-stage-signals)
        (check (search "boom" (princ-to-string condition)) :and-says-what-died))
      ;; Opting out still works, and now the failure is READABLE rather than
      ;; merely absent -- which is what makes a partial result usable.
      (multiple-value-bind (objects failures) (collect-pipeline boom :errorp nil)
        (check (null objects) :nothing-came-through)
        (check failures :but-the-failure-is-reported)
        (check (search "boom" (princ-to-string (cdr (first failures))))
               :and-it-is-the-right-one)))
    ;; A genuinely empty pipeline is still empty, with no failures -- the
    ;; distinction the second value exists to make.
    (multiple-value-bind (objects failures)
        (collect-pipeline (list (counter :limit 3) (where (constantly nil))))
      (check (null objects) :empty-result)
      (check (null failures) :and-no-failures))))

(defun test-uniq-dedupes-strings ()
  "UNIQ defaulted to EQL, under which two equal STRINGS are different objects --
so `ls | uniq :key .name` quietly kept every duplicate.  A wrong answer with no
error, on the field anyone would most obviously dedupe by."
  (with-timeout (20 :uniq-strings)
    (check (equal '("a" "b")
                  (collect-pipeline (list (from-list (list "a" "b" "a" "b")) (uniq))))
           :strings-dedupe)
    (check (equal '("x.lisp")
                  (collect-pipeline (list (from-list (list (list :name "x.lisp")
                                                          (list :name "x.lisp")))
                                          (uniq :key ($ (fld :name)))
                                          (xform ($ (fld :name))))))
           :string-keys-dedupe)
    ;; Lists too, which EQL also never matched.
    (check (= 1 (length (collect-pipeline
                         (list (from-list (list (list 1 2) (list 1 2))) (uniq)))))
           :equal-structures-dedupe)
    ;; What already worked must keep working: EQUAL compares numbers as EQL
    ;; does and falls back to EQ for structs, so it is a superset here.
    (check (equal '(1 2) (collect-pipeline
                          (list (from-list (list 1 2 1)) (uniq))))
           :integers-still-dedupe)
    (check (equal '(:a :b) (collect-pipeline
                            (list (from-list (list :a :b :a)) (uniq))))
           :keywords-still-dedupe)
    (check (= 2 (length (collect-pipeline
                         (list (from-list (list 1.5d0 1.5d0 2.5d0)) (uniq)))))
           :doubles-still-dedupe)
    ;; Identity is still reachable when that is what you meant.
    (check (= 2 (length (collect-pipeline
                         (list (from-list (list (copy-seq "a") (copy-seq "a")))
                               (uniq :test #'eq)))))
           :eq-is-still-available)))

(defun test-counter-limit-is-a-count ()
  "LIMIT counts objects, which is what the name says.  It used to bound the
VALUE, and the two agree for `counter :limit 5` -- which is why nothing noticed
until FROM or BY was given, and then it emitted NOTHING rather than erroring."
  (with-timeout (20 :counter-limit)
    (check (equal '(0 1 2 3 4) (collect-pipeline (list (counter :limit 5))))
           :the-common-case-is-unchanged)
    (check (equal '(5 6 7) (collect-pipeline (list (counter :from 5 :limit 3))))
           :from-plus-limit-emits-that-many)
    (check (equal '(0 2 4) (collect-pipeline (list (counter :by 2 :limit 3))))
           :by-plus-limit-emits-that-many)
    (check (equal '(10 8 6) (collect-pipeline (list (counter :from 10 :by -2 :limit 3))))
           :counting-down-works-at-all)
    (check (null (collect-pipeline (list (counter :limit 0)))) :zero-emits-nothing)
    ;; Still infinite without one, which is what TAKE is demonstrated against.
    (check (= 4 (length (collect-pipeline (list (counter) (take 4)))))
           :no-limit-is-still-endless)))

;;; --------------------------------------------------------------------- help

(defun stage-named (name) (gethash name plumb::*stages*))

(defun test-help-registry ()
  ;; The property, not a census: this asserted (= 23 ...) and adding a stage
  ;; failed a test about HELP.  What matters is that DEFSTAGE registers every
  ;; stage it defines, and that each entry names a real constructor.
  (check (loop for name being the hash-keys of plumb::*stages*
               always (fboundp name))
         :every-registered-stage-is-callable)
  (check (loop for name in '(counter ls where take sort-by table print-items)
               always (stage-named name))
         :defstage-registers-what-it-defines)
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
                  test-glob-matching
                  test-glob-posix-classes
                  test-glob-finds-awkward-names
                  test-glob-and-symlinks
                  test-ls-streams
                  test-glob-order-is-depth-first
                  test-ls-stats-rather-than-opens
                  test-alien-stat-layout-matches-sb-posix
                  test-sub-second-timestamps
                  test-reader-dispatch
                  test-reader-pipeline
                  test-reader-blocks
                  test-a-leading-comment-is-still-lisp
                  test-a-keyword-in-a-required-position-is-a-value
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
                  test-stderr-files-are-private
                  test-sh-filter-round-trip
                  test-sh-filter-needs-stdin-eof
                  test-sh-filter-survives-a-full-pipe
                  test-sh-filter-teardown
                  test-sh-filter-feeder-errors-are-not-swallowed
                  test-sh-filter-exit-status
                  test-to-sh-sink
                  test-lines-still-works
                  test-editor-text-operations
                  test-editor-word-motion
                  test-editor-kill-and-yank
                  test-editor-history
                  test-visible-width-ignores-colour
                  test-incremental-read
                  test-sink-prints-one-line-per-object
                  test-channel-counters
                  test-channel-last-is-dropped-with-the-buffer
                  test-watch-pipeline
                  test-watch-leaves-nothing-behind
                  test-watch-stage-is-a-tap
                  test-watch-reads-as-a-reserved-word
                  test-interrupt-abandons-the-pipeline-not-the-session
                  test-close-input-is-refcounted
                  test-run-sets-refcounts-from-thread-counts
                  test-workers-process-every-object-exactly-once
                  test-workers-do-not-preserve-order
                  test-take-after-a-parallel-stage-tears-everything-down
                  test-one-worker-failing-leaves-the-others-running
                  test-cancel-stops-a-parallel-pipeline
                  test-workers-need-the-parallel-declaration
                  test-watch-is-unaffected-by-workers
                  test-disks-emits-something
                  test-disks-agrees-with-the-system-tool
                  test-disks-usage-only-where-mounted
                  test-disks-has-no-selection-options
                  test-pool-reuses-threads
                  test-pool-never-waits-for-a-free-worker
                  test-a-stuck-stage-does-not-starve-later-pipelines
                  test-a-failing-task-releases-its-worker
                  test-pooled-threads-still-carry-the-stage-name
                  test-await-is-idempotent
                  test-env
                  test-a-field-named-value-is-not-the-whole-row
                  test-commits
                  test-commits-can-be-bounded-by-take
                  test-changes
                  test-git-outside-a-repository-is-a-clear-error
                  test-lsof-field-format-state-machine
                  test-handles-on-our-own-process
                  test-line-continuation
                  test-a-failed-pipeline-is-not-an-empty-one
                  test-uniq-dedupes-strings
                  test-counter-limit-is-a-count
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
