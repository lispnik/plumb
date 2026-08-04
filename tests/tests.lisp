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
  ;; No suffix literals yet, so 5kb is a word.
  (check (equal '(take "5kb") (read-shell "take 5kb")) :no-suffix-literals-yet))

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

;;; ------------------------------------------------------------ presenting

(defun test-present ()
  (check (string= "abc" (present "abc")) :string-is-itself)
  (check (string= "hello" (present (make-line :text "hello" :number 1))) :line-is-its-text)
  (check (string= "a.lisp" (present (make-file-entry :name "a.lisp"))) :file-entry-is-its-name)
  (check (string= "src/" (present (make-file-entry :name "src" :dir-p t))) :directories-get-a-slash)
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

(defmacro help-output (&body body)
  "Capture what HELP prints.  A string stream is not interactive, so PAINT
leaves it plain and the assertions can look for bare text."
  `(with-output-to-string (*standard-output*) ,@body))

(defun test-help-registry ()
  (check (= 18 (hash-table-count plumb::*stages*)) :every-stage-registered)
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
                  test-reader-dispatch
                  test-reader-pipeline
                  test-reader-blocks
                  test-reader-variables-and-globs
                  test-reader-lisp-escape
                  test-reader-pipes-do-not-split-everything
                  test-reader-runs
                  test-present
                  test-table
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
