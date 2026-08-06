;;;; git.lisp -- commits and working-tree status as objects.
;;;;
;;;; The rare source with no platform fork at all.  `git log --format=...` and
;;;; `git status --porcelain=v2` are git's OWN documented contracts, byte for
;;;; byte the same on every machine git runs on -- which is the property DISKS
;;;; had to work hardest to fake and PS only half has.  There is no #+darwin
;;;; anywhere in this file and there should never need to be.
;;;;
;;;; Same argument as PS for why it shells out: git already gets this right,
;;;; and what plumb adds is that it arrives as objects with typed fields, so
;;;; WHERE and SORT-BY replace --author, --since, --grep and --sort.

(in-package #:plumb)

;;; Field and record separators.  ASCII US (31) between fields: git writes it
;;; for %x1f and no commit metadata can contain it, unlike a tab or a comma.
(defconstant +us+ (code-char 31))

(defun split-on-char (line char)
  (let ((fields '()) (start 0))
    (loop for pos = (position char line :start start)
          do (push (subseq line start (or pos (length line))) fields)
             (if pos (setf start (1+ pos)) (return)))
    (nreverse fields)))

;;; ----------------------------------------------------------------- commits

(defstruct commit
  hash short author email date parents subject)

(defmethod present ((c commit))
  (format nil "~a  ~a" (commit-short c) (commit-subject c)))

(defparameter +commit-format+
  (format nil "--format=%H~c%h~c%an~c%ae~c%at~c%P~c%s" +us+ +us+ +us+ +us+ +us+ +us+)
  "%s is the SUBJECT -- git's term for the first line -- so a record is always
one line however long the message is.  %at is a unix timestamp, which is why
this needs no date parser: an ISO 8601 reader would be a hundred lines to
re-derive what git already hands over as an integer.")

(defun parse-commit-line (line)
  (let ((f (split-on-char line +us+)))
    (when (= (length f) 7)
      (make-commit
       :hash (first f)
       :short (second f)
       :author (third f)
       :email (fourth f)
       ;; Universal time, like LS's .mtime, so one literal compares against
       ;; both.  A unit that changed meaning per source would undo the reason
       ;; for having objects.
       :date (universal-from-unix (parse-integer (fifth f) :junk-allowed t))
       ;; Empty for the root commit; two or more for a merge.
       :parents (remove "" (split-on-char (sixth f) #\Space) :test #'string=)
       :subject (seventh f)))))

(defstage commits (&key path revision directory)
  "Emit a COMMIT per revision, newest first, as `git log` walks them.

  commits | take 5 | table
  commits | where {(string= .author \"Matthew Kennedy\")} | tally
  commits | where {(> .date (- (get-universal-time) 7d))} | table
  commits :path \"src/stat.lisp\" | take 3

.DATE is a universal time, like LS's .mtime, so `7d` means the same thing
against either.  PATH limits to commits touching it and REVISION picks the
starting point -- both are things git does far more cheaply than a filter
could, which is the same reason LS takes a pattern.  Everything else is WHERE.

There is no :LIMIT.  `commits | take 5` stops the walk: TAKE closes the channel,
the SEND fails, and the teardown cascade kills the git process -- which is how
an infinite COUNTER is bounded too."
  (:consumes nil) (:produces :objects)
  (with-command (proc (append (list "git" "log" +commit-format+)
                              (when revision (list revision))
                              (when path (list "--" path)))
                 '(:output :stream) :directory directory
                 :stderr :capture :on-exit :signal)
    (let ((out (sb-ext:process-output proc)))
      (loop for line = (read-line out nil nil)
            while line
            do (let ((commit (parse-commit-line line)))
                 (when commit (emit commit)))))))

;;; ------------------------------------------------------------------ status

(defstruct change
  path status staged unstaged old-path)

(defmethod present ((c change))
  ;; Two columns then the path, like `git status --short` -- but with
  ;; porcelain v2's `.` for "unchanged" rather than --short's space, because
  ;; that is the format being parsed and because a leading space is invisible
  ;; in a terminal.
  (format nil "~a~a ~a"
          (change-code (change-staged c)) (change-code (change-unstaged c))
          (change-path c)))

(defun change-code (state)
  (case state
    (:modified "M") (:added "A") (:deleted "D") (:renamed "R") (:copied "C")
    (:unmerged "U") (:untracked "?") (:ignored "!") (t ".")))

(defun status-letter (char)
  (case char
    (#\M :modified) (#\A :added) (#\D :deleted) (#\R :renamed)
    (#\C :copied) (#\U :unmerged) (t nil)))

(defun parse-status-line (line)
  "One porcelain v2 record.  The leading character is the kind:

  1  ordinary change      2  renamed or copied      u  unmerged
  ?  untracked            !  ignored                #  header

Fields are space separated and the path is last, so the path is taken as the
remainder rather than by splitting -- a path may contain spaces."
  (when (plusp (length line))
    (let ((kind (char line 0)))
      (case kind
        ((#\1 #\2)
         (let ((f (split-on-char line #\Space)))
           (when (>= (length f) 9)
             (let* ((xy (second f))
                    (staged (status-letter (char xy 0)))
                    (unstaged (status-letter (char xy 1)))
                    ;; Kind 2 has an extra rename-score field before the path,
                    ;; and joins new and old with a tab.
                    (rest-index (if (char= kind #\1) 8 9))
                    (tail (nth-value 1 (nth-field-and-rest line rest-index)))
                    (tab (position #\Tab tail)))
               (make-change
                :path (if tab (subseq tail 0 tab) tail)
                :old-path (when tab (subseq tail (1+ tab)))
                :staged staged
                :unstaged unstaged
                :status (or staged unstaged))))))
        (#\u (let ((tail (nth-value 1 (nth-field-and-rest line 10))))
               (make-change :path tail :status :unmerged
                            :staged :unmerged :unstaged :unmerged)))
        (#\? (make-change :path (subseq line 2) :status :untracked
                          :unstaged :untracked))
        (#\! (make-change :path (subseq line 2) :status :ignored))
        (t nil)))))                     ; # header lines

(defun nth-field-and-rest (line n)
  "Skip N space-separated fields; return them and the untouched remainder.
The remainder is a path, which may contain spaces, so it must not be split."
  (let ((start 0))
    (dotimes (i n)
      (let ((space (position #\Space line :start start)))
        (unless space (return-from nth-field-and-rest (values nil "")))
        (setf start (1+ space))))
    (values (subseq line 0 start) (subseq line start))))

(defstage changes (&key directory (ignored nil))
  "Emit a CHANGE per entry in `git status` -- what is modified, staged,
renamed or untracked in the working tree.

  changes | table
  changes | where {(eq .status :untracked)} | print-items
  changes | where {.staged} | tally

.STAGED and .UNSTAGED are git's two columns: what is different between HEAD and
the index, and between the index and the working tree.  Either may be NIL,
which is what `.` means in `git status --short`.

--porcelain=v2 rather than the default output: it is the format git documents
as stable for scripts, where the human one is explicitly not."
  (:consumes nil) (:produces :objects)
  (with-command (proc (append (list "git" "status" "--porcelain=v2")
                              (when ignored (list "--ignored")))
                 '(:output :stream) :directory directory
                 :stderr :capture :on-exit :signal)
    (let ((out (sb-ext:process-output proc)))
      (loop for line = (read-line out nil nil)
            while line
            do (let ((change (parse-status-line line)))
                 (when change (emit change)))))))
