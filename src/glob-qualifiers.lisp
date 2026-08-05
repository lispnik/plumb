;;;; glob-qualifiers.lisp -- zsh's (...) qualifiers on a glob.
;;;;
;;;;   ls "*(.)"            plain files
;;;;   ls "*(.Lm+1)"        plain files over a megabyte
;;;;   ls "*(mh-1)"         modified within the last hour
;;;;   ls "*(om[1,3])"      the three most recently modified
;;;;
;;;; Qualifiers filter, then order, then subscript -- in that order, because
;;;; [1,3] means "of the sorted result", not "of the first three found".
;;;;
;;;; Two senses are easy to get backwards, so they are taken from zsh rather
;;;; than from memory:
;;;;
;;;;   L+n  is LARGER than n; L-n is smaller.
;;;;   m-n  is NEWER than n units ago; m+n is older.  The sign reads as a
;;;;        comparison against the file's *age*, not against its timestamp.
;;;;   oL   sorts ascending by size, but om sorts NEWEST first -- consistent
;;;;        only if o is read as ascending order of age.

(in-package #:plumb)

(defstruct (glob-qualifiers (:conc-name gq-))
  (tests '())                           ; (lambda (path stat) -> boolean)
  (order nil)                           ; :name :size :links :atime :mtime :ctime
  (reverse nil)
  (from nil) (to nil)                   ; [n] or [n,m], 1-based inclusive
  (follow nil))                         ; the - qualifier: stat through symlinks

(defparameter +size-units+ '((#\k . 1024) (#\m . 1048576)
                             (#\g . 1073741824) (#\p . 1125899906842624)))

(defparameter +time-units+ '((#\M . 2592000) (#\w . 604800) (#\h . 3600)
                             (#\m . 60) (#\s . 1)))

(defun qualifier-stat (path follow)
  (ignore-errors (if follow
                     (let ((s (sb-posix:stat path)))
                       (make-file-stat :mode (sb-posix:stat-mode s)
                                       :size (sb-posix:stat-size s)
                                       :nlink (sb-posix:stat-nlink s)
                                       :uid (sb-posix:stat-uid s)
                                       :gid (sb-posix:stat-gid s)
                                       :ino (sb-posix:stat-ino s)
                                       :dev (sb-posix:stat-dev s)
                                       ;; Whole seconds only: SB-POSIX's STAT
                                       ;; has no fractions, and this branch is
                                       ;; the - qualifier asking to follow.
                                       :atime (universal-from-unix (sb-posix:stat-atime s))
                                       :mtime (universal-from-unix (sb-posix:stat-mtime s))
                                       :ctime (universal-from-unix (sb-posix:stat-ctime s))))
                     (file-stat path))))

(defun mode-test (bit)
  (lambda (path stat) (declare (ignore path)) (logtest (fs-mode stat) bit)))

(defun type-test (predicate)
  (lambda (path stat) (declare (ignore path)) (funcall predicate (fs-mode stat))))

;;; ------------------------------------------------------------------ parsing

(defun parse-signed-number (text index)
  "A leading + or - and the digits after it.  Returns the comparison, the
number, and the index just past them."
  (let ((comparison :=))
    (case (and (< index (length text)) (char text index))
      (#\+ (setf comparison :>) (incf index))
      (#\- (setf comparison :<) (incf index)))
    (let ((start index))
      (loop while (and (< index (length text)) (digit-char-p (char text index)))
            do (incf index))
      (when (> index start)
        (values comparison (parse-integer text :start start :end index) index)))))

(defun compare-by (comparison value threshold)
  (ecase comparison (:> (> value threshold)) (:< (< value threshold))
                    (:= (= value threshold))))

(defun parse-size-qualifier (text index)
  "L[unit][+-]n -- a size test."
  (let ((scale 1))
    (let ((unit (assoc (and (< index (length text)) (char text index)) +size-units+)))
      (when unit (setf scale (cdr unit)) (incf index)))
    (multiple-value-bind (comparison n next) (parse-signed-number text index)
      (when comparison
        (values (lambda (path stat)
                  (declare (ignore path))
                  (compare-by comparison (fs-size stat) (* n scale)))
                next)))))

(defun parse-time-qualifier (text index accessor)
  "m|a|c [unit][+-]n -- an AGE test.  The sign compares against how old the
file is, so - is newer than and + is older than."
  (let ((scale 86400))                  ; days unless a unit says otherwise
    (let ((unit (assoc (and (< index (length text)) (char text index)) +time-units+)))
      (when unit (setf scale (cdr unit)) (incf index)))
    (multiple-value-bind (comparison n next) (parse-signed-number text index)
      (when comparison
        (values (lambda (path stat)
                  (declare (ignore path))
                  (let ((age (- (get-universal-time) (funcall accessor stat))))
                    (compare-by comparison age (* n scale))))
                next)))))

(defun parse-delimited-name (text index)
  "u:name: -- the text between the colons, and the index past the second."
  (when (and (< index (length text)) (char= (char text index) #\:))
    (let ((close (position #\: text :start (1+ index))))
      (when close
        (values (subseq text (1+ index) close) (1+ close))))))

(defun parse-qualifiers (text)
  "TEXT as a GLOB-QUALIFIERS, or NIL if it is not a qualifier list at all --
which is how (a|b) stays alternation rather than being read as qualifiers."
  (let ((q (make-glob-qualifiers))
        (i 0)
        (negate nil))
    (flet ((add (test)
             (push (if negate
                       (let ((inner test))
                         (lambda (p s) (not (funcall inner p s))))
                       test)
                   (gq-tests q))
             (setf negate nil)))
      (loop while (< i (length text))
            do (let ((c (char text i)))
                 (incf i)
                 (case c
                   (#\^ (setf negate (not negate)))
                   (#\- (setf (gq-follow q) t))
                   ;; types
                   (#\. (add (type-test #'sb-posix:s-isreg)))
                   (#\/ (add (type-test #'sb-posix:s-isdir)))
                   (#\@ (add (type-test #'sb-posix:s-islnk)))
                   (#\= (add (type-test #'sb-posix:s-issock)))
                   (#\p (add (type-test #'sb-posix:s-isfifo)))
                   (#\% (let ((kind (and (< i (length text)) (char text i))))
                          (case kind
                            (#\b (incf i) (add (type-test #'sb-posix:s-isblk)))
                            (#\c (incf i) (add (type-test #'sb-posix:s-ischr)))
                            (t (add (lambda (p s) (declare (ignore p))
                                      (or (sb-posix:s-isblk (fs-mode s))
                                          (sb-posix:s-ischr (fs-mode s)))))))))
                   ;; permissions
                   (#\* (add (lambda (p s) (declare (ignore p))
                               (and (sb-posix:s-isreg (fs-mode s))
                                    (logtest (fs-mode s) sb-posix:s-ixusr)))))
                   (#\r (add (mode-test sb-posix:s-irusr)))
                   (#\w (add (mode-test sb-posix:s-iwusr)))
                   (#\x (add (mode-test sb-posix:s-ixusr)))
                   (#\A (add (mode-test sb-posix:s-irgrp)))
                   (#\I (add (mode-test sb-posix:s-iwgrp)))
                   (#\E (add (mode-test sb-posix:s-ixgrp)))
                   (#\R (add (mode-test sb-posix:s-iroth)))
                   (#\W (add (mode-test sb-posix:s-iwoth)))
                   (#\X (add (mode-test sb-posix:s-ixoth)))
                   (#\s (add (mode-test sb-posix:s-isuid)))
                   (#\S (add (mode-test sb-posix:s-isgid)))
                   (#\t (add (mode-test sb-posix:s-isvtx)))
                   ;; ownership
                   (#\U (add (let ((me (sb-posix:getuid)))
                               (lambda (p s) (declare (ignore p)) (eql (fs-uid s) me)))))
                   (#\G (add (let ((mine (sb-posix:getgid)))
                               (lambda (p s) (declare (ignore p)) (eql (fs-gid s) mine)))))
                   (#\u (multiple-value-bind (name next) (parse-delimited-name text i)
                          (unless name (return-from parse-qualifiers nil))
                          (setf i next)
                          (let ((uid (or (ignore-errors
                                          (sb-posix:passwd-uid (sb-posix:getpwnam name)))
                                         (parse-integer name :junk-allowed t))))
                            (add (lambda (p s) (declare (ignore p)) (eql (fs-uid s) uid))))))
                   (#\g (multiple-value-bind (name next) (parse-delimited-name text i)
                          (unless name (return-from parse-qualifiers nil))
                          (setf i next)
                          (let ((gid (or (ignore-errors
                                          (sb-posix:group-gid (sb-posix:getgrnam name)))
                                         (parse-integer name :junk-allowed t))))
                            (add (lambda (p s) (declare (ignore p)) (eql (fs-gid s) gid))))))
                   ;; size, link count, device
                   (#\L (multiple-value-bind (test next) (parse-size-qualifier text i)
                          (unless test (return-from parse-qualifiers nil))
                          (setf i next) (add test)))
                   (#\l (multiple-value-bind (comparison n next) (parse-signed-number text i)
                          (unless comparison (return-from parse-qualifiers nil))
                          (setf i next)
                          (add (lambda (p s) (declare (ignore p))
                                 (compare-by comparison (fs-nlink s) n)))))
                   (#\d (multiple-value-bind (comparison n next) (parse-signed-number text i)
                          (declare (ignore comparison))
                          (unless n (return-from parse-qualifiers nil))
                          (setf i next)
                          (add (lambda (p s) (declare (ignore p)) (eql (fs-dev s) n)))))
                   ;; times
                   (#\m (multiple-value-bind (test next) (parse-time-qualifier text i #'fs-mtime)
                          (unless test (return-from parse-qualifiers nil))
                          (setf i next) (add test)))
                   (#\a (multiple-value-bind (test next) (parse-time-qualifier text i #'fs-atime)
                          (unless test (return-from parse-qualifiers nil))
                          (setf i next) (add test)))
                   (#\c (multiple-value-bind (test next) (parse-time-qualifier text i #'fs-ctime)
                          (unless test (return-from parse-qualifiers nil))
                          (setf i next) (add test)))
                   ;; ordering
                   ((#\o #\O)
                    (let ((by (and (< i (length text)) (char text i))))
                      (unless by (return-from parse-qualifiers nil))
                      (incf i)
                      (setf (gq-reverse q) (char= c #\O)
                            (gq-order q) (case by
                                           (#\n :name) (#\L :size) (#\l :links)
                                           (#\a :atime) (#\m :mtime) (#\c :ctime)
                                           (#\N :none)
                                           (t (return-from parse-qualifiers nil))))))
                   ;; subscript
                   (#\[ (let ((close (position #\] text :start i)))
                          (unless close (return-from parse-qualifiers nil))
                          (let* ((body (subseq text i close))
                                 (comma (position #\, body)))
                            (setf i (1+ close))
                            (setf (gq-from q) (parse-integer body :end comma :junk-allowed t)
                                  (gq-to q) (if comma
                                                (parse-integer body :start (1+ comma)
                                                                    :junk-allowed t)
                                                (gq-from q))))))
                   (t (return-from parse-qualifiers nil))))))   ; not qualifiers
    (setf (gq-tests q) (nreverse (gq-tests q)))
    q))

;;; ---------------------------------------------------------------- applying

(defun qualifier-sort-key (order)
  (ecase order
    (:name (lambda (path stat) (declare (ignore stat)) (basename path)))
    (:size (lambda (path stat) (declare (ignore path)) (fs-size stat)))
    (:links (lambda (path stat) (declare (ignore path)) (fs-nlink stat)))
    ;; Negated, because zsh's o for a time means newest first -- ascending
    ;; order of age.  At nanosecond precision, since a directory built in one
    ;; second would otherwise tie on every file and fall back to name order.
    (:atime (lambda (path stat) (declare (ignore path))
              (- (precise-time (fs-atime stat) (fs-atime-nsec stat)))))
    (:mtime (lambda (path stat) (declare (ignore path))
              (- (precise-time (fs-mtime stat) (fs-mtime-nsec stat)))))
    (:ctime (lambda (path stat) (declare (ignore path))
              (- (precise-time (fs-ctime stat) (fs-ctime-nsec stat)))))))

(defun apply-qualifiers (paths qualifiers)
  "Filter, then order, then subscript -- in that order, since [1,3] means the
first three of the sorted result."
  (let* ((follow (gq-follow qualifiers))
         (pairs (loop for path in paths
                      for stat = (qualifier-stat path follow)
                      when stat collect (cons path stat))))
    (setf pairs (remove-if-not
                 (lambda (pair)
                   (every (lambda (test) (funcall test (car pair) (cdr pair)))
                          (gq-tests qualifiers)))
                 pairs))
    (let ((order (gq-order qualifiers)))
      (when (and order (not (eq order :none)))
        (let ((key (qualifier-sort-key order)))
          ;; Times sort newest first, which is what zsh's o means for them:
          ;; ascending order of age.  QUALIFIER-SORT-KEY negates them for it.
          (setf pairs (stable-sort (copy-list pairs)
                                   (lambda (a b)
                                     (let ((x (funcall key (car a) (cdr a)))
                                           (y (funcall key (car b) (cdr b))))
                                       (if (and (stringp x) (stringp y))
                                           (string< x y)
                                           (< x y))))))))
      (when (gq-reverse qualifiers) (setf pairs (nreverse pairs))))
    (let ((paths (mapcar #'car pairs)))
      (if (gq-from qualifiers)
          (let* ((count (length paths))
                 (from (max 1 (gq-from qualifiers)))
                 (to (min count (or (gq-to qualifiers) from))))
            (if (<= from to) (subseq paths (1- from) to) '()))
          paths))))

(defun split-glob-qualifiers (component)
  "COMPONENT as its pattern and its qualifier text, if it ends in a qualifier
group.  (#q...) is explicit; a bare trailing (...) is only qualifiers when it
parses as some, which is what keeps (a|b) alternation."
  (let ((length (length component)))
    (when (and (plusp length) (char= (char component (1- length)) #\)))
      (let ((depth 0) (open nil))
        (loop for i from (- length 1) downto 0
              do (case (char component i)
                   (#\) (incf depth))
                   (#\( (decf depth) (when (zerop depth) (setf open i) (return)))))
        (when open
          (let ((body (subseq component (1+ open) (1- length))))
            (cond
              ((and (>= (length body) 2) (string= "#q" (subseq body 0 2)))
               (values (subseq component 0 open) (subseq body 2)))
              ((and (plusp (length body)) (parse-qualifiers body))
               (values (subseq component 0 open) body))
              (t (values component nil)))))))))
