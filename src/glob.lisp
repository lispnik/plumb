;;;; glob.lisp -- shell globbing over directory entries read as strings.
;;;;
;;;; This used to be CL's DIRECTORY with a pathname pattern, which was wrong in
;;;; five ways, three of them silent:
;;;;
;;;;   [a-c]  was the literal set {a,-,c}, so ranges quietly skipped members
;;;;   [!a]   was the literal set {!,a}, so negation matched the opposite
;;;;   *      matched dotfiles, where a shell's does not
;;;;   A*     never matched a.txt, even on a case-insensitive filesystem
;;;;   star*.txt  vanished from LS entirely
;;;;
;;;; That last one was the real damage.  DIRECTORY returns pathnames whose name
;;;; component holds * and [ as pattern *objects*; FILE-NAMESTRING then renders
;;;; them escaped ("star\\*.txt"), LSTAT on that escaped path fails, and the
;;;; entry was dropped without a word.  Files hardest to name were the ones LS
;;;; could not see.
;;;;
;;;; So: read names with READDIR as plain strings, match them here, and build
;;;; pathnames only at the end with PARSE-NATIVE-NAMESTRING, which treats * as
;;;; the character it is.  NATIVE-NAMESTRING takes them back unescaped.

(in-package #:plumb)

;;; ------------------------------------------------------------------ matching

(defparameter +posix-character-classes+
  '(("alpha" . :alpha) ("digit" . :digit) ("alnum" . :alnum) ("space" . :space)
    ("upper" . :upper) ("lower" . :lower) ("punct" . :punct) ("print" . :print)
    ("graph" . :graph) ("cntrl" . :cntrl) ("xdigit" . :xdigit) ("blank" . :blank)))

(defun character-class-match (class char)
  (ecase class
    (:alpha (and (alpha-char-p char) t))
    (:digit (and (digit-char-p char) t))
    (:alnum (and (alphanumericp char) t))
    (:space (and (member char '(#\Space #\Tab #\Newline #\Page #\Return #\Linefeed)) t))
    (:upper (upper-case-p char))
    (:lower (lower-case-p char))
    (:punct (and (graphic-char-p char) (not (alphanumericp char)) (char/= char #\Space)))
    (:print (and (graphic-char-p char) t))
    (:graph (and (graphic-char-p char) (char/= char #\Space)))
    (:cntrl (not (graphic-char-p char)))
    (:xdigit (and (digit-char-p char 16) t))
    (:blank (and (member char '(#\Space #\Tab)) t))))

(defun glob-set-match (pattern start char)
  "Match CHAR against the [...] starting at START.  Returns whether it matched
and the index just past the closing bracket.

Ranges (a-z), negation (! or ^) and the POSIX classes [:alpha:] and friends.
[.x.] and [=x=] are accepted and degenerate to the literal character: they
differ from it only under a collating locale, and there is none here.  A ]
first in the set is literal, as in every shell."
  (let ((i (1+ start))
        (length (length pattern))
        (negate nil)
        (matched nil)
        (first t))
    (when (and (< i length) (member (char pattern i) '(#\! #\^)))
      (setf negate t)
      (incf i))
    (loop
      (when (>= i length)                 ; unterminated: treat [ as literal
        (return (values (char= char #\[) (1+ start))))
      (let ((c (char pattern i)))
        (cond
          ((and (char= c #\]) (not first))
           (return (values (if negate (not matched) matched) (1+ i))))
          ;; [:alpha:] [.x.] [=x=]
          ((and (char= c #\[) (< (1+ i) length)
                (member (char pattern (1+ i)) '(#\: #\. #\=)))
           (let* ((kind (char pattern (1+ i)))
                  (at (search (format nil "~c]" kind) pattern :start2 (+ i 2))))
             (cond
               ((null at) (when (char= c char) (setf matched t)) (incf i))
               (t (let ((body (subseq pattern (+ i 2) at)))
                    (setf i (+ at 2))
                    (if (char= kind #\:)
                        (let ((class (cdr (assoc body +posix-character-classes+
                                                 :test #'string-equal))))
                          (if class
                              (when (character-class-match class char) (setf matched t))
                              ;; An unknown class is just its characters.
                              (when (find char body) (setf matched t))))
                        ;; Collating symbol or equivalence class: literally.
                        (when (find char body) (setf matched t))))))))
          ;; a-z, but a trailing - before ] is itself literal
          ((and (< (+ i 2) length)
                (char= (char pattern (1+ i)) #\-)
                (char/= (char pattern (+ i 2)) #\]))
           (when (char<= c char (char pattern (+ i 2))) (setf matched t))
           (incf i 3))
          (t (when (char= c char) (setf matched t))
             (incf i))))
      (setf first nil))))

(defun glob-match-from (pattern p name n)
  (let ((plen (length pattern)) (nlen (length name)))
    (loop
      (when (>= p plen) (return (>= n nlen)))
      (let ((pc (char pattern p)))
        (case pc
          (#\*
           ;; Backtrack over every split.  * never crosses a separator because
           ;; matching runs one path component at a time.
           (return (loop for k from n to nlen
                         thereis (glob-match-from pattern (1+ p) name k))))
          (#\?
           (when (>= n nlen) (return nil))
           (incf p) (incf n))
          (#\[
           (when (>= n nlen) (return nil))
           (multiple-value-bind (ok next) (glob-set-match pattern p (char name n))
             (unless ok (return nil))
             (setf p next)
             (incf n)))
          (#\\
           ;; \x matches x literally, which is how a name containing a
           ;; metacharacter is written.
           (when (>= (1+ p) plen) (return nil))
           (when (or (>= n nlen) (char/= (char pattern (1+ p)) (char name n)))
             (return nil))
           (incf p 2) (incf n))
          (t
           (when (or (>= n nlen) (char/= pc (char name n))) (return nil))
           (incf p) (incf n)))))))

(defun glob-match (pattern name)
  "Does NAME match the single-component PATTERN?

A leading dot must be matched explicitly, as in a shell: * does not find
.hidden.  CL's DIRECTORY had no such rule, so `ls *` listed dotfiles."
  (if (and (plusp (length name)) (char= (char name 0) #\.)
           (not (and (plusp (length pattern)) (char= (char pattern 0) #\.))))
      nil
      (glob-match-from pattern 0 name 0)))

(defun glob-pattern-p (text)
  "Does TEXT contain an unescaped metacharacter?"
  (loop with i = 0
        while (< i (length text))
        do (case (char text i)
             (#\\ (incf i 2))
             ((#\* #\? #\[) (return t))
             (t (incf i)))
        finally (return nil)))

;;; ----------------------------------------------------------------- walking

(defun read-directory-names (directory)
  "Entry names in DIRECTORY as plain strings, sorted.

Nothing here goes through a pathname, which is the whole point: a name is
whatever bytes the filesystem holds, not something to be re-parsed.

Sorting happens here rather than over the finished result, because the walk
streams and so has no finished result to sort.  This is now the only place
ordering comes from."
  (let ((names '()) (dir nil))
    (unwind-protect
         (progn
           (setf dir (ignore-errors (sb-posix:opendir directory)))
           (when dir
             (loop for entry = (sb-posix:readdir dir)
                   until (sb-alien:null-alien entry)
                   do (let ((name (sb-posix:dirent-name entry)))
                        (unless (or (string= name ".") (string= name ".."))
                          (push name names))))))
      (when dir (ignore-errors (sb-posix:closedir dir))))
    (sort names #'string<)))

(defun directory-string-p (path &key (follow t))
  "Is PATH a directory?  FOLLOW decides whether a symlink to one counts.

Descending a named component follows, as a shell does -- /tmp is a symlink to
/private/tmp on macOS, and refusing it made every pattern under /tmp match
nothing.  ** does not follow, so a link pointing back up cannot make the
recursion run forever."
  (let ((stat (ignore-errors (if follow (sb-posix:stat path) (file-stat path)))))
    (and stat (sb-posix:s-isdir (if follow
                                    (sb-posix:stat-mode stat)
                                    (fs-mode stat))))))

(defun basename (path)
  "The last component of PATH, as a string.  Used instead of FILE-NAMESTRING,
which escapes glob metacharacters back into the name."
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path (1+ slash)) path)))

(defun join-path (directory name)
  (concatenate 'string directory
               (if (and (plusp (length directory))
                        (char= (char directory (1- (length directory))) #\/))
                   "" "/")
               name))

(defun walk-glob (directory components function)
  "Call FUNCTION on each path under DIRECTORY matching COMPONENTS, depth first.

Nothing is collected.  FUNCTION is free to stop the walk by transferring
control out of it -- which is exactly what happens when a downstream TAKE has
had enough: EMIT signals CHANNEL-CLOSED, that unwinds through here, and
SPAWN-STAGE treats it as normal termination.  The early exit is the existing
teardown doing its job, not machinery added for it."
  (cond
    ((null components) (funcall function directory))
    ;; ** matches zero or more directory levels.
    ;; ** matches zero or more directory levels.  The two cases -- the rest of
    ;; the pattern starting here, and ** consuming this level -- are
    ;; interleaved per entry rather than run one after the other, so the walk
    ;; is genuinely depth first.  Doing the zero-level pass first emits every
    ;; sibling before descending into any of them, which a final sort used to
    ;; hide and streaming cannot.
    ((string= (first components) "**")
     (let ((rest (rest components)))
       (when (null rest) (funcall function directory))
       (dolist (name (read-directory-names directory))
         (let ((child (join-path directory name)))
           (when (and rest (glob-match (first rest) name))
             (if (rest rest)
                 (when (directory-string-p child)
                   (walk-glob child (rest rest) function))
                 (funcall function child)))
           ;; :FOLLOW NIL here and only here: a symlink pointing back up a tree
           ;; would otherwise recurse until the stack gave up.
           (when (directory-string-p child :follow nil)
             (walk-glob child components function))))))
    (t
     (let ((component (first components))
           (rest (rest components)))
       (if (glob-pattern-p component)
           (loop for name in (read-directory-names directory)
                 when (glob-match component name)
                   do (let ((child (join-path directory name)))
                        (if rest
                            (when (directory-string-p child)
                              (walk-glob child rest function))
                            (funcall function child))))
           ;; A literal component needs no scan; just descend.
           (let ((child (join-path directory (unescape-glob component))))
             (cond (rest (when (directory-string-p child)
                           (walk-glob child rest function)))
                   ((ignore-errors (file-stat child)) (funcall function child)))))))))

(defun unescape-glob (component)
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length component))
          do (if (and (char= (char component i) #\\) (< (1+ i) (length component)))
                 (progn (write-char (char component (1+ i)) out) (incf i 2))
                 (progn (write-char (char component i) out) (incf i))))))

(defun split-path-components (text)
  (loop with start = 0
        for pos = (position #\/ text :start start)
        for piece = (subseq text start pos)
        unless (string= piece "") collect piece
        while pos do (setf start (1+ pos))))

;;; -------------------------------------------------------------------- glob

(defun map-glob (spec function)
  "Call FUNCTION on each path string matching SPEC, depth first with each
directory's names in order.  Nothing is collected, so a caller that stops early
stops the walk -- see WALK-GLOB.

Ordering is per directory rather than over the whole result, since there is no
whole result to sort.  On a real tree the two agree: over /usr/share/man's 2962
files they are byte-identical.  They differ only where a directory name is a
prefix of a sibling file name, which puts c/d.txt before c.txt."
  ;; A bare .name is the field-accessor shorthand, so a dotfile written without
  ;; quotes arrives here as a block instead of a path.  Saying so beats "the
  ;; value #<FUNCTION (LAMBDA (IT))> is not of type ..." by a wide margin.
  (when (functionp spec)
    (error "A bare .name is a field accessor, so a dotfile needs quoting: ~
write (ls \".gitignore\") -- in word mode, ls \".gitignore\"."))
  (let* ((text (if (pathnamep spec) (sb-ext:native-namestring spec) (string spec)))
         (absolute (and (plusp (length text)) (char= (char text 0) #\/)))
         (root (if absolute "/" (sb-ext:native-namestring *default-pathname-defaults*)))
         (components (split-path-components text)))
    (cond
      ((null components)
       (dolist (name (read-directory-names root))
         (funcall function (join-path root name))))
      ((glob-pattern-p text) (walk-glob root components function))
      (t
       ;; No metacharacters: a directory lists its members, a file is itself.
       ;; Without the first case (ls "src") would name src rather than what is
       ;; in it.
       (let ((path (unescape-glob (reduce #'join-path components :initial-value root))))
         (cond ((directory-string-p path)
                (dolist (name (read-directory-names path))
                  (funcall function (join-path path name))))
               ((ignore-errors (file-stat path)) (funcall function path)))))))
  (values))

(defun glob (spec)
  "Pathnames matching SPEC, in the order MAP-GLOB yields them.

A pattern containing * ? or [...] globs and ** descends; a directory lists its
members; anything else names itself.  Collected here rather than streamed, so
that callers wanting a list still have one -- LS does not use this."
  (let ((paths '()))
    (map-glob spec (lambda (path) (push path paths)))
    (mapcar #'sb-ext:parse-native-namestring (nreverse paths))))
