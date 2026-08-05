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

(defun glob-set-match (pattern start char)
  "Match CHAR against the [...] starting at START.  Returns whether it matched
and the index just past the closing bracket.

Supports ranges (a-z) and negation (! or ^), neither of which CL pathname
patterns had.  A ] first in the set is literal, as in every shell."
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
  "Entry names in DIRECTORY as plain strings.  Nothing here goes through a
pathname, which is the whole point: a name is whatever bytes the filesystem
holds, not something to be re-parsed."
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
    names))

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

(defun walk-glob (directory components)
  "Paths under DIRECTORY matching the remaining pattern COMPONENTS."
  (cond
    ((null components) (list directory))
    ;; ** matches zero or more directory levels.
    ((string= (first components) "**")
     (append (walk-glob directory (rest components))
             (loop for name in (read-directory-names directory)
                   for child = (join-path directory name)
                   ;; :FOLLOW NIL here and only here: a symlink pointing back
                   ;; up a tree would otherwise recurse until the stack gave up.
                   when (directory-string-p child :follow nil)
                     append (walk-glob child components))))
    (t
     (let ((component (first components))
           (rest (rest components)))
       (if (glob-pattern-p component)
           (loop for name in (read-directory-names directory)
                 when (glob-match component name)
                   append (let ((child (join-path directory name)))
                            (if rest
                                (when (directory-string-p child) (walk-glob child rest))
                                (list child))))
           ;; A literal component needs no scan; just descend.
           (let ((child (join-path directory (unescape-glob component))))
             (cond (rest (when (directory-string-p child) (walk-glob child rest)))
                   ((ignore-errors (file-stat child)) (list child)))))))))

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

(defun glob (spec)
  "Pathnames matching SPEC, sorted so output is stable.  A pattern containing
* ? or [...] globs and ** descends; a directory lists its members; anything
else names itself."
  ;; A bare .name is the field-accessor shorthand, so a dotfile written without
  ;; quotes arrives here as a block instead of a path.  Saying so beats "the
  ;; value #<FUNCTION (LAMBDA (IT))> is not of type ..." by a wide margin.
  (when (functionp spec)
    (error "A bare .name is a field accessor, so a dotfile needs quoting: ~
write (ls \".gitignore\") -- in word mode, ls \".gitignore\"."))
  (let* ((text (if (pathnamep spec)
                   (sb-ext:native-namestring spec)
                   (string spec)))
         (absolute (and (plusp (length text)) (char= (char text 0) #\/)))
         (root (if absolute "/" (sb-ext:native-namestring *default-pathname-defaults*)))
         (components (split-path-components text))
         (paths
           (cond
             ((null components) (mapcar (lambda (n) (join-path root n))
                                        (read-directory-names root)))
             ((glob-pattern-p text) (walk-glob root components))
             (t
              ;; No metacharacters: a directory lists its members, a file is
              ;; itself.  Without the first case (ls "src") would name src
              ;; rather than what is in it.
              (let ((path (unescape-glob
                           (reduce #'join-path components :initial-value root))))
                (cond ((directory-string-p path)
                       (mapcar (lambda (n) (join-path path n))
                               (read-directory-names path)))
                      ((ignore-errors (file-stat path)) (list path))))))))
    (mapcar #'sb-ext:parse-native-namestring (sort paths #'string<))))
