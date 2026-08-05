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
;;;
;;; Patterns are parsed to a small tree once per component and matched with
;;; backtracking, rather than walked character by character.  Alternation,
;;; closure and negation all need to retry, and a char walk cannot.
;;;
;;; Grammar, loosest binding first:
;;;
;;;   pattern  := alt ( '~' alt )*          zsh exclusion: match the first,
;;;                                         reject anything the rest match
;;;   alt      := branch ( '|' branch )*
;;;   branch   := [ '^' ] seq               zsh negation of the whole branch
;;;   seq      := postfix*
;;;   postfix  := atom [ '#' | '##' ]       zsh closure: zero-or-more, one-or-more
;;;   atom     := '*' | '?' | '[' set ']' | '(' alt ')' | extglob
;;;             | '<' n '-' m '>' | '(#' flags ')' | '\' char | char
;;;   extglob  := ( '?' | '*' | '+' | '@' | '!' ) '(' alt ')'      bash

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

;;; ---- parser

(defstruct (glob-parser (:conc-name gp-))
  (text "" :type string)
  (position 0 :type fixnum)
  (fold nil))                           ; (#i) seen: match case-insensitively

(defun gp-peek (p &optional (ahead 0))
  (let ((i (+ (gp-position p) ahead)))
    (when (< i (length (gp-text p))) (char (gp-text p) i))))

(defun gp-next (p) (prog1 (gp-peek p) (incf (gp-position p))))
(defun gp-eat (p char) (when (eql (gp-peek p) char) (incf (gp-position p)) t))

(defun parse-bracket-set (p)
  "[...] -- ranges, negation, POSIX classes, collating and equivalence.

[.x.] and [=x=] degenerate to the literal character: they only differ from it
under a collating locale, and there is none here."
  (let ((items '()) (negated nil) (first t))
    (when (or (eql (gp-peek p) #\!) (eql (gp-peek p) #\^))
      (setf negated t)
      (gp-next p))
    (loop
      (let ((c (gp-peek p)))
        (cond
          ;; Unterminated: a shell treats the [ as an ordinary character, so
          ;; give the caller its position back and say so.
          ((null c) (return-from parse-bracket-set nil))
          ((and (char= c #\]) (not first)) (gp-next p) (return))
          ;; [:alpha:] [.x.] [=x=]
          ((and (char= c #\[) (member (gp-peek p 1) '(#\: #\. #\=)))
           (let* ((kind (gp-peek p 1))
                  (close (format nil "~c]" kind))
                  (at (search close (gp-text p) :start2 (+ (gp-position p) 2))))
             (cond
               ((null at) (push c items) (gp-next p))
               (t (let ((body (subseq (gp-text p) (+ (gp-position p) 2) at)))
                    (setf (gp-position p) (+ at 2))
                    (if (char= kind #\:)
                        (let ((class (cdr (assoc body +posix-character-classes+
                                                 :test #'string-equal))))
                          (if class
                              (push (list :class class) items)
                              ;; unknown class: the characters, literally
                              (map nil (lambda (ch) (push ch items)) body)))
                        ;; collating or equivalence: the literal character(s)
                        (map nil (lambda (ch) (push ch items)) body)))))))
          ;; a-z, but a - just before ] is itself literal
          ((and (eql (gp-peek p 1) #\-) (gp-peek p 2) (char/= (gp-peek p 2) #\]))
           (let ((lo (gp-next p)))
             (gp-next p)
             (push (list :range lo (gp-next p)) items)))
          ((char= c #\\)
           (gp-next p)
           (let ((escaped (gp-next p))) (when escaped (push escaped items))))
          (t (push (gp-next p) items))))
      (setf first nil))
    (list :set negated (nreverse items))))

(defun parse-numeric-range (p)
  "<n-m>, <-m>, <n->, <> -- a run of digits whose value is in range."
  (let ((close (position #\> (gp-text p) :start (gp-position p))))
    (if (null close)
        (list :literal #\<)                     ; not a range after all
        (let* ((body (subseq (gp-text p) (gp-position p) close))
               (dash (position #\- body)))
          (setf (gp-position p) (1+ close))
          (if (null dash)
              (let ((n (parse-integer body :junk-allowed t)))
                (list :number n n))
              (list :number
                    (parse-integer body :end dash :junk-allowed t)
                    (parse-integer body :start (1+ dash) :junk-allowed t)))))))

(defun parse-glob-flags (p)
  "(#i) and friends.  Only the case flags mean anything here; (#q...) is the
qualifier form and is handled by the caller, not by the matcher."
  (loop for c = (gp-peek p)
        until (or (null c) (char= c #\)))
        do (case (char-downcase c)
             (#\i (setf (gp-fold p) t))
             (#\l (setf (gp-fold p) t))
             (t nil))
           (gp-next p))
  (gp-eat p #\))
  (list :seq))                          ; a flag matches nothing itself

(defun parse-glob-atom (p)
  (let ((c (gp-next p)))
    (case c
      (#\* (list :any))
      (#\? (list :one))
      (#\[ (let ((start (gp-position p)))
             (or (parse-bracket-set p)
                 (progn (setf (gp-position p) start) (list :literal #\[)))))
      (#\< (parse-numeric-range p))
      (#\\ (let ((escaped (gp-next p)))
             (if escaped (list :literal escaped) (list :literal #\\))))
      (#\( (if (eql (gp-peek p) #\#)
               (progn (gp-next p) (parse-glob-flags p))
               (prog1 (parse-glob-alternation p) (gp-eat p #\)))))
      (t (list :literal c)))))

(defun extglob-prefix-p (p c)
  (and (member c '(#\? #\* #\+ #\@ #\!)) (eql (gp-peek p) #\()))

(defun parse-glob-postfix (p)
  (let* ((c (gp-peek p))
         (node
           (cond
             ;; bash extglob: ?(p) *(p) +(p) @(p) !(p)
             ((and c (progn (gp-next p) (extglob-prefix-p p c)))
              (gp-next p)                          ; the (
              (let ((inner (parse-glob-alternation p)))
                (gp-eat p #\))
                (case c
                  (#\? (list :closure inner 0 1))
                  (#\* (list :closure inner 0 nil))
                  (#\+ (list :closure inner 1 nil))
                  (#\@ inner)
                  (#\! (list :not inner)))))
             (t (decf (gp-position p)) (parse-glob-atom p)))))
    ;; zsh closure: x# is zero or more, x## one or more
    (loop while (eql (gp-peek p) #\#)
          do (gp-next p)
             (if (eql (gp-peek p) #\#)
                 (progn (gp-next p) (setf node (list :closure node 1 nil)))
                 (setf node (list :closure node 0 nil))))
    node))

(defun parse-glob-sequence (p)
  (let ((negated (gp-eat p #\^))
        (nodes '()))
    (loop for c = (gp-peek p)
          until (or (null c) (member c '(#\| #\) #\~)))
          do (push (parse-glob-postfix p) nodes))
    (let ((seq (cons :seq (nreverse nodes))))
      (if negated (list :not seq) seq))))

(defun parse-glob-alternation (p)
  (let ((branches (list (parse-glob-sequence p))))
    (loop while (gp-eat p #\|)
          do (push (parse-glob-sequence p) branches))
    (if (rest branches) (cons :alt (nreverse branches)) (first branches))))

(defun parse-glob (text)
  "TEXT as a match tree, plus whether matching folds case."
  (let ((p (make-glob-parser :text text)))
    (let ((main (parse-glob-alternation p))
          (excluded '()))
      ;; zsh p1~p2: match p1, then reject anything p2 also matches.
      (loop while (gp-eat p #\~)
            do (push (parse-glob-alternation p) excluded))
      (values (if excluded
                  (list* :except main (nreverse excluded))
                  main)
              (gp-fold p)))))

;;; ---- matcher

(defun glob-chars-equal (a b fold)
  (if fold (char-equal a b) (char= a b)))

(defun match-node (node name start fold k)
  "Match NODE against NAME from START, calling K with each end position it can
reach.  Continuation passing, because *, closure and alternation must be able
to give up a choice and try another."
  (let ((length (length name)))
    (ecase (first node)
      (:seq (match-sequence (rest node) name start fold k))
      (:literal
       (and (< start length)
            (glob-chars-equal (second node) (char name start) fold)
            (funcall k (1+ start))))
      (:one (and (< start length) (funcall k (1+ start))))
      (:any (loop for end from length downto start
                    thereis (funcall k end)))
      (:set
       (and (< start length)
            (let ((matched (set-contains-p (third node) (char name start) fold)))
              (and (if (second node) (not matched) matched)
                   (funcall k (1+ start))))))
      (:alt (loop for branch in (rest node)
                    thereis (match-node branch name start fold k)))
      (:number
       (destructuring-bind (lo hi) (rest node)
         (loop for end from (1+ start) to length
               while (every #'digit-char-p (subseq name start end))
               thereis (let ((value (parse-integer name :start start :end end)))
                         (and (or (null lo) (>= value lo))
                              (or (null hi) (<= value hi))
                              (funcall k end))))))
      (:closure
       (destructuring-bind (inner minimum maximum) (rest node)
         (match-closure inner minimum maximum name start fold k)))
      (:not
       ;; Any span the inner pattern does NOT match.  bash's !(p) and zsh's ^p.
       (loop for end from start to length
             thereis (and (not (match-node (second node) name start fold
                                           (lambda (e) (= e end))))
                          (funcall k end))))
      (:except
       (and (match-node (second node) name start fold
                        (lambda (e) (= e length)))
            (notany (lambda (excluded)
                      (match-node excluded name start fold (lambda (e) (= e length))))
                    (cddr node))
            (funcall k length))))))

(defun match-sequence (nodes name start fold k)
  (if (null nodes)
      (funcall k start)
      (match-node (first nodes) name start fold
                  (lambda (next) (match-sequence (rest nodes) name next fold k)))))

(defun match-closure (inner minimum maximum name start fold k)
  (labels ((try (position count)
             (or (and (>= count minimum) (funcall k position))
                 (and (or (null maximum) (< count maximum))
                      (match-node inner name position fold
                                  (lambda (next)
                                    ;; A zero-width match would spin forever.
                                    (and (> next position) (try next (1+ count)))))))))
    (try start 0)))

(defun set-contains-p (items char fold)
  (loop for item in items
          thereis (cond
                    ((characterp item) (glob-chars-equal item char fold))
                    ((eq (first item) :range)
                     (if fold
                         (or (char<= (char-downcase (second item)) (char-downcase char)
                                     (char-downcase (third item)))
                             (char<= (char-upcase (second item)) (char-upcase char)
                                     (char-upcase (third item))))
                         (char<= (second item) char (third item))))
                    ((eq (first item) :class)
                     (character-class-match (second item) char)))))

(defvar *glob-ignore-case* nil
  "Match without regard to case, like bash's nocaseglob.")

(defvar *glob-match-dotfiles* nil
  "Let * find names beginning with a dot, like bash's dotglob.")

(defun glob-match (pattern name)
  "Does NAME match the single-component PATTERN?

A leading dot must be matched explicitly, as in a shell: * does not find
.hidden unless *GLOB-MATCH-DOTFILES* says otherwise."
  (when (and (plusp (length name)) (char= (char name 0) #\.)
             (not *glob-match-dotfiles*)
             (not (and (plusp (length pattern)) (char= (char pattern 0) #\.))))
    (return-from glob-match nil))
  (multiple-value-bind (tree fold) (parse-glob pattern)
    (let ((length (length name)))
      (and (match-node tree name 0 (or fold *glob-ignore-case*)
                       (lambda (end) (= end length)))
           t))))

(defun glob-pattern-p (text)
  "Does TEXT contain an unescaped metacharacter?"
  (loop with i = 0
        while (< i (length text))
        do (case (char text i)
             (#\\ (incf i 2))
             ((#\* #\? #\[ #\( #\| #\< #\^ #\~ #\#) (return t))
             (t (incf i)))
        finally (return nil)))

;;; ---------------------------------------------------------- brace expansion
;;;
;;; Not glob: a separate expansion that runs first and produces several
;;; patterns, each globbed in turn.  {a,b} {1..9} {a..e} {1..9..2}, nested and
;;; adjacent.  A group with no top-level comma and no .. is left alone, so a
;;; directory literally called {x} still works.

(defun find-brace-group (text &optional (from 0))
  "Positions of the first { and its matching }, or NIL."
  (loop with depth = 0 with open = nil
        with i = from
        while (< i (length text))
        do (case (char text i)
             (#\\ (incf i))
             (#\{ (when (zerop depth) (setf open i)) (incf depth))
             (#\} (decf depth)
                  (when (and (zerop depth) open)
                    (return-from find-brace-group (values open i)))))
           (incf i)
        finally (return nil)))

(defun split-top-level (text separator)
  "Split on SEPARATOR at brace depth zero."
  (let ((parts '()) (start 0) (depth 0))
    (loop with i = 0
          while (< i (length text))
          do (let ((c (char text i)))
               (cond ((char= c #\\) (incf i))
                     ((char= c #\{) (incf depth))
                     ((char= c #\}) (decf depth))
                     ((and (char= c separator) (zerop depth))
                      (push (subseq text start i) parts)
                      (setf start (1+ i)))))
             (incf i))
    (push (subseq text start) parts)
    (nreverse parts)))

(defun brace-range (body)
  "{1..9}, {1..9..2}, {a..e} as a list of strings, or NIL."
  (let ((pieces (split-sequence-dots body)))
    (when (member (length pieces) '(2 3))
      (destructuring-bind (from to &optional step) pieces
        (let ((low (parse-integer from :junk-allowed t))
              (high (parse-integer to :junk-allowed t))
              (by (abs (or (and step (parse-integer step :junk-allowed t)) 1))))
          (cond
            ((and low high (plusp by))
             (loop for n = low then (if (<= low high) (+ n by) (- n by))
                   while (if (<= low high) (<= n high) (>= n high))
                   collect (princ-to-string n)))
            ;; {a..e}
            ((and (= 1 (length from)) (= 1 (length to)) (plusp by))
             (let ((a (char-code (char from 0))) (b (char-code (char to 0))))
               (loop for c = a then (if (<= a b) (+ c by) (- c by))
                     while (if (<= a b) (<= c b) (>= c b))
                     collect (string (code-char c)))))))))))

(defun split-sequence-dots (text)
  (let ((parts '()) (start 0) (i 0))
    (loop while (< i (length text))
          do (if (and (< (1+ i) (length text))
                      (char= (char text i) #\.) (char= (char text (1+ i)) #\.))
                 (progn (push (subseq text start i) parts) (incf i 2) (setf start i))
                 (incf i)))
    (push (subseq text start) parts)
    (nreverse parts)))

(defun brace-alternatives (body)
  (let ((commas (split-top-level body #\,)))
    (cond ((rest commas) commas)
          ((brace-range body))
          (t nil))))                    ; not a group after all

(defun expand-braces (text)
  "TEXT as the list of strings brace expansion produces."
  (labels ((expand (text from)
             (multiple-value-bind (open close) (find-brace-group text from)
               (if (null open)
                   (list text)
                   (let ((alternatives (brace-alternatives (subseq text (1+ open) close))))
                     (if (null alternatives)
                         ;; A literal {x}: step past it rather than spinning.
                         (expand text (1+ close))
                         (loop for alternative in alternatives
                               append (expand (concatenate 'string
                                                           (subseq text 0 open)
                                                           alternative
                                                           (subseq text (1+ close)))
                                              0))))))))
    (expand text 0)))

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
    ;; ** matches zero or more directory levels; *** is zsh's follow-symlinks
    ;; form of it.
    ((or (string= (first components) "**") (string= (first components) "***"))
     (let ((follow (string= (first components) "***")))
       (append (walk-glob directory (rest components))
               (loop for name in (read-directory-names directory)
                     for child = (join-path directory name)
                     ;; ** does not follow: a symlink pointing back up a tree
                     ;; would otherwise recurse until the stack gave up.  ***
                     ;; asks for that risk deliberately.
                     when (directory-string-p child :follow follow)
                       append (walk-glob child components)))))
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

(defun glob-one (text)
  "One brace-free pattern as a list of path strings."
  (multiple-value-bind (pattern qualifier-text) (split-glob-qualifiers text)
    (let* ((text (or pattern text))
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
      (if qualifier-text
          (let ((qualifiers (parse-qualifiers qualifier-text)))
            (if qualifiers
                (apply-qualifiers (sort paths #'string<) qualifiers)
                paths))
          paths))))

(defun glob (spec)
  "Pathnames matching SPEC, sorted so output is stable.

Braces expand first and each result is globbed in turn; a pattern containing
* ? [...] (...) <n-m> ^ ~ or # matches; ** descends; a trailing (...) is a
qualifier list; a directory lists its members; anything else names itself."
  ;; A bare .name is the field-accessor shorthand, so a dotfile written without
  ;; quotes arrives here as a block instead of a path.  Saying so beats "the
  ;; value #<FUNCTION (LAMBDA (IT))> is not of type ..." by a wide margin.
  (when (functionp spec)
    (error "A bare .name is a field accessor, so a dotfile needs quoting: ~
write (ls \".gitignore\") -- in word mode, ls \".gitignore\"."))
  (let* ((text (if (pathnamep spec) (sb-ext:native-namestring spec) (string spec)))
         (ordered (some (lambda (p) (nth-value 1 (split-glob-qualifiers p)))
                        (expand-braces text)))
         (paths (loop for pattern in (expand-braces text)
                      append (glob-one pattern))))
    (setf paths (remove-duplicates paths :test #'string=))
    ;; A qualifier may have ordered or subscripted the result, and re-sorting
    ;; would throw that away.
    (mapcar #'sb-ext:parse-native-namestring
            (if ordered paths (sort paths #'string<)))))
