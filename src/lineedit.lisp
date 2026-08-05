;;;; lineedit.lisp -- a small line editor with emacs keys, for the REPL.
;;;;
;;;; Design decisions, so they do not have to be rediscovered:
;;;;
;;;; - Raw mode is entered per line and left before anything is evaluated.  A
;;;;   pipeline therefore runs with signals in their normal state, so ^C still
;;;;   tears one down; during editing ISIG is off and ^C is just a key that
;;;;   abandons the line, which is what readline does.
;;;;
;;;; - Display is ONE row with horizontal scrolling, never a wrapped multi-row
;;;;   region.  Plumb forms are long, and a one-row window is the difference
;;;;   between arithmetic that is obviously right and arithmetic that corrupts
;;;;   the screen at exactly the wrong moment.
;;;;
;;;; - Terminal width comes from STTY, cached and invalidated by SIGWINCH.  See
;;;;   the note above QUERY-COLUMNS for why not the usual ESC[6n round trip.
;;;;
;;;; - Word motion is symbol-aware rather than readline's alphanumeric-only:
;;;;   M-b over *default-capacity* should be one word, not three.
;;;;
;;;; Colour lives in src/ansi.lisp: HELP wants it too.

(in-package #:plumb.lineedit)

;;; -------------------------------------------------------------- the terminal

(defun tty-p ()
  (ignore-errors
   (and (interactive-stream-p *standard-input*)
        (interactive-stream-p *standard-output*))))

(defmacro with-raw-terminal ((&optional (fd 0)) &body body)
  (let ((f (gensym "FD")) (saved (gensym "SAVED")) (raw (gensym "RAW")))
    `(let* ((,f ,fd)
            (,saved (sb-posix:tcgetattr ,f))
            (,raw (sb-posix:tcgetattr ,f)))
       (setf (sb-posix:termios-lflag ,raw)
             (logandc2 (sb-posix:termios-lflag ,raw)
                       (logior sb-posix:icanon sb-posix:echo
                               sb-posix:isig sb-posix:iexten))
             (sb-posix:termios-iflag ,raw)
             (logandc2 (sb-posix:termios-iflag ,raw) sb-posix:ixon)
             (aref (sb-posix:termios-cc ,raw) sb-posix:vmin) 1
             (aref (sb-posix:termios-cc ,raw) sb-posix:vtime) 0)
       ;; TCSADRAIN, not TCSAFLUSH: the mode is toggled around every line, and
       ;; flushing would throw away type-ahead -- which is what a pasted
       ;; multi-line form looks like from here.
       (unwind-protect
            (progn (sb-posix:tcsetattr ,f sb-posix:tcsadrain ,raw)
                   ,@body)
         ;; Never leave the user without an echo.
         (ignore-errors (sb-posix:tcsetattr ,f sb-posix:tcsadrain ,saved))))))

(defun read-key-char (&optional timeout)
  "One character.  With TIMEOUT (seconds), NIL if none arrives in time."
  (or (read-char-no-hang *standard-input* nil nil)
      (if timeout
          (when (sb-sys:wait-until-fd-usable 0 :input timeout)
            (read-char-no-hang *standard-input* nil nil))
          (read-char *standard-input* nil nil))))

;;; ------------------------------------------------------------ key decoding

(defun decode-csi ()
  (let ((params (make-string-output-stream)))
    (loop for c = (read-key-char 0.05)
          do (cond ((null c) (return :unknown))
                   ((or (digit-char-p c) (char= c #\;)) (write-char c params))
                   (t (return (csi-key c (get-output-stream-string params))))))))

(defun csi-key (final params)
  (let ((modified (search ";5" params)))    ; ";5" is the control modifier
    (case final
      (#\A :up) (#\B :down)
      (#\C (if modified :ctrl-right :right))
      (#\D (if modified :ctrl-left :left))
      (#\H :home) (#\F :end)
      (#\~ (cond ((string= params "1") :home)
                 ((string= params "3") :delete)
                 ((string= params "4") :end)
                 ((string= params "7") :home)
                 ((string= params "8") :end)
                 (t :unknown)))
      (t :unknown))))

(defun decode-ss3 ()
  (let ((c (read-key-char 0.05)))
    (case c
      (#\A :up) (#\B :down) (#\C :right) (#\D :left)
      (#\H :home) (#\F :end)
      (t :unknown))))

(defun read-key ()
  "A character, a keyword (:LEFT :HOME ...), or (:META . character).
NIL at end of input."
  (let ((c (read-key-char)))
    (cond ((null c) nil)
          ((char/= c +esc+) c)
          (t (let ((next (read-key-char 0.05)))   ; a lone ESC is just ESC
               (cond ((null next) +esc+)
                     ((char= next #\[) (decode-csi))
                     ((char= next #\O) (decode-ss3))
                     (t (cons :meta next))))))))

;;; ------------------------------------------------------------------ history

(defvar *history* (make-array 0 :adjustable t :fill-pointer 0)
  "Accepted lines, oldest first.  Persisted to *HISTORY-FILE*.")

(defvar *history-limit* 500)

(defvar *history-file*
  (merge-pathnames ".plumb_history" (user-homedir-pathname))
  "Where history persists between sessions.  NIL disables persistence.")

(defun load-history (&optional (path *history-file*))
  "Fill *HISTORY* from PATH, keeping the most recent *HISTORY-LIMIT* lines.
Rewrites the file when it has grown well past the limit, so an append-only log
cannot grow without bound.  A missing or unreadable file is not an error --
losing history is never worth failing to start over."
  (when path
    (ignore-errors
     (let ((lines (with-open-file (in path :if-does-not-exist nil)
                    (when in
                      (loop for line = (read-line in nil nil)
                            while line
                            unless (string= line "") collect line)))))
       (when lines
         (let ((recent (last lines *history-limit*)))
           (setf (fill-pointer *history*) 0)
           (dolist (line recent) (vector-push-extend line *history*))
           (when (> (length lines) (* 2 *history-limit*))
             (with-open-file (out path :direction :output :if-exists :supersede
                                       :if-does-not-exist :create)
               (dolist (line recent) (write-line line out))))))))
    (length *history*)))

(defun append-history (line &optional (path *history-file*))
  "Append one accepted line.  Appending rather than rewriting at exit means a
crash keeps the history, and two sessions interleave instead of clobbering."
  (when (and path (plusp (length line)))
    (ignore-errors
     (with-open-file (out path :direction :output :if-exists :append
                               :if-does-not-exist :create)
       (write-line line out))))
  line)

(defun add-history (line)
  "Record LINE unless it is blank or repeats the previous entry.

Newlines become spaces, the way bash stores a multi-line command as one entry:
the display is a single row, and a recalled form carrying a real newline would
run off the end of it.  The caveat is bash's too -- a newline inside a string
literal does not survive the round trip."
  (let ((line (substitute #\Space #\Newline
                          (string-trim '(#\Space #\Tab #\Newline) line))))
    (unless (or (string= line "")
                (and (plusp (fill-pointer *history*))
                     (string= line (aref *history* (1- (fill-pointer *history*))))))
      (vector-push-extend line *history*)
      (append-history line)
      (when (> (fill-pointer *history*) *history-limit*)
        (replace *history* *history* :start2 1)
        (decf (fill-pointer *history*)))))
  line)

;;; --------------------------------------------------------------- completion

(defvar *completer* nil
  "A function of one argument -- the word before point -- returning the strings
it could be completed to.  NIL leaves TAB inserting whitespace.  The editor
deliberately knows nothing about what is being completed; the CLI installs one
that knows about stages.")

(defun common-prefix (strings)
  (if (null (rest strings))
      (first strings)
      (let ((end (reduce #'min strings :key #'length)))
        (dotimes (i end (subseq (first strings) 0 end))
          (unless (every (lambda (s) (char-equal (char (first strings) i) (char s i)))
                         strings)
            (return (subseq (first strings) 0 i)))))))

;;; ------------------------------------------------------------------- editor

(defstruct (editor (:conc-name ed-) (:copier nil))
  (text (make-array 32 :element-type 'character :adjustable t :fill-pointer 0))
  (point 0 :type fixnum)
  (start 0 :type fixnum)                ; leftmost visible column of TEXT
  (prompt "")
  (width 0 :type fixnum)                ; visible width of PROMPT
  (columns 80 :type fixnum)
  (history #())
  (index nil)                           ; NIL while editing the live line
  (stash nil)                           ; live line parked during history browsing
  (kill ""))

(defun ed-string (ed) (coerce (ed-text ed) 'simple-string))
(defun ed-length (ed) (fill-pointer (ed-text ed)))

(defun ed-insert (ed string)
  (let* ((text (ed-text ed))
         (n (length string))
         (p (ed-point ed)))
    (dotimes (i n) (vector-push-extend #\Space text))
    ;; Overlapping REPLACE on one sequence is defined to behave as if copied.
    (replace text text :start1 (+ p n) :start2 p)
    (replace text string :start1 p)
    (incf (ed-point ed) n)))

(defun ed-delete (ed start end &key kill)
  "Remove [START, END).  With KILL, the removed text goes to the kill ring."
  (let* ((text (ed-text ed))
         (len (fill-pointer text))
         (start (max 0 (min start len)))
         (end (max 0 (min end len))))
    (when (< start end)
      (when kill (setf (ed-kill ed) (subseq text start end)))
      (replace text text :start1 start :start2 end)
      (decf (fill-pointer text) (- end start))
      (setf (ed-point ed)
            (cond ((<= (ed-point ed) start) (ed-point ed))
                  ((>= (ed-point ed) end) (- (ed-point ed) (- end start)))
                  (t start))))))

(defun ed-replace-all (ed string)
  (setf (fill-pointer (ed-text ed)) 0
        (ed-point ed) 0)
  (ed-insert ed string))

;;; Word motion.  WORD-CHAR-P is symbol-aware; C-w keeps readline's whitespace
;;; rule, which is the more useful of the two when killing an argument.

(defun word-char-p (c)
  (or (alphanumericp c) (find c "-+*/<>=?!_&%$:.")))

(defun backward-word-pos (ed &optional (word-p #'word-char-p))
  (let ((text (ed-text ed)) (p (ed-point ed)))
    (loop while (and (plusp p) (not (funcall word-p (char text (1- p))))) do (decf p))
    (loop while (and (plusp p) (funcall word-p (char text (1- p)))) do (decf p))
    p))

(defun forward-word-pos (ed &optional (word-p #'word-char-p))
  (let* ((text (ed-text ed)) (len (fill-pointer text)) (p (ed-point ed)))
    (loop while (and (< p len) (not (funcall word-p (char text p)))) do (incf p))
    (loop while (and (< p len) (funcall word-p (char text p))) do (incf p))
    p))

(defun map-word (ed function)
  "Apply FUNCTION to the characters of the word ahead of point, and move past it."
  (let ((end (forward-word-pos ed)) (text (ed-text ed)))
    (loop for i from (ed-point ed) below end
          do (setf (char text i) (funcall function (char text i))))
    (setf (ed-point ed) end)))

;;; History browsing.  The line being typed is stashed on the way into the
;;; history and restored on the way back out, so C-p C-n is a no-op.

(defun history-move (ed delta)
  (let* ((h (ed-history ed))
         (n (length h)))
    (when (plusp n)
      (unless (ed-index ed)
        (setf (ed-stash ed) (ed-string ed)
              (ed-index ed) n))
      (let ((i (+ (ed-index ed) delta)))
        (cond ((< i 0))                 ; already at the oldest entry
              ((>= i n)
               (setf (ed-index ed) nil)
               (ed-replace-all ed (or (ed-stash ed) ""))
               (setf (ed-point ed) (ed-length ed)))
              (t
               (setf (ed-index ed) i)
               (ed-replace-all ed (aref h i))
               (setf (ed-point ed) (ed-length ed))))))))

;;; ---------------------------------------------------------------- redisplay

(defun redisplay (ed)
  (let* ((out *standard-output*)
         (len (ed-length ed))
         (avail (max 1 (- (ed-columns ed) (ed-width ed) 1)))
         (start (ed-start ed)))
    ;; Scroll the window the minimum needed to keep point on screen.
    (when (< (ed-point ed) start) (setf start (ed-point ed)))
    (when (>= (ed-point ed) (+ start avail)) (setf start (1+ (- (ed-point ed) avail))))
    (when (and (plusp start) (< (- len start) avail))
      (setf start (max 0 (- len avail -1))))
    (setf (ed-start ed) start)
    (write-char #\Return out)
    (write-string (ed-prompt ed) out)
    (write-string (subseq (ed-text ed) start (min len (+ start avail))) out)
    (format out "~c[K" +esc+)           ; erase whatever the old line left
    (write-char #\Return out)
    (let ((col (+ (ed-width ed) (- (ed-point ed) start))))
      (when (plusp col) (format out "~c[~dC" +esc+ col)))
    (force-output out)))

;;; --------------------------------------------------------------- the reader

(defvar *prompt* "> "
  "A string, or a function of no arguments returning one.  Set by the CLI to
PLUMB.CLI::PLUMB-PROMPT; bind or SETF it to change the prompt.")

(defvar *continuation-prompt* "... "
  "Shown while a form is still unbalanced.  String or function, as *PROMPT*.")

(defun prompt-text (prompt)
  (let ((p (if (or (functionp prompt)
                   (and prompt (symbolp prompt) (fboundp prompt)))
               (funcall prompt)
               prompt)))
    (typecase p (string p) (null "") (t (princ-to-string p)))))

(defun read-line-edited (&key (prompt *prompt*) (history *history*))
  "Read one line.  Returns (VALUES STRING STATUS), STATUS being :LINE, :EOF
(^D on an empty line, or a closed stream) or :INTERRUPT (^C).

Falls back to READ-LINE when standard input is not a terminal, so the same
call works under a pipe."
  (let ((text (prompt-text prompt)))
    (if (not (tty-p))
        (let ((line (read-line *standard-input* nil nil)))
          (if line (values line :line) (values "" :eof)))
        (with-raw-terminal ()
          (%edit text history)))))

(defun %edit (prompt history)
  (let ((ed (make-editor :prompt prompt
                         :width (visible-width prompt)
                         :history history
                         :columns (terminal-width))))
    (flet ((done (status)
             (write-char #\Newline *standard-output*)
             (force-output *standard-output*)
             (return-from %edit (values (ed-string ed) status))))
      (redisplay ed)
      (loop
        (let ((key (read-key)))
          (cond
            ((null key) (done :eof))
            ;; ---- named keys
            ((eq key :left)       (setf (ed-point ed) (max 0 (1- (ed-point ed)))))
            ((eq key :right)      (setf (ed-point ed) (min (ed-length ed) (1+ (ed-point ed)))))
            ((eq key :up)         (history-move ed -1))
            ((eq key :down)       (history-move ed +1))
            ((eq key :home)       (setf (ed-point ed) 0))
            ((eq key :end)        (setf (ed-point ed) (ed-length ed)))
            ((eq key :delete)     (ed-delete ed (ed-point ed) (1+ (ed-point ed))))
            ((eq key :ctrl-left)  (setf (ed-point ed) (backward-word-pos ed)))
            ((eq key :ctrl-right) (setf (ed-point ed) (forward-word-pos ed)))
            ((eq key :unknown))
            ;; ---- meta
            ((consp key)
             (let ((c (char-downcase (cdr key))))
               (case c
                 (#\b (setf (ed-point ed) (backward-word-pos ed)))
                 (#\f (setf (ed-point ed) (forward-word-pos ed)))
                 (#\d (ed-delete ed (ed-point ed) (forward-word-pos ed) :kill t))
                 (#\u (map-word ed #'char-upcase))
                 (#\l (map-word ed #'char-downcase))
                 (#\c (let ((p (ed-point ed)))
                        (map-word ed #'char-downcase)
                        (let ((s (position-if #'alpha-char-p (ed-text ed) :start p)))
                          (when (and s (< s (ed-point ed)))
                            (setf (char (ed-text ed) s) (char-upcase (char (ed-text ed) s)))))))
                 (#\< (history-move ed (- (length history))))
                 (#\> (history-move ed (length history)))
                 (t (when (eql (cdr key) (code-char 127))   ; M-DEL
                      (ed-delete ed (backward-word-pos ed) (ed-point ed) :kill t))))))
            ;; ---- control and printable characters
            ((characterp key)
             (case (char-code key)
               (1  (setf (ed-point ed) 0))                                   ; C-a
               (2  (setf (ed-point ed) (max 0 (1- (ed-point ed)))))           ; C-b
               (3  (write-string "^C" *standard-output*) (done :interrupt))   ; C-c
               (4  (if (zerop (ed-length ed))                                 ; C-d
                       (done :eof)
                       (ed-delete ed (ed-point ed) (1+ (ed-point ed)))))
               (5  (setf (ed-point ed) (ed-length ed)))                       ; C-e
               (6  (setf (ed-point ed) (min (ed-length ed) (1+ (ed-point ed))))) ; C-f
               (7  (ed-replace-all ed ""))                                    ; C-g
               ((8 127) (ed-delete ed (1- (ed-point ed)) (ed-point ed)))      ; C-h, DEL
               (9  (complete ed))                                             ; TAB
               ((10 13) (done :line))                                         ; C-j, RET
               (11 (ed-delete ed (ed-point ed) (ed-length ed) :kill t))       ; C-k
               (12 (format *standard-output* "~c[H~c[2J" +esc+ +esc+))        ; C-l
               (14 (history-move ed +1))                                      ; C-n
               (16 (history-move ed -1))                                      ; C-p
               (20 (let ((p (ed-point ed)) (text (ed-text ed)))               ; C-t
                     (when (and (>= p 1) (>= (ed-length ed) 2))
                       (let ((i (if (< p (ed-length ed)) p (1- p))))
                         (rotatef (char text (1- i)) (char text i))
                         (setf (ed-point ed) (min (ed-length ed) (1+ i)))))))
               (21 (ed-delete ed 0 (ed-point ed) :kill t))                    ; C-u
               (23 (ed-delete ed (backward-word-pos                           ; C-w
                                  ed (lambda (c) (not (member c '(#\Space #\Tab)))))
                              (ed-point ed) :kill t))
               (25 (ed-insert ed (ed-kill ed)))                               ; C-y
               (t (when (or (graphic-char-p key) (char= key #\Space))
                    (ed-insert ed (string key))
                    ;; Typing leaves the history and edits the live line.
                    (setf (ed-index ed) nil))))))
          (redisplay ed))))))

;;; TAB.  Defined after the editor because it needs WORD-CHAR-P and the ED-
;;; accessors; called from %EDIT's dispatch above.

(defun word-before-point (ed)
  "The word TAB should complete, and where it starts."
  (let ((start (ed-point ed)) (text (ed-text ed)))
    (loop while (and (plusp start) (word-char-p (char text (1- start))))
          do (decf start))
    (values (subseq text start (ed-point ed)) start)))

(defun complete (ed)
  "With no completer, TAB indents -- that is the only thing left to mean.  With
one: a single candidate is inserted outright, several are reduced to their
common prefix, and TAB again lists them, which is the readline bargain."
  (if (null *completer*)
      (ed-insert ed "  ")
      (multiple-value-bind (word start) (word-before-point ed)
        (let ((candidates (sort (copy-list (funcall *completer* word)) #'string<)))
          (cond
            ((null candidates))         ; nothing matches: leave the line alone
            ((null (rest candidates))
             (ed-delete ed start (ed-point ed))
             (ed-insert ed (concatenate 'string (first candidates) " ")))
            (t
             (let ((prefix (common-prefix candidates)))
               (if (> (length prefix) (length word))
                   (progn (ed-delete ed start (ed-point ed))
                          (ed-insert ed prefix))
                   ;; No more common text to give, so show the choices.  The
                   ;; redisplay that follows every key repaints the prompt.
                   (progn (terpri *standard-output*)
                          (plumb::write-wrapped candidates *standard-output*)))))))))
  (values))
