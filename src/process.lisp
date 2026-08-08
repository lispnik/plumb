;;;; process.lisp -- external commands as pipeline stages.  Unix only: a string
;;;; command is handed to /bin/sh.
;;;;
;;;; `lines` already turns any stream into objects, so a child process could
;;;; always be spliced in by hand.  What was missing is everything around the
;;;; data: nothing killed a child whose reader had gone away, and a command
;;;; that failed was indistinguishable from one that printed nothing.
;;;;
;;;; Both live in WITH-COMMAND's UNWIND-PROTECT, which is the only place a
;;;; child can be reaped -- a downstream TAKE closes our output, the SEND
;;;; inside EMIT-LINES signals CHANNEL-CLOSED, and we unwind through there.
;;;;
;;;; SH-FILTER (objects -> stdin, stdout -> objects) is the one stage in the
;;;; project that runs a thread of its own, and it is a DELIBERATE exception to
;;;; "a stage contains no concurrency" rather than an oversight.  The reason is
;;;; a deadlock, not a preference: a filter must write the child's stdin and
;;;; read its stdout at the same time -- write it all first and the kernel's
;;;; pipe buffer fills with nobody draining stdout, and both sides stop for
;;;; good.  Nor can one thread wait on both, because the object side blocks in
;;;; RECV, a condition variable, and the byte side is a file descriptor; there
;;;; is no primitive that selects across the two.
;;;;
;;;; What keeps the exception narrow, and what any change here must preserve:
;;;; the feeder thread touches exactly ONE channel, never closes it, and owns
;;;; no port.  CLOSE-INPUT stays with SPAWN-STAGE on the stage thread, so the
;;;; refcounts still count precisely the threads RUN knows about -- which is
;;;; the property the no-concurrency rule exists to protect.  See CLAUDE.md.

(in-package #:plumb)

(defconstant +sigterm+ 15
  "Core PLUMB does not depend on sb-posix, so the number is spelled out.")

(define-condition command-failed (error)
  ((command   :initarg :command   :reader command-failed-command)
   (exit-code :initarg :exit-code :reader command-failed-exit-code)
   (stderr    :initarg :stderr    :initform nil :reader command-failed-stderr))
  (:report
   (lambda (c s)
     (format s "command ~s exited ~a~@[: ~a~]"
             (command-failed-command c)
             (command-failed-exit-code c)
             (let ((text (command-failed-stderr c)))
               (when (and text (plusp (length text)))
                 (string-trim '(#\Space #\Newline) text)))))))

(defun %command-argv (command)
  "A string runs under /bin/sh -c; a list is exec'd directly, with no shell to
quote against."
  (etypecase command
    (string (values "/bin/sh" (list "-c" command)))
    (cons   (values (first command) (mapcar #'princ-to-string (rest command))))))

(defun %command-label (command)
  (if (stringp command)
      command
      (format nil "~{~a~^ ~}" command)))

(defun %stderr-file ()
  (merge-pathnames (format nil "plumb-stderr-~36r.txt" (random (expt 2 48)))
                   #p"/tmp/"))

(defun %slurp (path)
  (when (and path (probe-file path))
    (with-open-file (in path :if-does-not-exist nil)
      (when in
        (let ((text (make-string (file-length in))))
          (subseq text 0 (read-sequence text in)))))))

(defun finish-command (proc command stderr-file on-exit)
  "Normal path: let the child finish, then decide what its exit code means."
  ;; Closing stdin first, or `wc -l` waits for an EOF that never comes.
  (let ((in (sb-ext:process-input proc)))
    (when in (ignore-errors (close in))))
  (sb-ext:process-wait proc)
  (let ((code (sb-ext:process-exit-code proc))
        (text (%slurp stderr-file)))
    ;; Captured stderr is still the user's to see, whether or not we signal.
    (when (and text (plusp (length text)))
      (write-string text *error-output*)
      (force-output *error-output*))
    (when (and (eq on-exit :signal) (integerp code) (/= code 0))
      (error 'command-failed :command (%command-label command)
                             :exit-code code :stderr text))
    code))

(defun reap-command (proc stderr-file)
  "Cleanup path, run on every exit.  A child still alive here is one whose
reader went away, so it is killed rather than left writing into a dead pipe.
On the normal path FINISH-COMMAND already waited and this just tidies up."
  (when (ignore-errors (sb-ext:process-alive-p proc))
    (ignore-errors (sb-ext:process-kill proc +sigterm+))
    (ignore-errors (sb-ext:process-wait proc)))
  (ignore-errors (sb-ext:process-close proc))
  (when stderr-file (ignore-errors (delete-file stderr-file))))

(defmacro with-command ((var command run-options &key directory stderr on-exit)
                        &body body)
  "Spawn COMMAND, run BODY with VAR bound to the process, then reap it.
RUN-OPTIONS is a list of extra SB-EXT:RUN-PROGRAM arguments -- the two stages
differ only in whether they want a stdout stream or a stdin one."
  (let ((cmd (gensym "CMD")) (file (gensym "FILE"))
        (prog (gensym "PROG")) (args (gensym "ARGS")))
    `(let* ((,cmd ,command)
            (,file (unless (eq ,stderr :inherit) (%stderr-file))))
       (multiple-value-bind (,prog ,args) (%command-argv ,cmd)
         (let ((,var (apply #'sb-ext:run-program ,prog ,args
                            :wait nil :search t :directory ,directory
                            (append ,run-options
                                    (when ,file
                                      (list :error ,file
                                            :if-error-exists :supersede))))))
           (unwind-protect
                (progn ,@body
                       (finish-command ,var ,cmd ,file ,on-exit))
             (reap-command ,var ,file)))))))

;;; ------------------------------------------------------------------- stages

(defstage sh ((command (or string cons)) &key directory (on-exit :signal) (stderr :capture))
  "Run COMMAND and emit its stdout as LINE objects.

A string runs under /bin/sh, so pipes and globs work; a list is exec'd
directly, which is what you want when an argument came from somewhere else.
A non-zero exit signals COMMAND-FAILED unless ON-EXIT is :IGNORE.  STDERR is
:CAPTURE (replayed on our stderr, and attached to the condition) or :INHERIT
(live on the terminal, but then there is nothing to attach)."
  (:consumes nil) (:produces :objects)
  (with-command (proc command '(:output :stream)
                  :directory directory :stderr stderr :on-exit on-exit)
    (emit-lines (sb-ext:process-output proc) (%command-label command))))

(defstage to-sh ((command (or string cons)) &key directory (on-exit :signal) (stderr :capture))
  "Write each object to COMMAND's stdin, one line each.  A sink; the command's
own stdout is inherited, so `(to-sh \"wc -l\")` prints where you would expect."
  (:consumes t) (:produces nil)
  (with-command (proc command '(:input :stream :output t)
                  :directory directory :stderr stderr :on-exit on-exit)
    ;; A stage thread does not inherit the caller's *PRINT-PRETTY*, and one
    ;; object per line is the contract every downstream tool assumes.
    (let ((*print-pretty* nil)
          (in (sb-ext:process-input proc)))
      (do-input (x)
        (write-line (if (stringp x) x (princ-to-string x)) in)))))

;;; ---------------------------------------------------- the mid-pipeline filter

(defun %feed-command (stream input as label)
  "On a thread of its own, write every object arriving on INPUT to STREAM, one
line each, and close STREAM when the objects run out.  Returns the TASK.

Closing is the point of the exercise as much as the writing: `wc -l' produces
nothing until stdin reaches EOF, and the stage thread is meanwhile blocked
reading the stdout it will never write.  So the close happens on both exit
paths, including the one where a write failed.

A write to a child that has already gone signals rather than raising SIGPIPE,
so an error here is expected traffic on the TAKE path and not a fault.  It is
also what bounds this thread when the child exits early -- SBCL buffers, so the
failure arrives within a buffer's worth of lines rather than after the whole
input has been drained."
  (spawn (format nil "plumb:feed>~a" label)
         (lambda ()
           (let ((*print-pretty* nil))
             (unwind-protect
                  (ignore-errors
                   (loop
                     (multiple-value-bind (obj ok) (recv input)
                       (unless ok (return))
                       (write-line (funcall as obj) stream)))
                   (finish-output stream))
               (ignore-errors (close stream)))))))

(defstage sh-filter ((command (or string cons)) &key directory as
                                                     (on-exit :signal) (stderr :capture))
  "Filter the stream through COMMAND: each object to its stdin as a line, its
stdout back as LINE objects.

  ls \"src/*.lisp\" | sh-filter \"sort -r\" | take 3 | print-items
  ps | sh-filter (list \"grep\" \"lisp\") | tally

This is the shape SH and TO-SH leave out, and the one that lets an external
command sit in the middle of a pipeline instead of only at an end.

Objects become text through PRESENT, which is the project's one-object-one-line
rule, so a FILE-ENTRY arrives at the command as the line you would have seen
from PRINT-ITEMS.  :AS overrides that with any function of one object.  Note
that TO-SH uses PRINC-TO-STRING instead -- an inconsistency worth knowing about
until one of them moves.

Coming back the other way the objects are LINEs, exactly as from SH, so .text
and .number work and nothing knows the difference.

RUNS A THREAD, alone among stages.  See the header of this file for why that
is forced rather than chosen.  It follows that :WORKERS is not offered: two
copies would interleave their lines into one stdin, and the command would see
neither stream.

A non-zero exit signals COMMAND-FAILED unless ON-EXIT is :IGNORE, and STDERR is
:CAPTURE or :INHERIT, both as for SH."
  (:consumes t) (:produces :objects)
  (let ((label (%command-label command))
        (input *input*)
        (as (if as (ensure-fn as) #'present)))
    (unless input
      (error "Stage ~a tried to read input but is wired as a source." *stage-name*))
    (with-command (proc command '(:input :stream :output :stream)
                        :directory directory :stderr stderr :on-exit on-exit)
      (let ((feeder (%feed-command (sb-ext:process-input proc) input as label))
            (drained nil))
        (unwind-protect
             (progn (emit-lines (sb-ext:process-output proc) label)
                    (setf drained t))
          ;; Teardown, and the order matters on every line of it.
          (unless drained
            ;; We unwound, so downstream closed on us and the child's output is
            ;; going nowhere.  Break it NOW: the feeder may be parked on a full
            ;; pipe, and no channel operation can wake that -- only the read
            ;; end closing will.  On the drained path the child is already
            ;; finishing and killing it would lose the exit status.
            (ignore-errors (sb-ext:process-kill proc +sigterm+)))
          ;; SIGPIPE upstream, so a source still producing stops.  This is also
          ;; what releases a feeder parked in RECV: RECV wakes on
          ;; PRODUCER-CLOSED, which arrives only once the upstream stage tries
          ;; to SEND, sees the closed channel and unwinds.  SPAWN-STAGE's own
          ;; CLOSE-INPUT still runs afterwards and is idempotent with this.
          (abort-input input)
          ;; Everything above makes the feeder terminate; the timeout is for
          ;; the child that ignores SIGTERM, which would otherwise hang the
          ;; whole pipeline here rather than just itself.
          (await feeder :timeout 5))))))

;;; ------------------------------------------------------------------- ps
;;;
;;; The data comes from ps(1); what plumb adds is that it arrives as objects,
;;; so the columns keep their types and there is nothing to re-parse.  A native
;;; implementation would mean /proc on Linux and sysctl plus libproc on macOS --
;;; two lots of platform FFI to obtain what ps already prints correctly.
;;;
;;; BSD-style options, which GNU ps also accepts.  Each field is written with a
;;; trailing = to suppress its header, so there is no header line to skip, and
;;; ARGS comes last because it is the only one that can contain a space.

(defstruct process
  pid ppid user state pcpu pmem rss vsz etime tty name command args)

(defmethod present ((p process))
  (format nil "~6@a  ~a" (process-pid p) (process-name p)))

(defparameter +ps-fields+
  "pid=,ppid=,user=,state=,pcpu=,pmem=,rss=,vsz=,etime=,tty=,args=")

(defconstant +ps-fixed-fields+ 10
  "How many space-free columns precede ARGS.")

(defun kilobytes-to-bytes (text)
  (let ((kb (parse-integer text :junk-allowed t)))
    (when kb (* kb 1024))))

(defun parse-real (text)
  (let ((value (ignore-errors (let ((*read-eval* nil)) (read-from-string text)))))
    (when (realp value) value)))

(defun parse-ps-line (line)
  "One ps line as a PROCESS, or NIL if it is too short to be one."
  (let ((fields '()) (i 0) (n (length line)))
    (flet ((skip-spaces ()
             (loop while (and (< i n) (char= (char line i) #\Space)) do (incf i))))
      (dotimes (k +ps-fixed-fields+)
        (declare (ignorable k))
        (skip-spaces)
        (let ((start i))
          (loop while (and (< i n) (char/= (char line i) #\Space)) do (incf i))
          (push (subseq line start i) fields)))
      (skip-spaces)
      (let ((f (nreverse fields)))
        (when (and (= (length f) +ps-fixed-fields+) (parse-integer (first f) :junk-allowed t))
          (let* ((args (subseq line (min i n)))
                 (command (subseq args 0 (or (position #\Space args) (length args)))))
            (make-process :pid   (parse-integer (nth 0 f) :junk-allowed t)
                          :ppid  (parse-integer (nth 1 f) :junk-allowed t)
                          :user  (nth 2 f)
                          :state (nth 3 f)
                          :pcpu  (parse-real (nth 4 f))
                          :pmem  (parse-real (nth 5 f))
                          ;; ps reports these in kilobytes.  Bytes here, so
                          ;; that .rss and LS's .size are the same unit and one
                          ;; 500mb literal means the same thing against both.
                          :rss   (kilobytes-to-bytes (nth 6 f))
                          :vsz   (kilobytes-to-bytes (nth 7 f))
                          :etime (nth 8 f)
                          :tty   (nth 9 f)
                          :name  (basename command)
                          :command command
                          :args  args)))))))

(defstage ps ()
  "Emit a PROCESS per running process -- all of them, as `ps ax` does.

There are no selection options on purpose.  Narrowing is WHERE and ordering is
SORT-BY, which is the whole argument for objects over text: ps(1) needs -u, -e,
--sort and -o because its output is a formatted string, and once the columns
keep their types none of that has to exist.

  ps | where {(> .rss 500mb)} | sort-by .rss :desc | take 5 | table
  ps | tally :key .name
  ps | where {(string= .user \"root\")} | tally

RSS and VSZ are in BYTES, not the kilobytes ps prints.  Deliberately: LS
reports .size in bytes, and a unit that changed meaning depending on which
source produced the object would undo the reason for having objects."
  (:consumes nil) (:produces :objects)
  (with-command (proc (list "ps" "axo" +ps-fields+) '(:output :stream)
                 :stderr :capture :on-exit :signal)
    (let ((out (sb-ext:process-output proc)))
      (loop for line = (read-line out nil nil)
            while line
            do (let ((process (parse-ps-line line)))
                 (when process (emit process)))))))

;;; ---------------------------------------------------------------- handles
;;;
;;; lsof -F is a field format built for parsing -- that is what -F is for -- and
;;; it exists on both platforms with the same flags and the same shape.  The
;;; output is a STREAM of tagged lines rather than a table: `p` opens a process
;;; record, `f` opens a file record inside it, and everything else sets a field
;;; on whichever is currently open.  So this is a small state machine, and the
;;; last record has to be flushed at EOF.
;;;
;;; -n and -P turn off DNS and port-name lookup, which keeps it fast and stops
;;; the output depending on the network; -w drops warnings about processes we
;;; cannot examine.

(defstruct handle
  pid command uid user
  fd                                    ; "cwd", "txt", "3" -- a string, not a
                                        ; number, because most of them are not
  type protocol state
  size inode name)

(defmethod present ((h handle))
  (format nil "~6@a  ~12a ~a" (handle-pid h) (handle-command h)
          (or (handle-name h) (handle-type h) (handle-fd h))))

(defun map-lsof-handles (stream function users)
  (let ((pid nil) (command nil) (uid nil) (file nil))
    (flet ((flush ()
             (when file (funcall function file) (setf file nil))))
      (loop for line = (read-line stream nil nil)
            while line
            do (when (plusp (length line))
                 (let ((tag (char line 0))
                       (value (subseq line 1)))
                   (case tag
                     ;; A new process ends whatever file record was open.
                     (#\p (flush)
                          (setf pid (parse-integer value :junk-allowed t)
                                command nil uid nil))
                     (#\c (setf command value))
                     (#\u (setf uid (parse-integer value :junk-allowed t)))
                     (#\f (flush)
                          (setf file (make-handle
                                      :pid pid :command command :uid uid
                                      :user (when uid
                                              (name-for-id uid users
                                                           #'sb-posix:getpwuid
                                                           #'sb-posix:passwd-name))
                                      :fd value)))
                     (#\t (when file
                            (setf (handle-type file)
                                  (intern (string-upcase value) :keyword))))
                     (#\s (when file
                            (setf (handle-size file) (parse-integer value :junk-allowed t))))
                     (#\i (when file
                            (setf (handle-inode file) (parse-integer value :junk-allowed t))))
                     (#\n (when file (setf (handle-name file) value)))
                     (#\P (when file
                            (setf (handle-protocol file)
                                  (intern (string-upcase value) :keyword))))
                     ;; T carries several key=value pairs; ST is the TCP state.
                     (#\T (when (and file (eql 0 (search "ST=" value)))
                            (setf (handle-state file)
                                  (intern (subseq value 3) :keyword))))))))
      (flush))))

(defstage handles (&key pid network)
  "Emit a HANDLE per open file -- every descriptor every process holds, which
on unix means sockets and devices too, not just files.

  handles | where {(eq .type :ipv4)} | table :columns (list :command :name :state)
  handles | where {(eq .state :listen)} | sort-by .command | table
  handles | tally :key .command | sort-by .count :desc | take 10
  handles :pid (sb-posix:getpid) | table

.FD is a STRING, because most of them are not numbers: cwd, txt, rtd and mem
are descriptors in lsof's sense.  .TYPE and .PROTOCOL are lsof's own vocabulary
upcased into keywords -- :REG :DIR :IPV4 :IPV6 :UNIX :FIFO :CHR -- rather than
a set invented here.

:PID and :NETWORK exist for the reason COMMITS takes a :PATH -- lsof does that
selection far more cheaply than a filter can, an order of magnitude for
:NETWORK -- and everything else is WHERE.

lsof exits non-zero when there are processes it may not examine, which for an
unprivileged user is most of them, so the status is ignored: a partial answer is
the normal answer here."
  (:consumes nil) (:produces :objects)
  (let ((users (make-hash-table)))
    (with-command (proc (append (list "lsof" "-F" "pcuftsinPT" "-n" "-P" "-w")
                                (when network (list "-i"))
                                (when pid (list "-p" (princ-to-string pid))))
                   '(:output :stream) :stderr :capture :on-exit :ignore)
      (map-lsof-handles (sb-ext:process-output proc)
                        (lambda (handle) (emit handle))
                        users))))
