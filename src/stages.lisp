;;;; stages.lisp -- a starter set.  Note how little any of them know about
;;;; threads: the concurrency is entirely in RECV, SEND and SPAWN-STAGE.

(in-package #:plumb)

;;; ---------------------------------------------------------------- sources

(defstage from-list ((items sequence))
  "Emit each element of ITEMS."
  (:consumes nil) (:produces :objects)
  (map nil (lambda (x) (emit x)) items))

(defstage counter (&key (from 0) (by 1) limit)
  "Emit integers forever (or until LIMIT).  Useful for proving that a
downstream TAKE really does tear the source down."
  (:consumes nil) (:produces :objects)
  (loop for i = from then (+ i by)
        while (or (null limit) (< i limit))
        do (emit i)))

(defstruct file-entry path name size mtime dir-p)

(defmethod present ((object file-entry))
  (if (file-entry-dir-p object)
      (concatenate 'string (file-entry-name object) "/")
      (file-entry-name object)))

(defun as-directory (pathname)
  "PATHNAME as a directory, whether or not it was written with a trailing /."
  (if (pathname-name pathname)
      (make-pathname :directory (append (or (pathname-directory pathname) '(:relative))
                                        (list (file-namestring pathname)))
                     :name nil :type nil :defaults pathname)
      pathname))

(defun shell-glob-pathname (pathname)
  "Shell glob semantics on a CL pathname.  Shell * means anything; CL * means
`any name, no type', so *.lisp works untranslated but * and f* do not -- a
wild name with no type has to match any type as well."
  (if (and (wild-pathname-p pathname :name) (null (pathname-type pathname)))
      (make-pathname :type :wild :defaults pathname)
      pathname))

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
  (flet ((sorted (paths) (sort paths #'string< :key #'namestring)))
    (let ((pathname (pathname spec)))
      (cond
        ((wild-pathname-p pathname)
         (sorted (directory (shell-glob-pathname pathname) :resolve-symlinks nil)))
        ;; A plain name that is a file is just itself.
        ((let ((truename (and (pathname-name pathname) (probe-file pathname))))
           (and truename (pathname-name truename) (list truename))))
        ;; ...and one that is a directory lists what is in it.  Without this,
        ;; (ls "src") with no trailing slash merges to *.* carrying no
        ;; directory component, and silently lists the current directory.
        (t (sorted (directory (merge-pathnames "*.*" (as-directory pathname))
                              :resolve-symlinks nil)))))))

(defstage ls (&optional (pattern *default-pathname-defaults*))
  "Emit a FILE-ENTRY per match.  A directory lists its members; a pattern
containing * ? or [...] globs, and ** descends into subdirectories."
  (:consumes nil) (:produces :objects)
  (dolist (p (glob pattern))
    (let ((dir-p (null (pathname-name p))))
      (emit (make-file-entry
             :path p
             :name (if dir-p
                       (car (last (pathname-directory p)))
                       (file-namestring p))
             :dir-p dir-p
             :size (unless dir-p
                     (ignore-errors
                      (with-open-file (s p :element-type '(unsigned-byte 8))
                        (file-length s))))
             :mtime (ignore-errors (file-write-date p)))))))

(defstruct line text number source)

(defmethod present ((object line)) (line-text object))

(defun emit-lines (stream source)
  "Read STREAM to EOF, emitting one LINE per line.  Shared by LINES and SH.
EMIT is a macro over (SEND (PORT :OUT) ...) and PORT reads the *OUTPUTS*
special, so an ordinary function called from a stage thread can emit."
  (loop for n from 1
        for text = (read-line stream nil nil)
        while text
        do (send (port :out) (make-line :text text :number n :source source))))

(defstage lines ((stream stream))
  "Byte/character boundary: turn a CL stream into LINE objects.  This is where
an external process's stdout enters the object world."
  (:consumes nil) (:produces :objects)
  (emit-lines stream stream))

;;; ------------------------------------------------------------- transforms

(defstage where ((pred (or function symbol)))
  "Pass through only the objects satisfying PRED."
  (:consumes :objects) (:produces :objects)
  (let ((pred (ensure-fn pred)))
    (do-input (x)
      (when (funcall pred x)
        (emit x)))))

(defstage xform ((fn (or function symbol)))
  "Apply FN to each object."
  (:consumes :objects) (:produces :objects)
  (let ((fn (ensure-fn fn)))
    (do-input (x)
      (emit (funcall fn x)))))

(defstage take ((n (integer 0)))
  "Pass the first N objects, then stop the whole upstream."
  (:consumes :objects) (:produces :objects)
  (when (zerop n) (finish))
  (do-input (x)
    (emit x)
    (when (zerop (decf n)) (finish))))

(defstage drop ((n (integer 0)))
  "Discard the first N objects, pass the rest."
  (:consumes :objects) (:produces :objects)
  (do-input (x)
    (if (plusp n) (decf n) (emit x))))

(defstage uniq (&key (test #'eql) key)
  "Pass an object only the first time its KEY is seen.  TEST defaults to EQL,
so string keys want :TEST #'EQUAL."
  (:consumes :objects) (:produces :objects)
  (let ((seen '()) (key (if key (ensure-fn key) #'identity)))
    (do-input (x)
      (let ((k (funcall key x)))
        (unless (member k seen :test test)
          (push k seen)
          (emit x))))))

(defstage peek (&key (stream *standard-output*) (prefix "-> "))
  "Tap: print each object as it goes by, pass it on unchanged."
  (:consumes :objects) (:produces :objects)
  (do-input (x)
    (with-output-lock (format stream "~a~a~%" prefix (present x)))
    (emit x)))

;;; A collecting stage: emits nothing until its input hits EOF.  Sorting is
;;; inherently a barrier, and the type system does not need to know that --
;;; backpressure handles it.
(defstage sort-by ((key (or function symbol)) &key desc (predicate nil))
  "Sort the whole stream by KEY.  A barrier: nothing is emitted until the input
hits EOF, which the type signature does not say and does not need to."
  (:consumes :objects) (:produces :objects) (:barrier t)
  (let ((key (ensure-fn key))
        (buf (make-array 16 :adjustable t :fill-pointer 0))
        (pred (or predicate #'default-lessp)))
    (do-input (x) (vector-push-extend x buf))
    (let ((sorted (sort buf (if desc (complement pred) pred) :key key)))
      (map nil (lambda (x) (emit x)) sorted))))

(defun default-lessp (a b)
  (cond ((and (realp a) (realp b)) (< a b))
        ((and (stringp a) (stringp b)) (string< a b))
        ((and (symbolp a) (symbolp b)) (string< (string a) (string b)))
        ((null a) (not (null b)))
        ((null b) nil)
        (t (string< (princ-to-string a) (princ-to-string b)))))

(defstage tally (&key key)
  "Count objects (per KEY, if given) and emit the totals at EOF."
  (:consumes :objects) (:produces :objects) (:barrier t)
  (if key
      (let ((counts (make-hash-table :test #'equal))
            (key (ensure-fn key)))
        (do-input (x) (incf (gethash (funcall key x) counts 0)))
        (maphash (lambda (k v) (emit (list :key k :count v))) counts))
      (let ((n 0))
        (do-input (x) (incf n))
        (emit n))))

(defstage accumulate ((fn (or function symbol)) initial)
  "Fold the stream, emitting one result at EOF."
  (:consumes :objects) (:produces :objects) (:barrier t)
  (let ((fn (ensure-fn fn)) (acc initial))
    (do-input (x) (setf acc (funcall fn acc x)))
    (emit acc)))

;;; ------------------------------------------------------------------ sinks

(defstage to-text (&key (formatter nil))
  "Objects back out to text.  The other half of the byte boundary."
  (:consumes :objects) (:produces :bytes)
  (let ((formatter (if formatter (ensure-fn formatter)
                       (lambda (x) (princ-to-string x)))))
    (do-input (x)
      (emit (funcall formatter x)))))

(defstage print-items (&key (stream *standard-output*))
  "Print each object to STREAM, one line each.  A sink: produces nothing."
  (:consumes t) (:produces nil)
  (do-input (x)
    (with-output-lock
      (write-line (present x) stream)
      (force-output stream))))

(defstage tee (&rest branches)
  "Send every object down each of BRANCHES as well as onward, so one stream
feeds several pipelines.  Each branch is an ordinary list of stages.

  (tee (list (where ($ (fld :dir-p))) (to-file \"dirs.txt\"))
       (list (tally)))

Objects are SHARED with the branches, not copied.  That is deliberate: nothing
can deep-copy an arbitrary Lisp object correctly, and every stage here already
produces new values rather than mutating.  Note what sharing means -- this
stage sends to the branches and emits onward concurrently, so a branch that
mutates is a data *race*, not merely a visible change.  Where a branch must
mutate, copying is itself a stage: put (xform #'copy-file-entry) at its head.
That keeps the policy explicit and composable instead of a flag on TEE.

A branch that stops early, say on a TAKE, is dropped and the rest carry on;
that independence is the entire point of a fan-out."
  (:consumes :objects) (:produces :objects)
  ;; This spawns pipelines, which is as close as anything here comes to a stage
  ;; containing concurrency.  It stays within the rule in the way that matters:
  ;; all coordination is still SEND, RECV and one UNWIND-PROTECT, and the
  ;; threads belong to RUN rather than to this body.
  (let* ((heads (mapcar (lambda (branch)
                          (make-channel :name (format nil "tee->~a"
                                                      (stage-name (first branch)))))
                        branches))
         (pipes (mapcar (lambda (branch head) (run branch :input head)) branches heads))
         (live (copy-list heads)))
    (unwind-protect
         (do-input (x)
           (dolist (head heads)
             (when (member head live)
               (handler-case (send head x)
                 (channel-closed () (setf live (remove head live))))))
           (emit x))
      ;; Every branch gets its EOF whichever way this stage ended.
      (dolist (head heads) (ignore-errors (close-output head)))
      (dolist (pipe pipes) (ignore-errors (join pipe))))))

(defstage route ((pred (or function symbol)))
  "Send objects satisfying PRED out the :YES port and the rest out :NO, wiring
each to its own branch:

  (run (list (ls) (route ($ (fld :dir-p))))
       :ports (list :yes (list (to-file \"dirs.txt\"))
                    :no  (list (to-file \"files.txt\"))))

Nothing goes out :OUT, so this ends the main line -- the branches are where the
objects went.  A port with no branch is discarded, which EXPLAIN reports.

TRY-EMIT rather than EMIT: one branch finishing must leave the others running,
which is the difference between a fan-out and a pipeline."
  (:consumes :objects) (:produces nil) (:ports :yes :no)
  (let ((pred (ensure-fn pred)))
    (do-input (x)
      (try-emit x (if (funcall pred x) :yes :no)))))

(defstage to-file ((path (or string pathname)) &key (if-exists :supersede))
  "Write each object to PATH, one line each.  A sink; what > and >> expand to.
WITH-OPEN-FILE is the teardown story -- an upstream error or a downstream close
unwinds through it and the file is closed either way."
  (:consumes t) (:produces nil)
  (with-open-file (out path :direction :output
                            :if-exists if-exists :if-does-not-exist :create)
    (do-input (x)
      (write-line (present x) out))))

(defstage from-file ((path (or string pathname)))
  "Emit a LINE per line of PATH.  A source; what < expands to.  Opening the
stream here rather than taking one means it is closed on every exit path."
  (:consumes nil) (:produces :objects)
  (with-open-file (in path)
    (emit-lines in path)))

(defstage table (&key columns (stream *standard-output*) (max-width 40))
  "Buffer the whole stream, then print it as an aligned table.  A barrier, like
SORT-BY and for the same reason: a column cannot be sized until the last row
has arrived.  COLUMNS defaults to the union of FIELDS across the rows."
  (:consumes :objects) (:produces nil) (:barrier t)
  (let ((rows (make-array 16 :adjustable t :fill-pointer 0)))
    (do-input (x) (vector-push-extend x rows))
    (render-table (coerce rows 'list)
                  :columns columns :stream stream :max-width max-width)))
