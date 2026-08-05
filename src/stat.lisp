;;;; stat.lisp -- one lstat, with everything the kernel actually returns.
;;;;
;;;; SB-POSIX's STAT is missing three things the syscall provides: sub-second
;;;; timestamps, st_blocks, and (on Darwin) st_birthtime.  Reaching them means
;;;; declaring the platform's struct stat and calling lstat through sb-alien.
;;;;
;;;; That is only safe because the layout is checked rather than trusted: this
;;;; file reads fields SB-POSIX also knows -- size, mode, ino, uid, nlink,
;;;; mtime -- and the test suite asserts the two agree.  A wrong offset would
;;;; show up there as a mismatch on a field with a known value, instead of as
;;;; plausible-looking nonsense in the fields nothing else can check.
;;;;
;;;; Darwin/arm64 is implemented and tested.  Everywhere else falls back to
;;;; SB-POSIX with the extra fields NIL, so nothing breaks -- it just does not
;;;; gain anything.  Linux would want its own struct, laid out differently.

(in-package #:plumb)

(defstruct (file-stat (:conc-name fs-))
  size mode nlink uid gid ino dev rdev
  ;; Universal time, as CL counts it; the -NSEC fields are the fraction of a
  ;; second on top, 0 to 999999999.
  atime atime-nsec mtime mtime-nsec ctime ctime-nsec
  birthtime                             ; Darwin only; NIL elsewhere
  blocks                                ; 512-byte blocks actually allocated
  blksize)

(defconstant +unix-to-universal+ (encode-universal-time 0 0 0 1 1 1970 0)
  "2208988800.  CL counts from 1900, unix from 1970.")

(defun universal-from-unix (seconds)
  (when seconds (+ seconds +unix-to-universal+)))

#+darwin
(progn
  ;; struct stat as Darwin defines it with 64-bit inodes, which is the only
  ;; shape modern macOS has.  timespec is flattened into its two 64-bit halves
  ;; so the layout is stated here rather than assembled from another type.
  ;; sizeof is 144; the pad after RDEV is SBCL's, from aligning ATIME-SEC.
  (sb-alien:define-alien-type nil
    (sb-alien:struct darwin-stat
      (dev (sb-alien:signed 32))
      (mode (sb-alien:unsigned 16))
      (nlink (sb-alien:unsigned 16))
      (ino (sb-alien:unsigned 64))
      (uid (sb-alien:unsigned 32))
      (gid (sb-alien:unsigned 32))
      (rdev (sb-alien:signed 32))
      (atime-sec (sb-alien:signed 64)) (atime-nsec (sb-alien:signed 64))
      (mtime-sec (sb-alien:signed 64)) (mtime-nsec (sb-alien:signed 64))
      (ctime-sec (sb-alien:signed 64)) (ctime-nsec (sb-alien:signed 64))
      (birthtime-sec (sb-alien:signed 64)) (birthtime-nsec (sb-alien:signed 64))
      (size (sb-alien:signed 64))
      (blocks (sb-alien:signed 64))
      (blksize (sb-alien:signed 32))
      (flags (sb-alien:unsigned 32))
      (gen (sb-alien:unsigned 32))
      (lspare (sb-alien:signed 32))
      (qspare-1 (sb-alien:signed 64))
      (qspare-2 (sb-alien:signed 64))))

  (sb-alien:define-alien-routine ("lstat" %lstat) sb-alien:int
    (path sb-alien:c-string)
    (buffer (* (sb-alien:struct darwin-stat))))

  (defun file-stat (path)
    "One lstat.  NIL if the entry is gone or unreachable."
    (let ((name (namestring path)))
      (sb-alien:with-alien ((buffer (sb-alien:struct darwin-stat)))
        (when (zerop (%lstat name (sb-alien:addr buffer)))
          (macrolet ((f (slot) `(sb-alien:slot buffer ',slot)))
            (make-file-stat
             :size (f size) :mode (f mode) :nlink (f nlink)
             :uid (f uid) :gid (f gid) :ino (f ino) :dev (f dev) :rdev (f rdev)
             :atime (universal-from-unix (f atime-sec)) :atime-nsec (f atime-nsec)
             :mtime (universal-from-unix (f mtime-sec)) :mtime-nsec (f mtime-nsec)
             :ctime (universal-from-unix (f ctime-sec)) :ctime-nsec (f ctime-nsec)
             :birthtime (universal-from-unix (f birthtime-sec))
             :blocks (f blocks) :blksize (f blksize))))))))

#-darwin
(defun file-stat (path)
  "SB-POSIX fallback: everything it knows, and NIL for what it does not.  A
platform wanting sub-second times needs its own struct stat above."
  (let ((s (ignore-errors (sb-posix:lstat path))))
    (when s
      (make-file-stat
       :size (sb-posix:stat-size s) :mode (sb-posix:stat-mode s)
       :nlink (sb-posix:stat-nlink s) :uid (sb-posix:stat-uid s)
       :gid (sb-posix:stat-gid s) :ino (sb-posix:stat-ino s)
       :dev (sb-posix:stat-dev s) :rdev (sb-posix:stat-rdev s)
       :atime (universal-from-unix (sb-posix:stat-atime s)) :atime-nsec nil
       :mtime (universal-from-unix (sb-posix:stat-mtime s)) :mtime-nsec nil
       :ctime (universal-from-unix (sb-posix:stat-ctime s)) :ctime-nsec nil
       :birthtime nil :blocks nil :blksize nil))))

(defun precise-time (seconds nanoseconds)
  "SECONDS and NANOSECONDS as one exact rational, for sorting inside a second.
Exact rather than a float: a double cannot hold a universal time to nanosecond
resolution, so 1e-9 differences would vanish just where they matter."
  (when seconds
    (if nanoseconds (+ seconds (/ nanoseconds 1000000000)) seconds)))
