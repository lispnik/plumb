;;;; blockdev.lisp -- disks, partitions and volumes as objects.
;;;;
;;;; The two platforms are reliable for opposite reasons, which is worth
;;;; knowing before trusting anything here.
;;;;
;;;; LINUX is the easy half.  /sys/block is a kernel-stable interface: plain
;;;; file reads, no root, no subprocess.  It is what lsblk itself reads.
;;;;
;;;; DARWIN is the fragile half.  There is no sysfs; /dev/disk* is
;;;; root:operator, so the ioctl route (DKIOCGETBLOCKCOUNT) needs privileges we
;;;; do not have, and IOKit means a large alien surface over CoreFoundation.
;;;; What is left is parsing `diskutil info -all` -- a *tool*, not an
;;;; interface, whose output has changed across releases.  It is the best
;;;; unprivileged source available and it should be treated as the part most
;;;; likely to rot.
;;;;
;;;; Either way this follows what PS already settled: let the thing that
;;;; already gets it right produce the data, and let plumb's contribution be
;;;; that it arrives as objects with typed fields.  That is why there are no
;;;; selection options -- narrowing is WHERE, ordering is SORT-BY.

(in-package #:plumb)

(defstruct block-device
  name                                  ; "mmcblk0p2" / "disk3s1"
  node                                  ; "/dev/mmcblk0p2"
  type                                  ; :disk :partition :volume :loop :ram
  parent                                ; the whole device this belongs to
  size                                  ; BYTES -- see the unit note below
  block-size
  read-only
  removable
  model
  mount-point
  fs-type
  ;; Filesystem facts, so only ever set when MOUNT-POINT is.  Bytes, like SIZE.
  used
  available
  ;; Linux only; NIL on Darwin.  Same shape FILE-STAT uses for the fields
  ;; sb-posix cannot reach -- present where the platform has them, NIL where it
  ;; does not, rather than a different struct per platform.
  major minor rotational start
  ;; Darwin only; NIL on Linux.
  content protocol internal
  ;; Not backed by physical hardware: a macOS disk image, or a Linux loop
  ;; device with a backing file.  Both platforms have a definite source for
  ;; this, which is why it is here and `ejectable` is not -- there is no
  ;; Ejectable key anywhere in `diskutil info`, so that field could only ever
  ;; have been NIL while the docs claimed otherwise.
  virtual)

(defmethod present ((d block-device))
  (format nil "~12a ~@[~a~]" (block-device-name d) (block-device-node d)))

;;; ------------------------------------------------------------------ shared

(defun read-first-line (path)
  "The first line of PATH, trimmed, or NIL if it cannot be read.

NIL rather than an error on purpose: sysfs is full of attributes that exist for
one driver and not the next -- MMC has device/name where SCSI has
device/model -- and a device can be unplugged between listing it and reading
it, which is the same race LS answers by dropping the entry."
  (ignore-errors
   (with-open-file (in path :if-does-not-exist nil)
     (when in
       (let ((line (read-line in nil nil)))
         (when line (string-trim '(#\Space #\Tab #\Return) line)))))))

(defun read-integer-file (path)
  (let ((text (read-first-line path)))
    (when text (parse-integer text :junk-allowed t))))

(defun parse-boolean-file (path)
  "sysfs writes flags as 0 or 1."
  (let ((n (read-integer-file path)))
    (when n (not (zerop n)))))

;;; ------------------------------------------------------------------- Linux

(defconstant +sysfs-sector+ 512
  "sysfs reports `size` in 512-byte sectors ALWAYS -- not in the device's
logical_block_size.  Multiplying by queue/logical_block_size is the classic
wrong answer here, and it happens to be right on any 512-byte device, so it
survives casual testing.  See Documentation/ABI/testing/sysfs-block.")

(defun linux-mount-table ()
  "/proc/mounts as an alist of device node -> (mount-point . fs-type)."
  (let ((table '()))
    (ignore-errors
     (with-open-file (in "/proc/mounts" :if-does-not-exist nil)
       (when in
         (loop for line = (read-line in nil nil)
               while line
               do (let* ((fields (split-on-spaces line)))
                    (when (and (>= (length fields) 3)
                               (eql 0 (search "/dev/" (first fields))))
                      (push (cons (first fields)
                                  (cons (unescape-mount-field (second fields))
                                        (third fields)))
                            table)))))))
    (nreverse table)))

(defun split-on-spaces (line)
  (let ((fields '()) (i 0) (n (length line)))
    (loop while (< i n)
          do (loop while (and (< i n) (char= (char line i) #\Space)) do (incf i))
             (let ((start i))
               (loop while (and (< i n) (char/= (char line i) #\Space)) do (incf i))
               (when (> i start) (push (subseq line start i) fields))))
    (nreverse fields)))

(defun unescape-mount-field (field)
  "/proc/mounts octal-escapes space, tab, newline and backslash, so a mount
point under `/mnt/my disk` arrives as `/mnt/my\\040disk`."
  (let ((out (make-string-output-stream)) (i 0) (n (length field)))
    (loop while (< i n)
          do (let ((c (char field i)))
               (if (and (char= c #\\) (<= (+ i 3) (1- n))
                        (every #'digit-char-p (subseq field (1+ i) (+ i 4))))
                   (progn (write-char (code-char (parse-integer field :start (1+ i)
                                                                     :end (+ i 4)
                                                                     :radix 8))
                                      out)
                          (incf i 4))
                   (progn (write-char c out) (incf i)))))
    (get-output-stream-string out)))

(defun parse-df-usage (stream)
  "`df -Pk` as an alist of device node -> (used . available), in BYTES.

-P is POSIX and -k pins the block size to 1024 on both platforms, which is the
point: without -k, macOS reports 512-byte blocks and GNU df reports 1024, so
the same command would mean different numbers on the two systems."
  (let ((table '()))
    (read-line stream nil nil)                    ; the header
    (loop for line = (read-line stream nil nil)
          while line
          do (let ((f (split-on-spaces line)))
               (when (and (>= (length f) 4) (eql 0 (search "/dev/" (first f))))
                 (let ((used (parse-integer (third f) :junk-allowed t))
                       (available (parse-integer (fourth f) :junk-allowed t)))
                   (push (cons (first f)
                               (cons (when used (* used 1024))
                                     (when available (* available 1024))))
                         table)))))
    (nreverse table)))

(defun linux-usage-table ()
  "Usage per mounted device.  One `df` call, because sysfs does not carry it and
statvfs is not in sb-posix -- and a hand-written statvfs layout would be a
per-architecture struct to get wrong, for two numbers df already computes.
Failure degrades to NIL rather than taking the stage down: usage is the least
important thing here."
  ;; The table is set inside the body rather than returned from it: WITH-COMMAND
  ;; yields FINISH-COMMAND's value, not the body's, so reading its result gives
  ;; the exit status.  SH and PS never noticed because they use it purely for
  ;; effect -- this was an empty device list on Linux and nothing at all on macOS.
  (let ((table nil))
    (ignore-errors
     (with-command (proc (list "df" "-Pk") '(:output :stream) :on-exit :ignore)
       (setf table (parse-df-usage (sb-ext:process-output proc)))))
    table))

(defun linux-device-model (directory)
  "The model string, wherever this driver keeps it.  SCSI and NVMe use
device/model; MMC uses device/name.  Neither is guaranteed."
  (or (read-first-line (merge-pathnames "device/model" directory))
      (read-first-line (merge-pathnames "device/name" directory))))

(defun linux-major-minor (directory)
  "The `dev` attribute, \"179:0\", as two values."
  (let ((text (read-first-line (merge-pathnames "dev" directory))))
    (when text
      (let ((colon (position #\: text)))
        (when colon
          (values (parse-integer text :end colon :junk-allowed t)
                  (parse-integer text :start (1+ colon) :junk-allowed t)))))))

(defun linux-device-type (name whole-p)
  (cond ((not whole-p) :partition)
        ((eql 0 (search "loop" name)) :loop)
        ((eql 0 (search "ram" name)) :ram)
        (t :disk)))

(defun linux-block-device (name directory &key parent mounts usage)
  "One /sys/block entry (or partition subdirectory) as a BLOCK-DEVICE."
  (let* ((node (concatenate 'string "/dev/" name))
         (whole-p (null parent))
         (sectors (read-integer-file (merge-pathnames "size" directory)))
         (mount (cdr (assoc node mounts :test #'string=)))
         (use (cdr (assoc node usage :test #'string=))))
    (multiple-value-bind (major minor) (linux-major-minor directory)
      (make-block-device
       :name name
       :node node
       :type (linux-device-type name whole-p)
       :parent parent
       :size (when sectors (* sectors +sysfs-sector+))
       :block-size (read-integer-file
                    (merge-pathnames (if whole-p
                                         "queue/logical_block_size"
                                         "../queue/logical_block_size")
                                     directory))
       :read-only (parse-boolean-file (merge-pathnames "ro" directory))
       :removable (when whole-p
                    (parse-boolean-file (merge-pathnames "removable" directory)))
       :model (linux-device-model (if whole-p directory
                                      (merge-pathnames "../" directory)))
       :mount-point (car mount)
       :fs-type (cdr mount)
       :used (car use)
       :available (cdr use)
       :major major
       :minor minor
       :rotational (parse-boolean-file
                    (merge-pathnames (if whole-p
                                         "queue/rotational"
                                         "../queue/rotational")
                                     directory))
       :start (unless whole-p (read-integer-file (merge-pathnames "start" directory)))
       ;; A loop device only has loop/backing_file once something is attached,
       ;; which is exactly the distinction wanted: an idle loopN is not a
       ;; virtual disk, it is an empty slot.
       :virtual (and whole-p
                     (read-first-line (merge-pathnames "loop/backing_file" directory))
                     t)))))

(defun map-linux-block-devices (function)
  "Every whole device in /sys/block, then its partitions.  A partition is a
subdirectory carrying a `partition` file -- which is how the kernel says so,
and cheaper than guessing from the name."
  (let ((mounts (linux-mount-table))
        (usage (linux-usage-table)))
    (dolist (name (ignore-errors (read-directory-names "/sys/block/")))
      (let ((directory (sb-ext:parse-native-namestring
                        (concatenate 'string "/sys/block/" name "/"))))
        (funcall function
                 (linux-block-device name directory :mounts mounts :usage usage))
        (dolist (child (or (ignore-errors (read-directory-names directory)) '()))
          (let ((sub (sb-ext:parse-native-namestring
                      (concatenate 'string (namestring directory) child "/"))))
            (when (read-first-line (merge-pathnames "partition" sub))
              (funcall function
                       (linux-block-device child sub :parent name
                                                     :mounts mounts :usage usage)))))))))

;;; ------------------------------------------------------------------ Darwin

(defparameter +diskutil-record-separator+ "**********")

(defun diskutil-key-value (line)
  "\"   Device Node:   /dev/disk0\" -> (VALUES \"Device Node\" \"/dev/disk0\").
NIL for continuation lines and blanks, which carry no key."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Return) line))
         (colon (position #\: trimmed)))
    (when (and colon (plusp colon))
      (values (string-right-trim " " (subseq trimmed 0 colon))
              (string-left-trim " " (subseq trimmed (1+ colon)))))))

(defun diskutil-exact-bytes (text)
  "The exact byte count out of `500.3 GB (500277792768 Bytes) (exactly ...)`.

The parenthetical is the whole reason this reads `diskutil info` rather than
`diskutil list`: the leading figure is rounded to one decimal, so a listing
built from it could not answer (> .size 100gb) correctly."
  (when text
    (let ((open (position #\( text)))
      (when open
        (let ((n (parse-integer text :start (1+ open) :junk-allowed t)))
          (when (and n (search "Bytes" text :start2 open)) n))))))

(defun diskutil-yes (text)
  (and text (string-equal text "Yes")))

(defun diskutil-record-device (fields)
  "One parsed record as a BLOCK-DEVICE, or NIL if it names no device."
  (flet ((f (key) (cdr (assoc key fields :test #'string-equal))))
    (let ((name (f "Device Identifier")))
      (when name
        (let* ((whole (diskutil-yes (f "Whole")))
               (parent (f "Part of Whole"))
               (mounted (diskutil-yes (f "Mounted"))))
          (make-block-device
           :name name
           :node (f "Device Node")
           ;; A macOS volume is not a partition: an APFS volume lives in a
           ;; container and has no fixed extent.  Calling both :PARTITION would
           ;; be inventing a common name for things that are not the same.
           :type (cond (whole :disk)
                       ((search "Volume" (or (f "Content (IOContent)") "")) :volume)
                       (mounted :volume)
                       (t :partition))
           :parent (unless (and parent (string= parent name)) parent)
           :size (diskutil-exact-bytes (or (f "Disk Size") (f "Volume Total Space")))
           :block-size (let ((text (f "Device Block Size")))
                         (when text (parse-integer text :junk-allowed t)))
           :read-only (diskutil-yes (f "Media Read-Only"))
           :removable (let ((text (f "Removable Media")))
                        (and text (not (string-equal text "Fixed"))))
           :model (f "Device / Media Name")
           :mount-point (when mounted (f "Mount Point"))
           :used (when mounted (diskutil-exact-bytes (f "Volume Used Space")))
           :available (when mounted
                        (diskutil-exact-bytes (or (f "Volume Free Space")
                                                  (f "Container Free Space"))))
           :fs-type (or (f "File System Personality") (f "Type (Bundle)"))
           :content (f "Content (IOContent)")
           :protocol (f "Protocol")
           :internal (let ((text (f "Device Location")))
                       (and text (string-equal text "Internal")))
           :virtual (diskutil-yes (f "Virtual"))))))))

(defun map-darwin-block-devices (function)
  "One `diskutil info -all`, split on its record separator.

One subprocess for the whole table rather than one per device, which is the
same reason PS runs `ps axo` once."
  (with-command (proc (list "diskutil" "info" "-all") '(:output :stream)
                 :stderr :capture :on-exit :signal)
    (let ((out (sb-ext:process-output proc))
          (fields '()))
      (flet ((flush ()
               (when fields
                 (let ((device (diskutil-record-device (nreverse fields))))
                   (when device (funcall function device)))
                 (setf fields '()))))
        (loop for line = (read-line out nil nil)
              while line
              do (if (search +diskutil-record-separator+ line)
                     (flush)
                     (multiple-value-bind (key value) (diskutil-key-value line)
                       (when key (push (cons key value) fields)))))
        (flush)))))

;;; ------------------------------------------------------------------- stage

(defun map-block-devices (function)
  "Call FUNCTION with each BLOCK-DEVICE this platform can describe."
  #+linux  (map-linux-block-devices function)
  #+darwin (map-darwin-block-devices function)
  #-(or linux darwin)
  (error "DISKS has no reader for this platform: ~a." (lisp-implementation-type)))

(defstage disks ()
  "Emit a BLOCK-DEVICE per disk, partition and volume -- mounted or not.

Sizes are in BYTES, like LS's .size and PS's .rss, so one 100gb literal means
the same thing against any of them.  .used and .available are set only for a
mounted device, and are filesystem facts rather than device ones: on an APFS
volume .size is the whole container, because that is what an APFS volume
actually has -- it has no fixed extent of its own.

There are no selection options, for the reason PS gives: narrowing is WHERE and
ordering is SORT-BY.  Everything is emitted, including Linux loop and ram
devices, which lsblk hides by default -- .type is what makes that workable.

  disks | where {(eq .type :disk)} | table
  disks | where {.mount-point} | sort-by .size :desc | table
  disks | where {(> .size 100gb)} | table :columns (list :name :size :mount-point)

The two platforms are read very differently and are not equally trustworthy.
Linux reads /sys/block directly -- a kernel-stable interface, no subprocess.
macOS parses `diskutil info -all`, which is a user-facing tool rather than an
interface; it is the best unprivileged source there, since /dev/disk* needs
root to open.  Fields the other platform has no answer for are NIL: .major,
.minor, .rotational and .start are Linux-only, .content, .protocol, .internal
are macOS-only; .virtual is set on both, from a disk image on macOS and from an
attached loop device on Linux."
  (:consumes nil) (:produces :objects)
  (map-block-devices (lambda (device) (emit device))))
