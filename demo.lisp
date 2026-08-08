;;;; demo.lisp -- sbcl --script demo.lisp   (from this directory)

(require :asdf)
(defparameter *here* (directory-namestring *load-truename*))

(asdf:initialize-source-registry
 `(:source-registry (:directory ,*here*)
                    (:tree ,(merge-pathnames "ocicl/" *here*))
                    (:directory ,(merge-pathnames "../arp-scan/" *here*))
                    (:tree ,(merge-pathnames "../arp-scan/ocicl/" *here*))
                    ;; NOT :inherit-configuration.  Everything needed is named
                    ;; above, and inheriting let this file quietly resolve jzon,
                    ;; cl-csv and Ironclad out of a NEIGHBOURING project when
                    ;; ocicl/ was missing -- so the "skipped cleanly" path could
                    ;; not be tested, and a demo would appear to work on a
                    ;; checkout where the build does not.
                    :ignore-inherited-configuration))

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "plumb")
  ;; The optional systems, each skipped rather than fatal: this file is a
  ;; demonstration, and it should still run most of itself on a checkout with
  ;; no vendored tree or no sibling arp-scan.  It covered NONE of them until
  ;; now, which meant `make demo` -- the only thing that exercises the real
  ;; binary -- said nothing about half the surface.
  (dolist (system '("plumb/json" "plumb/csv" "plumb/sql" "plumb/crypto" "plumb/arp"))
    (handler-case (asdf:load-system system)
      (error () (format t "~&; ~a unavailable -- skipping its section~%" system)))))

(in-package #:plumb)

;;; A glob is a STRING, never a pathname.  MERGE-PATHNAMES turns "src/a*.lisp"
;;; into a pathname whose :NAME is a wild PATTERN object, and LS then cannot
;;; take a native namestring of it -- see CLAUDE.md, "filenames are strings".
(defparameter *root* (directory-namestring *load-truename*))
(defun in-root (relative) (concatenate 'string *root* relative))

(format t "~&== ls | where {(> .size 1kb)} | sort-by .size :desc | take 5~%")
(each (list (ls (merge-pathnames "src/" *load-truename*))
            (where ($ (and (not (fld :dir-p)) (> (or (fld :size) 0) 1024))))
            (sort-by ($ (fld :size)) :desc t)
            (take 5))
      (lambda (f) (format t "  ~8d  ~a~%" (file-entry-size f) (file-entry-name f))))

(format t "~&~%== infinite source, bounded consumer~%")
(format t "  ~s~%" (collect-pipeline (list (counter) (where #'oddp) (take 6))))

(format t "~&~%== the source stage really is gone~%")
(let ((pipe (run (list (counter) (take 3)))))
  (join pipe)
  ;; Stages, not threads.  The threads are pooled, so they go back to the pool
  ;; rather than dying -- what teardown ends is the stage.
  (format t "  still running: ~d of ~d stages~%"
          (count-if #'task-live-p (pipeline-tasks pipe))
          (length (pipeline-tasks pipe))))

(format t "~&~%== errors are objects, not text on fd 2~%")
(let* ((err (make-channel))
       (pipe (run (list (from-list '(1 2 0 4))
                        (xform (lambda (n) (/ 100 n))))
                  :err err)))
  (join pipe)
  (close-output err)
  (let ((c (recv err)))
    (format t "  caught a live ~a: ~a~%" (type-of c) c)))

(format t "~&~%== mismatches are caught before any thread starts~%")
(handler-case (run (list (from-list '(1)) (to-text) (where #'evenp)))
  (pipeline-type-error (c) (format t "  ~a~%" c)))

(format t "~&~%== tally by extension~%")
(each (list (ls (merge-pathnames "src/" *load-truename*))
            (where ($ (not (fld :dir-p))))
            (tally :key ($ (or (pathname-type (fld :path)) "-"))))
      (lambda (row) (format t "  ~5d  ~a~%" (getf row :count) (getf row :key))))

;;; The CLI is the same library with a Lisp-speaking argument parser in front
;;; of it.  Running it as a subprocess and reading its stdout back through
;;; LINES is the round trip: objects -> text -> objects.

(format t "~&~%== bin/plumb as a subprocess, its stdout back through `sh`~%")
;; Against *LOAD-TRUENAME* itself this would inherit the type "lisp" and go
;; looking for bin/plumb.lisp; a directory has no name or type to donate.
(let ((binary (merge-pathnames "bin/plumb" (directory-namestring *load-truename*)))
      (form "(list (counter :limit 200) (where #'oddp))"))
  (if (not (probe-file binary))
      (format t "  not built -- run `make` first~%")
      (progn
        (format t "  $ plumb -e ~s~%" form)
        ;; The list form execs directly, so FORM reaches the child as one
        ;; argument with no shell to re-split it.
        (each (list (sh (list (namestring binary) "-e" form))
                    (where ($ (search "7" (fld :text))))
                    (take 4))
              (lambda (l) (format t "  line ~3d: ~a~%" (line-number l) (line-text l)))))))

(format t "~&~%== a command that fails is a condition, like any other stage error~%")
(let ((pipe (run (list (sh "ls /nonexistent-plumb-path")))))
  (join pipe)
  (let ((c (cdr (first (pipeline-failures pipe)))))
    (format t "  ~a: ~a~%" (type-of c) c)))

(format t "~&~%== and TAKE against an endless command really does stop it~%")
(let ((start (get-internal-real-time)))
  (format t "  ~s in ~,2fs, child killed on the way out~%"
          (mapcar #'present (collect-pipeline (list (sh "yes plumb") (take 3))))
          (/ (- (get-internal-real-time) start) internal-time-units-per-second)))

(format t "~&~%== presentation: a table, sized once the last row is in~%")
(each (list (ls (merge-pathnames "src/" (directory-namestring *load-truename*)))
            (where ($ (not (fld :dir-p))))
            (sort-by ($ (or (fld :size) 0)) :desc t)
            (take 4)
            (table :columns (list :name :size)))
      #'identity)

;;; ------------------------------------------------------- the optional systems
;;;
;;; Each guarded on its stage being registered, so this file runs on a checkout
;;; that has not vendored anything.

(defun have (stage) (gethash stage *stages*))

(when (have 'to-json)
  (format t "~&~%== JSON: any object serialises, through FIELDS and FIELD~%")
  (each (list (ls (in-root "src/a*.lisp"))
              (xform ($ (list :name (fld :name) :size (fld :size))))
              (to-json :pretty t))
        (lambda (text) (format t "~a~%" text))))

(when (have 'from-json)
  (format t "~&~%== ...and back, with a top-level array spread into objects~%")
  (each (list (from-list (list "[{\"host\":\"a\",\"up\":true},{\"host\":\"b\",\"up\":false}]"))
              (from-json)
              (where ($ (fld :up)))
              (table :columns (list :host :up)))
        #'identity))

(when (have 'to-csv)
  (format t "~&~%== CSV: quoting is the writer's job, and survives a round trip~%")
  (each (list (from-list (list (list :name "has,comma" :n 1)
                               (list :name "has\"quote" :n 2)))
              (to-csv))
        (lambda (line) (format t "  ~a~%" line))))

(when (have 'digest)
  (format t "~&~%== digests: one object per file, shasum(1)'s own line format~%")
  (each (list (ls (in-root "src/a*.lisp"))
              (digest :sha256))
        (lambda (d) (format t "  ~a~%" (present d)))))

(when (have 'to-sql)
  (format t "~&~%== SQL: persist once, then let the engine do the aggregating~%")
  (let ((db "/tmp/plumb-demo.db"))
    (ignore-errors (delete-file db))
    (join (run (list (ls (in-root "src/*.lisp"))
                     (xform ($ (list :name (fld :name) :size (fld :size))))
                     (to-sql "files" :database db :create t))))
    (each (list (from-sql "select count(*) as files, sum(size) as bytes,
                                  max(size) as largest from files where size > ?"
                          :database db :params (list 1024))
                (table))
          #'identity)
    (ignore-errors (delete-file db))))

(when (have 'interfaces)
  (format t "~&~%== the network: interfaces need no privileges (a scan needs root)~%")
  (each (list (interfaces)
              (where ($ (and (fld :ip) (not (fld :loopback)))))
              (table :columns (list :name :ip :netmask :mac)))
        #'identity))

(format t "~&~%== every stage this binary has~%")
(format t "  ~d stages: ~{~(~a~)~^ ~}~%"
        (hash-table-count *stages*)
        (sort (loop for k being the hash-keys of *stages* collect k) #'string<))
