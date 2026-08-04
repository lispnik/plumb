;;;; demo.lisp -- sbcl --script demo.lisp   (from this directory)

(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:directory ,(directory-namestring *load-truename*))
                    :inherit-configuration))
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "plumb"))

(in-package #:plumb)

(format t "~&== ls | where {(> .size 1kb)} | sort-by .size :desc | take 5~%")
(each (list (ls (merge-pathnames "src/" *load-truename*))
            (where ($ (and (not (fld :dir-p)) (> (or (fld :size) 0) 1024))))
            (sort-by ($ (fld :size)) :desc t)
            (take 5))
      (lambda (f) (format t "  ~8d  ~a~%" (file-entry-size f) (file-entry-name f))))

(format t "~&~%== infinite source, bounded consumer~%")
(format t "  ~s~%" (collect-pipeline (list (counter) (where #'oddp) (take 6))))

(format t "~&~%== the source thread really is gone~%")
(let ((pipe (run (list (counter) (take 3)))))
  (join pipe)
  (format t "  live threads: ~d~%"
          (count-if #'sb-thread:thread-alive-p (pipeline-threads pipe))))

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
