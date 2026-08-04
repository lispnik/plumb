;;;; build.lisp -- sbcl --script build.lisp   (or just: make)
;;;;
;;;; Dumps bin/plumb via ASDF's PROGRAM-OP.  PROGRAM-OP saves the *running*
;;;; image and exits the process, so nothing after ASDF:MAKE ever runs -- and
;;;; anything the binary should remember has to be set before that call.

(require :asdf)

(defparameter *root* (directory-namestring *load-truename*))

(asdf:initialize-source-registry
 `(:source-registry (:directory ,*root*) :inherit-configuration))

;; SAVE-LISP-AND-DIE will not create the directory for us.
(ensure-directories-exist (merge-pathnames "bin/" *root*))

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "plumb/cli"))

;; Bake the version into the image, so the system definition stays the single
;; source of truth and the binary does not need ASDF at runtime.
(setf (symbol-value (uiop:find-symbol* '#:*version* '#:plumb.cli))
      (asdf:component-version (asdf:find-system "plumb")))

(asdf:make "plumb/cli")
