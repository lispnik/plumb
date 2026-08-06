;;;; build.lisp -- sbcl --script build.lisp   (or just: make)
;;;;
;;;; Dumps bin/plumb via ASDF's PROGRAM-OP.  PROGRAM-OP saves the *running*
;;;; image and exits the process, so nothing after ASDF:MAKE ever runs -- and
;;;; anything the binary should remember has to be set before that call.

(require :asdf)

(defparameter *root* (directory-namestring *load-truename*))

;; The vendored tree comes BEFORE :inherit-configuration on purpose.  Without
;; it, ASDF was resolving Ironclad out of a *different* project under the
;; user's (:tree "~/Projects/common-lisp/") -- `make crypto` worked only because
;; a neighbouring checkout happened to vendor a copy.
(asdf:initialize-source-registry
 `(:source-registry (:directory ,*root*)
                    (:tree ,(merge-pathnames "ocicl/" *root*))
                    :inherit-configuration))

;; SAVE-LISP-AND-DIE will not create the directory for us.
(ensure-directories-exist (merge-pathnames "bin/" *root*))

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "plumb/cli"))

;; Every optional system, always.  The core is deliberately free of external
;; libraries so `asdf:load-system "plumb"` and `make test` need nothing outside
;; SBCL -- but a `plumb` on your PATH should have everything, so the BINARY
;; loads them all.  Each defines into the PLUMB package and exports at load
;; time, so its stages arrive as ordinary built-ins.
;;
;; A failure here is fatal on purpose.  A binary silently missing FROM-JSON or
;; DIGEST is the same trap as a build silently picking a flavour: it reports
;; success and the stage is simply not there.
(handler-bind ((warning #'muffle-warning))
  (dolist (system '("plumb/json" "plumb/csv" "plumb/sql" "plumb/crypto"))
    (asdf:load-system system)))

;; Bake the version into the image, so the system definition stays the single
;; source of truth and the binary does not need ASDF at runtime.
(setf (symbol-value (uiop:find-symbol* '#:*version* '#:plumb.cli))
      (asdf:component-version (asdf:find-system "plumb")))

(asdf:make "plumb/cli")
