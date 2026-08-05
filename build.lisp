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

;; PLUMB_CRYPTO=1 (that is, `make crypto`) adds the digest stages.  Off by
;; default because Ironclad is the only external dependency in the project and
;; the plain build must work without it.  Loading it here is enough: crypto.lisp
;; defines into PLUMB and exports at load time, so DIGEST reaches the dumped
;; image as an ordinary built-in.  Ironclad comes from ocicl/, vendored in this
;; repository -- no network, no Quicklisp, no dependency manager at build time.
(when (sb-ext:posix-getenv "PLUMB_CRYPTO")
  (handler-bind ((warning #'muffle-warning))
    (asdf:load-system "plumb/crypto")))

;; Bake the version into the image, so the system definition stays the single
;; source of truth and the binary does not need ASDF at runtime.
(setf (symbol-value (uiop:find-symbol* '#:*version* '#:plumb.cli))
      (asdf:component-version (asdf:find-system "plumb")))

(asdf:make "plumb/cli")
