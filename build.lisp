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
(defparameter *arp-scan*
  (let ((override (sb-ext:posix-getenv "PLUMB_ARPSCAN")))
    (if (and override (plusp (length override)))
        (pathname (if (char= (char override (1- (length override))) #\/)
                      override
                      (concatenate 'string override "/")))
        (merge-pathnames "../arp-scan/" *root*)))
  "The one unvendored dependency: a sibling checkout, under active development,
so a pinned copy here would mean maintaining two.  Named rather than left to the
user's own registry -- see the Makefile, which passes PLUMB_ARPSCAN so that the
build and the test targets cannot disagree about where it is.")

(asdf:initialize-source-registry
 `(:source-registry (:directory ,*root*)
                    (:tree ,(merge-pathnames "ocicl/" *root*))
                    (:directory ,*arp-scan*)
                    (:tree ,(merge-pathnames "ocicl/" *arp-scan*))
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
    (asdf:load-system system))
  ;; plumb/arp only if the sibling checkout is there, since it is the one
  ;; dependency this repository cannot supply itself.  A missing arp-scan makes
  ;; a binary without HOSTS, not a failed build -- which --version reports.
  (if (probe-file (merge-pathnames "arp-scan.asd" *arp-scan*))
      (asdf:load-system "plumb/arp")
      (format t "~&; no arp-scan checkout at ~a -- building without plumb/arp~%"
              *arp-scan*)))

;; Bake the version into the image, so the system definition stays the single
;; source of truth and the binary does not need ASDF at runtime.
(setf (symbol-value (uiop:find-symbol* '#:*version* '#:plumb.cli))
      (asdf:component-version (asdf:find-system "plumb")))

(asdf:make "plumb/cli")
