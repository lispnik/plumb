;;;; plumb.asd

(defsystem "plumb"
  :description "Thread-and-channel pipelines carrying Lisp objects instead of bytes."
  :author "Matthew"
  :license "MIT"
  :version "0.1.0"
  ;; sb-thread / sb-mop are in the SBCL core; sb-introspect is a contrib, used
  ;; only so HELP can show a lambda list for the non-stage built-ins.
  :depends-on ((:require :sb-introspect) (:require :sb-posix))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "ansi")
               (:file "stat")
               (:file "glob")
               (:file "channel")
               (:file "field")
               (:file "present")
               ;; STAGE before READER: the word-mode reader asks *STAGES* for a
               ;; stage's lambda list, to tell a keyword that is a flag from a
               ;; keyword that is a required argument.
               (:file "stage")
               (:file "reader")
               (:file "pipeline")
               (:file "stages")
               (:file "process")
               (:file "explain")
               (:file "watch")
               (:file "help"))
  :in-order-to ((test-op (test-op "plumb/tests"))))

;;; The executable.  No :PATHNAME here on purpose: :BUILD-PATHNAME is resolved
;;; against the system's directory, so a :PATHNAME of "src" would put the
;;; binary in src/bin/.  Build it with `make`, or with
;;;   (asdf:make "plumb/cli")
;;; -- note that PROGRAM-OP dumps the running image and exits the process.

(defsystem "plumb/cli"
  :description "The plumb command-line executable."
  :author "Matthew"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("plumb" (:require :sb-posix))   ; termios, for raw-mode line editing
  :serial t
  :components ((:module "src" :components ((:file "lineedit") (:file "cli"))))
  :build-operation "program-op"
  :build-pathname "bin/plumb"
  :entry-point "plumb.cli:main")

(defsystem "plumb/crypto"
  :description "Digest stages for plumb, on top of Ironclad."
  :author "Matthew"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("plumb" "ironclad")
  :serial t
  :components ((:module "src" :components ((:file "crypto"))))
  :in-order-to ((test-op (test-op "plumb/crypto/tests"))))

(defsystem "plumb/tests"
  :description "Test suite for plumb."
  :depends-on ("plumb/cli")             ; also covers the editor and the reader
  :serial t
  :pathname "tests"
  :components ((:file "tests"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :plumb/tests '#:run-tests)
               (error "plumb test suite failed."))))

(defsystem "plumb/crypto/tests"
  :description "Test suite for the digest stages."
  :depends-on ("plumb/crypto" "plumb/tests")
  :serial t
  :pathname "tests"
  :components ((:file "crypto"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :plumb/tests '#:run-crypto-tests)
               (error "plumb/crypto test suite failed."))))
