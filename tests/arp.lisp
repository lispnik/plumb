;;;; arp.lisp -- tests for the ARP stages.
;;;;
;;;; Same package and the same CHECK/WITH-TIMEOUT as tests.lisp.  Separate file
;;;; and separate system for the reason tests/sql.lisp is: `make test` must keep
;;;; running with no arp-scan checkout anywhere.
;;;;
;;;; NOTHING here scans.  A scan needs raw packet access and takes seconds, and
;;;; a test that depends on which machines happen to be switched on is not a
;;;; test.  What is checked instead is the CONVERSION -- arp-scan's records into
;;;; plumb objects -- against records built by hand, plus INTERFACES, which is
;;;; real, local and needs no privileges.

(in-package #:plumb/tests)

;;; ARP-HOST-OF and ARP-INTERFACE-OF are plumb internals -- conversion helpers,
;;; not something a pipeline calls -- so the tests reach for them by package,
;;; as the reader and help tests do for theirs.
;;;
;;; MAKE-HOST and MAKE-IFACE are arp-scan internals on purpose: fabricating a
;;; host record is a test fixture, not something the library should invite, so
;;; the accessors are exported and the constructors are not.

(defun sample-host ()
  (arp-scan::make-host
   :ip #(192 168 1 5) :mac #(1 2 3 4 5 6)
   :vendor "Acme Corp" :kind :global :rtt 0.25d0
   :fingerprint "1011" :first-seen 100 :last-seen 200
   :reply-count 3 :request-count 4
   :padding-kind :leak
   :alt-macs (list (cons #(10 11 12 13 14 15) 2))))

(defun test-arp-host-conversion ()
  "Byte vectors become strings here.  A 4-element vector is not what a table, a
filter or a person wants, and arp-scan's own FORMAT-IP/FORMAT-MAC are used so
these read the same as what the command prints."
  (let ((h (plumb::arp-host-of (sample-host))))
    (check (arp-host-p h) :converts-to-an-arp-host)
    (check (string= "192.168.1.5" (arp-ip h)) :ip-is-a-dotted-quad-string)
    (check (stringp (arp-mac h)) :mac-is-a-string)
    (check (= 17 (length (arp-mac h))) :mac-has-the-usual-shape)
    (check (string= "Acme Corp" (arp-vendor h)) :vendor)
    (check (eq :global (arp-kind h)) :kind-stays-a-keyword)
    (check (= 0.25d0 (arp-rtt h)) :rtt)
    (check (string= "1011" (arp-fingerprint h)) :fingerprint)
    ;; Universal times, like LS's .mtime, so one 7d literal compares to both.
    (check (eql 100 (arp-first-seen h)) :first-seen)
    (check (eql 200 (arp-last-seen h)) :last-seen)
    (check (eql 3 (arp-replies h)) :replies)
    (check (eql 4 (arp-requests h)) :requests)
    (check (eq :leak (arp-padding-kind h)) :padding-kind)
    ;; A conflicting claim is exactly what someone will want to read, so the
    ;; alternative MAC is rendered too rather than left as bytes.
    (let ((alt (first (arp-alt-macs h))))
      (check (stringp (field alt :mac)) :alt-mac-is-rendered)
      (check (eql 2 (field alt :count)) :alt-mac-count))))

(defun test-arp-host-absent-facts-are-nil ()
  "RTT is NIL unless the scan measured it and VENDOR is NIL for a MAC the OUI
database does not know.  Both are absent facts, not errors, so they have to
survive conversion as NIL rather than becoming \"NIL\" or breaking it."
  (let ((h (plumb::arp-host-of (arp-scan::make-host :ip #(10 0 0 1) :mac #(0 0 0 0 0 0)))))
    (check (string= "10.0.0.1" (arp-ip h)) :ip-still-converts)
    (check (null (arp-vendor h)) :unknown-vendor-is-nil)
    (check (null (arp-rtt h)) :unmeasured-rtt-is-nil)
    (check (null (arp-fingerprint h)) :unprobed-fingerprint-is-nil)
    (check (null (arp-alt-macs h)) :no-conflicting-claims)
    ;; And PRESENT must not fall over on the absent ones.
    (check (stringp (present h)) :present-tolerates-nils)))

(defun test-arp-interface-conversion ()
  (let ((i (plumb::arp-interface-of
            (arp-scan::make-iface :name "en9" :ip #(192 168 1 9)
                                  :netmask #(255 255 255 0)
                                  :mac #(1 2 3 4 5 6) :flags 0))))
    (check (arp-interface-p i) :converts)
    (check (string= "en9" (nic-name i)) :name)
    (check (string= "192.168.1.9" (nic-ip i)) :ip-is-a-string)
    (check (string= "255.255.255.0" (nic-netmask i)) :netmask-is-a-string)
    (check (stringp (nic-mac i)) :mac-is-a-string))
  ;; An interface with no address at all -- which most have -- must convert.
  (let ((i (plumb::arp-interface-of (arp-scan::make-iface :name "utun0" :flags 0))))
    (check (null (nic-ip i)) :no-address-is-nil)
    (check (null (nic-mac i)) :no-mac-is-nil)
    (check (stringp (present i)) :present-tolerates-nils)))

(defun test-interfaces-stage ()
  "The half that needs no privileges, against the machine actually running it."
  (with-timeout (30 :interfaces)
    (let ((nics (collect-pipeline (list (interfaces)))))
      (check (plusp (length nics)) :some-interfaces-found)
      (check (every #'arp-interface-p nics) :all-are-interfaces)
      (check (every #'nic-name nics) :every-interface-is-named)
      ;; Every machine has a loopback, and it is the one interface whose
      ;; address is knowable without asking the network.
      (let ((lo (find-if #'nic-loopback nics)))
        (check lo :loopback-is-present)
        (when lo
          (check (equal "127.0.0.1" (nic-ip lo)) :loopback-is-127-0-0-1)))
      ;; Addresses are rendered, not raw vectors -- the point of the conversion.
      (check (every (lambda (n) (or (null (nic-ip n)) (stringp (nic-ip n)))) nics)
             :addresses-are-strings)
      (check (every (lambda (n) (or (null (nic-mac n)) (stringp (nic-mac n)))) nics)
             :macs-are-strings))))

(defun test-arp-stages-are-registered ()
  (check (gethash 'plumb::hosts plumb::*stages*) :hosts-is-a-stage)
  (check (gethash 'plumb::interfaces plumb::*stages*) :interfaces-is-a-stage)
  (check (eq :source (plumb::stage-kind (gethash 'plumb::hosts plumb::*stages*)))
         :hosts-is-a-source)
  ;; Both compose like any other source.
  (check (check-pipeline (list (interfaces) (where #'identity) (table)))
         :interfaces-composes)
  (check (check-pipeline (list (hosts) (table))) :hosts-composes)
  ;; The docstring has to say the thing that will otherwise waste someone's
  ;; afternoon.
  (check (search "root" (string-downcase
                         (plumb::si-documentation (gethash 'plumb::hosts plumb::*stages*))))
         :hosts-documents-that-it-needs-privileges))

(defun run-arp-tests ()
  (let ((*passed* 0) (*failed* '()))
    (dolist (fn '(test-arp-host-conversion
                  test-arp-host-absent-facts-are-nil
                  test-arp-interface-conversion
                  test-interfaces-stage
                  test-arp-stages-are-registered))
      (format t "~&; ~a~%" fn)
      (funcall fn))
    (format t "~&~%~d passed, ~d failed~%" *passed* (length *failed*))
    (dolist (f (reverse *failed*))
      (format t "  FAIL: ~s~%" f))
    (null *failed*)))
