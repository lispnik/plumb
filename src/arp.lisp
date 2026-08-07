;;;; arp.lisp -- hosts and interfaces on the local network, as objects.
;;;;
;;;; A wrapper around the arp-scan project, which is a sibling checkout rather
;;;; than a vendored library -- the one dependency here that is not under
;;;; ocicl/, and named explicitly in the Makefile so it is a stated exception
;;;; instead of something ASDF happens to find.
;;;;
;;;; The argument for it is the one PS makes: arp-scan already does the hard
;;;; part -- libpcap, the ARP frames, the OUI database -- and what plumb adds is
;;;; that a host arrives as an object with typed fields, so WHERE and SORT-BY
;;;; replace whatever selection flags the command would otherwise grow.
;;;;
;;;; Two things are worth knowing before using these.
;;;;
;;;; SCANNING NEEDS ROOT.  Raw packet send and receive do, on both platforms.
;;;; INTERFACES does not, which is why it is the half the test suite can
;;;; actually exercise -- and why HOSTS says so in its error rather than
;;;; failing somewhere inside libpcap.
;;;;
;;;; A SCAN TAKES SECONDS, and that is a property of the network, not of this
;;;; code: requests go out across the range and then there is a listen window.
;;;; So HOSTS is a barrier in practice even though nothing here buffers -- the
;;;; underlying call returns everything at once.

(in-package #:plumb)

;;; ------------------------------------------------------------------- hosts

(defstruct (arp-host (:conc-name arp-))
  ip                                    ; dotted quad, as a string
  mac                                   ; colon-separated, as a string
  vendor                                ; from the OUI database, or NIL
  kind                                  ; :global :randomized :multicast
  rtt                                   ; seconds, or NIL unless asked for
  fingerprint
  first-seen last-seen                  ; universal times, like LS's .mtime
  replies requests
  padding-kind                          ; :none :zero :leak
  alt-macs)                             ; other MACs that claimed this IP

(defmethod present ((h arp-host))
  (format nil "~16a ~18a ~@[~a~]" (arp-ip h) (arp-mac h) (arp-vendor h)))

(defun arp-host-of (host)
  "One arp-scan HOST record as a plumb object.

The byte vectors become strings here rather than being passed through: a
4-element vector is not what a table, a filter or a person wants, and FORMAT-IP
and FORMAT-MAC are arp-scan's own renderings, so they agree with what the
command prints."
  (make-arp-host
   :ip (arp-scan:format-ip (arp-scan:host-ip host))
   :mac (arp-scan:format-mac (arp-scan:host-mac host))
   :vendor (arp-scan:host-vendor host)
   :kind (arp-scan:host-kind host)
   :rtt (arp-scan:host-rtt host)
   :fingerprint (arp-scan:host-fingerprint host)
   :first-seen (arp-scan:host-first-seen host)
   :last-seen (arp-scan:host-last-seen host)
   :replies (arp-scan:host-reply-count host)
   :requests (arp-scan:host-request-count host)
   :padding-kind (arp-scan:host-padding-kind host)
   ;; ((mac . count) ...) with the MAC as bytes; rendered, since a conflicting
   ;; claim is exactly the thing someone will want to read.
   :alt-macs (mapcar (lambda (entry)
                       (list :mac (arp-scan:format-mac (car entry))
                             :count (cdr entry)))
                     (arp-scan:host-alt-macs host))))

;;; -------------------------------------------------------------- interfaces

(defstruct (arp-interface (:conc-name nic-))
  name ip netmask mac up removable-flags loopback)

(defmethod present ((i arp-interface))
  (format nil "~10a ~@[~a~]~@[  ~a~]" (nic-name i) (nic-ip i) (nic-mac i)))

(defun arp-interface-of (iface)
  (make-arp-interface
   :name (arp-scan:iface-name iface)
   :ip (let ((ip (arp-scan:iface-ip iface))) (when ip (arp-scan:format-ip ip)))
   :netmask (let ((m (arp-scan:iface-netmask iface)))
              (when m (arp-scan:format-ip m)))
   :mac (let ((mac (arp-scan:iface-mac iface)))
          (when mac (arp-scan:format-mac mac)))
   :up (and (arp-scan:iface-up-p iface) t)
   :loopback (and (arp-scan:iface-loopback-p iface) t)
   :removable-flags (arp-scan:iface-flags iface)))

;;; ------------------------------------------------------------------ stages

(defstage interfaces ()
  "Emit an ARP-INTERFACE per network interface.

  interfaces | where {.up} | table
  interfaces | where {(and .ip (not .loopback))} | table :columns (list :name :ip :mac)

Needs no privileges, unlike HOSTS.  Addresses are rendered as strings -- a
4-byte vector is not what a table or a filter wants -- using arp-scan's own
FORMAT-IP and FORMAT-MAC, so they read the same as what the command prints."
  (:consumes nil) (:produces :objects)
  (dolist (iface (arp-scan:get-ifaces))
    (emit (arp-interface-of iface))))

(defstage hosts (&key interface cidr passive fingerprint
                      (wait-seconds 2) (retry 2))
  "Scan the local network and emit an ARP-HOST per machine that answers.

  hosts | table
  hosts | where {(search \"Raspberry\" .vendor)} | table :columns (list :ip :mac)
  hosts | where {.alt-macs} | print-items     ; two MACs claiming one IP
  hosts :passive | table                       ; listen only, send nothing

NEEDS ROOT.  Raw packet send and receive do on both platforms, so this signals
with that advice rather than failing somewhere inside libpcap.  INTERFACES needs
no privileges and is the one to reach for first.

Takes SECONDS by nature: requests go out across the range and then there is a
listen window, which :WAIT-SECONDS and :RETRY control.  :PASSIVE transmits
nothing at all and only listens, which is the polite option on a network you do
not own.  :FINGERPRINT probes each host with the malformed-frame matrix and is
emphatically not polite.

.RTT is NIL unless the underlying scan measured it, and .VENDOR is NIL for a MAC
the OUI database does not know -- both are absent facts rather than errors, so
filter with WHERE rather than expecting them."
  (:consumes nil) (:produces :objects)
  (let ((records (handler-case
                     (arp-scan:scan-hosts :interface interface :cidr cidr
                                          :passive passive
                                          :fingerprint fingerprint
                                          :wait-seconds wait-seconds
                                          :retry retry)
                   (error (c)
                     ;; libpcap's own failure is unhelpful about the cause, and
                     ;; the cause is almost always privileges.
                     (error "arp scan failed: ~a~%~
Raw packet capture needs root -- try: sudo plumb 'hosts | table'~%~
INTERFACES needs no privileges." c)))))
    (dolist (host records)
      (emit (arp-host-of host)))))

;;; Exported here rather than in package.lisp: these symbols only name anything
;;; once this system is loaded, and HELP lists exported symbols that are FBOUND.

(export '(hosts interfaces
          arp-host arp-host-p make-arp-host
          arp-ip arp-mac arp-vendor arp-kind arp-rtt arp-fingerprint
          arp-first-seen arp-last-seen arp-replies arp-requests
          arp-padding-kind arp-alt-macs
          arp-interface arp-interface-p make-arp-interface
          nic-name nic-ip nic-netmask nic-mac nic-up nic-loopback)
        '#:plumb)
