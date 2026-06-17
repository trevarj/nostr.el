;;; nostr-url.el --- URL host safety guards for nostr.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; SSRF guards for outbound HTTP(S) requests driven by relay-supplied URLs.
;; Nostr event content and kind-0 metadata can name arbitrary hosts (media
;; URLs, NIP-05 domains), so before issuing a request from the user's machine
;; the client refuses hosts that are, or resolve to, private/loopback/link-local
;; addresses (including cloud metadata endpoints).  Used by `nostr-media' and
;; `nostr-nip' before fetching.

;;; Code:

(require 'cl-lib)
(require 'dns)
(require 'subr-x)
(require 'url-parse)

(defgroup nostr-url nil
  "URL host safety for outbound Nostr requests."
  :group 'nostr)

(defcustom nostr-url-blocked-hosts
  '("localhost" "metadata.google.internal" "metadata.aws.internal")
  "Hostnames always treated as internal and never fetched.
Compared case-insensitively.  Applied in addition to the private, loopback,
and link-local IP range checks performed by `nostr-url-public-host-p'."
  :type '(repeat string)
  :group 'nostr-url)

(defcustom nostr-url-resolve-hosts t
  "When non-nil, resolve non-literal hostnames before fetching.
A hostname that resolves to any private/loopback/link-local address is
refused, defeating DNS-rebinding to internal hosts.  Disable in environments
without DNS access; IP-literal and blocklist checks still apply."
  :type 'boolean
  :group 'nostr-url)

(defcustom nostr-url-resolve-timeout 2
  "Seconds to wait for a single DNS lookup before giving up.
Bounds the cost of resolving a relay-supplied hostname when DNS is slow or
unreachable.  Only used while `nostr-url-resolve-hosts' is non-nil."
  :type 'number
  :group 'nostr-url)

(defconst nostr-url--ipv4-regexp
  "\\`\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\'"
  "Regexp matching an IPv4 literal with four decimal octets.")

(defun nostr-url--strip-ipv6-brackets (host)
  "Return HOST without surrounding IPv6 brackets.
Uses literal prefix/suffix removal so a host like \"[::1]\" is unwrapped
without interpreting the brackets as regexps (which `string-trim' would)."
  (string-remove-suffix "]" (string-remove-prefix "[" host)))

(defun nostr-url--ipv4-literal-p (host)
  "Return non-nil when HOST is an IPv4 literal."
  (and (stringp host)
       (string-match-p nostr-url--ipv4-regexp host)))

(defun nostr-url--ipv6-literal-p (host)
  "Return non-nil when HOST is an IPv6 literal, with or without brackets.
IPv6 literals always contain a colon; dotted IPv4-mapped forms
(`::ffff:1.2.3.4') are accepted so the embedded IPv4 is checked."
  (and (stringp host)
       (let ((h (nostr-url--strip-ipv6-brackets host)))
         (and (not (string-empty-p h))
              (string-match-p "\\`[0-9a-fA-F:.]*:[0-9a-fA-F:.]*\\'" h)))))

(defun nostr-url--ipv4-private-p (host)
  "Return non-nil when IPv4 literal HOST is an internal address.
Covers 0/8 (unspecified), 10/8 (private), 100.64/10 (CGNAT), 127/8 (loopback),
169.254/16 (link-local, including cloud metadata 169.254.169.254),
172.16/12 (private), 192.168/16 (private), and 224/4+ (multicast/reserved/
limited-broadcast)."
  (when (string-match nostr-url--ipv4-regexp host)
    (let ((o1 (string-to-number (match-string 1 host)))
          (o2 (string-to-number (match-string 2 host))))
      (or (= o1 0)
          (= o1 10)
          (= o1 127)
          (and (= o1 169) (= o2 254))
          (and (= o1 172) (>= o2 16) (<= o2 31))
          (and (= o1 192) (= o2 168))
          (and (= o1 100) (>= o2 64) (<= o2 127))
          (>= o1 224)))))

(defun nostr-url--ipv6-private-p (host)
  "Return non-nil when IPv6 literal HOST is an internal address.
Covers loopback (::1), unique-local fc00::/7 (fc/fd), link-local fe80::/10
(fe80..feb), and IPv4-mapped/compatible forms wrapping a private IPv4."
  (let ((h (nostr-url--strip-ipv6-brackets host)))
    (cond
     ((string-equal-ignore-case h "::1") t)
     ((string-match-p "\\`\\(fc\\|fd\\|fe[89ab]\\)" h) t)
     ((when (string-match "\\`::ffff:\\([0-9.]+\\)\\'" h)
        (nostr-url--ipv4-private-p (match-string 1 h))))
     ((when (string-match "\\`::\\([0-9.]+\\)\\'" h)
        (nostr-url--ipv4-private-p (match-string 1 h))))
     (t nil))))

(defun nostr-url--blocked-hostname-p (host)
  "Return non-nil when HOST is on `nostr-url-blocked-hosts'."
  (seq-some (lambda (blocked)
              (string-equal-ignore-case host blocked))
            nostr-url-blocked-hosts))

(defun nostr-url--resolved-ips (host)
  "Return a list of IP addresses HOST resolves to, or nil.
Uses `dns-query' when available; never signals and never blocks longer than
`nostr-url-resolve-timeout' seconds per query."
  (when (fboundp 'dns-query)
    (ignore-errors
      (let ((dns-timeout (max 0 nostr-url-resolve-timeout)))
        (delq nil (list (dns-query host 'A)
                        (dns-query host 'AAAA)))))))

(defun nostr-url--host-private-p (host &optional resolve)
  "Return non-nil when HOST names an internal address.
IP literals are checked directly; hostnames are checked against
`nostr-url-blocked-hosts' and, when RESOLVE is non-nil and
`nostr-url-resolve-hosts' is non-nil, resolved and re-checked so DNS-rebinding
to a private address is rejected."
  (cond
   ((not (stringp host)) nil)
   ((nostr-url--blocked-hostname-p host) t)
   ((nostr-url--ipv4-literal-p host) (nostr-url--ipv4-private-p host))
   ((nostr-url--ipv6-literal-p host) (nostr-url--ipv6-private-p host))
   ((and resolve nostr-url-resolve-hosts)
    (seq-some #'nostr-url--host-private-p (nostr-url--resolved-ips host)))
   (t nil)))

(defun nostr-url-host (url)
  "Return the lowercased host of URL, or nil when absent."
  (let ((host (url-host (url-generic-parse-url url))))
    (and (stringp host)
         (not (string-empty-p host))
         (downcase host))))

(defun nostr-url-public-host-p (url &optional resolve)
  "Return non-nil when URL targets a public, fetchable host.
Refuses non-http(s) schemes and hosts that are, or resolve to, private,
loopback, or link-local addresses (including cloud metadata endpoints).
When RESOLVE is non-nil (the default) and `nostr-url-resolve-hosts' is non-nil,
non-literal hostnames are resolved so a hostname pointing at an internal
address is also refused.  Pass nil to apply only the deterministic IP-literal
and blocklist checks, used by callers that already gate real network on a
configurable fetch hook."
  (let* ((parsed (url-generic-parse-url url))
         (scheme (downcase (or (url-type parsed) "")))
         (host (url-host parsed)))
    (and (member scheme '("http" "https"))
         (stringp host)
         (not (string-empty-p host))
         (not (nostr-url--host-private-p (downcase host) resolve)))))

(provide 'nostr-url)
;;; nostr-url.el ends here