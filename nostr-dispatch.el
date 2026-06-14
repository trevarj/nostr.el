;;; nostr-dispatch.el --- Open Nostr identifiers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; User-facing dispatcher for public NIP-19 and hex identifiers.

;;; Code:

(require 'subr-x)
(require 'nostr-db)
(require 'nostr-nip)
(require 'nostr-profile)
(require 'nostr-relay)
(require 'nostr-search)
(require 'nostr-thread)

(defconst nostr-dispatch-hex-pubkey-regexp "\\`[0-9a-fA-F]\\{64\\}\\'"
  "Regexp matching a hex public key or event id.")

(defun nostr-dispatch--event-by-id (event-id)
  "Return cached EVENT-ID as an event alist."
  (car (nostr-db-select-thread event-id)))

(defun nostr-dispatch-open-hex (value)
  "Open hex VALUE as a cached note or profile."
  (if-let* ((event (nostr-dispatch--event-by-id value)))
      (nostr-thread-open event)
    (nostr-profile-open (downcase value))))

(defun nostr-dispatch-open-decoded (decoded)
  "Open a backend DECODED NIP-19 response."
  (let ((entity (alist-get 'entity decoded nil nil #'equal)))
    (pcase entity
      ((or "npub" "nprofile")
       (nostr-profile-open (alist-get 'pubkey decoded)))
      ((or "note" "nevent")
       (if-let* ((event (nostr-dispatch--event-by-id (alist-get 'event_id decoded))))
           (nostr-thread-open event)
         (nostr-search-open (alist-get 'event_id decoded))))
      ("naddr"
       (let* ((kind (alist-get 'kind decoded))
              (pubkey (alist-get 'pubkey decoded))
              (identifier (alist-get 'identifier decoded))
              (event (and kind pubkey
                          (nostr-db-select-addressable-event
                           kind pubkey identifier))))
         (if event
             (nostr-thread-open event)
           (nostr-relay-fetch-addressable-event
            kind pubkey identifier (alist-get 'relays decoded))
           (message "Fetching addressable event %s:%s:%s"
                    kind pubkey (or identifier ""))
           (nostr-search-open
            (format "%s:%s:%s" kind pubkey (or identifier ""))))))
      (_ (user-error "Unsupported Nostr identifier: %s" entity)))))

;;;###autoload
(defun nostr-open-identifier (value)
  "Open Nostr identifier VALUE.
VALUE may be a hex pubkey/event id, or a public NIP-19 identifier."
  (interactive "sNostr identifier: ")
  (let ((value (string-trim value)))
    (when (string-prefix-p "@" value)
      (setq value (substring value 1)))
    (setq value (string-remove-prefix "nostr:" value))
    (cond
     ((string-empty-p value)
      (user-error "Identifier cannot be empty"))
     ((string-match-p nostr-dispatch-hex-pubkey-regexp value)
      (nostr-dispatch-open-hex value))
     ((or (string-prefix-p "npub1" value)
          (string-prefix-p "nprofile1" value)
          (string-prefix-p "note1" value)
          (string-prefix-p "nevent1" value)
          (string-prefix-p "naddr1" value))
      (nostr-dispatch-open-decoded (nostr-nip19-decode-sync value)))
     (t
      (nostr-search-open value)))))

(provide 'nostr-dispatch)
;;; nostr-dispatch.el ends here
