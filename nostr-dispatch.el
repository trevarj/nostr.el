;;; nostr-dispatch.el --- Open Nostr identifiers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; User-facing dispatcher for npub/note/hex identifiers.

;;; Code:

(require 'subr-x)
(require 'nostr-db)
(require 'nostr-nip)
(require 'nostr-profile)
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
      ("npub" (nostr-profile-open (alist-get 'pubkey decoded)))
      ("note"
       (if-let* ((event (nostr-dispatch--event-by-id (alist-get 'event_id decoded))))
           (nostr-thread-open event)
         (nostr-search-open (alist-get 'event_id decoded))))
      (_ (user-error "Unsupported Nostr identifier: %s" entity)))))

;;;###autoload
(defun nostr-open-identifier (value)
  "Open Nostr identifier VALUE.
VALUE may be a hex pubkey/event id, npub, or note."
  (interactive "sNostr identifier: ")
  (let ((value (string-trim value)))
    (cond
     ((string-empty-p value)
      (user-error "Identifier cannot be empty"))
     ((string-match-p nostr-dispatch-hex-pubkey-regexp value)
      (nostr-dispatch-open-hex value))
     ((or (string-prefix-p "npub1" value)
          (string-prefix-p "note1" value))
      (nostr-dispatch-open-decoded (nostr-nip19-decode-sync value)))
     (t
      (nostr-search-open value)))))

(provide 'nostr-dispatch)
;;; nostr-dispatch.el ends here
