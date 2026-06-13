;;; nostr-share.el --- Copy and browse Nostr identifiers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Sharing helpers for selected Nostr UI sections.

;;; Code:

(require 'browse-url)
(require 'subr-x)
(require 'nostr-nip)
(require 'nostr-ui)

(defcustom nostr-share-web-client-base-url "https://njump.me"
  "Base URL used by `nostr-share-browse' commands."
  :type 'string
  :group 'nostr)

(defun nostr-share--selected-or-error ()
  "Return selected Nostr UI data or signal a user error."
  (or (nostr-ui-selected-data)
      (user-error "No Nostr item selected")))

(defun nostr-share--note-id (data)
  "Return note id from selected DATA."
  (or (alist-get 'id data)
      (alist-get 'event-id data)
      (user-error "Selected item has no note id")))

(defun nostr-share--profile-pubkey (data)
  "Return profile pubkey from selected DATA."
  (or (alist-get 'pubkey data)
      (alist-get 'actor-pubkey data)
      (alist-get 'target-pubkey data)
      (user-error "Selected item has no profile pubkey")))

(defun nostr-share--relay-url (data)
  "Return relay URL from selected DATA."
  (or (alist-get 'url data)
      (user-error "Selected item has no relay URL")))

(defun nostr-share--encode-value (entity value)
  "Return NIP-19 ENTITY encoding for VALUE."
  (alist-get 'value (nostr-nip19-encode-sync entity value)))

(defun nostr-share--copy (value label)
  "Copy VALUE to kill ring with LABEL in the status message."
  (kill-new value)
  (message "Copied %s: %s" label value)
  value)

(defun nostr-share--web-url (nip19)
  "Return configured web URL for NIP19."
  (format "%s/%s"
          (string-remove-suffix "/" nostr-share-web-client-base-url)
          nip19))

(defun nostr-share-copy-note-id ()
  "Copy the selected note as a NIP-19 note id."
  (interactive)
  (let* ((data (nostr-share--selected-or-error))
         (note (nostr-share--encode-value "note" (nostr-share--note-id data))))
    (nostr-share--copy note "note id")))

(defun nostr-share-copy-profile-id ()
  "Copy the selected profile as a NIP-19 npub id."
  (interactive)
  (let* ((data (nostr-share--selected-or-error))
         (npub (nostr-share--encode-value "npub" (nostr-share--profile-pubkey data))))
    (nostr-share--copy npub "profile id")))

(defun nostr-share-copy-raw-id ()
  "Copy the selected raw note id, pubkey, or relay URL."
  (interactive)
  (let* ((data (nostr-share--selected-or-error))
         (value (or (alist-get 'id data)
                    (alist-get 'pubkey data)
                    (alist-get 'event-id data)
                    (alist-get 'actor-pubkey data)
                    (alist-get 'target-pubkey data)
                    (alist-get 'url data)
                    (user-error "Selected item has no shareable raw id"))))
    (nostr-share--copy value "raw id")))

(defun nostr-share-copy-relay-url ()
  "Copy the selected relay URL."
  (interactive)
  (nostr-share--copy (nostr-share--relay-url (nostr-share--selected-or-error))
                     "relay URL"))

(defun nostr-share-browse-note ()
  "Open the selected note in `nostr-share-web-client-base-url'."
  (interactive)
  (let* ((data (nostr-share--selected-or-error))
         (note (nostr-share--encode-value "note" (nostr-share--note-id data)))
         (url (nostr-share--web-url note)))
    (browse-url url)
    url))

(defun nostr-share-browse-profile ()
  "Open the selected profile in `nostr-share-web-client-base-url'."
  (interactive)
  (let* ((data (nostr-share--selected-or-error))
         (npub (nostr-share--encode-value "npub" (nostr-share--profile-pubkey data)))
         (url (nostr-share--web-url npub)))
    (browse-url url)
    url))

(defun nostr-share-copy ()
  "Copy the selected note/profile/relay using its most useful public form."
  (interactive)
  (pcase (nostr-ui-section-type (or (nostr-ui-section-at-point)
                                    (user-error "No Nostr item selected")))
    ('note (nostr-share-copy-note-id))
    ('profile (nostr-share-copy-profile-id))
    ('relay (nostr-share-copy-relay-url))
    (_ (nostr-share-copy-raw-id))))

(defun nostr-share-browse ()
  "Open the selected note or profile in the configured web client."
  (interactive)
  (pcase (nostr-ui-section-type (or (nostr-ui-section-at-point)
                                    (user-error "No Nostr item selected")))
    ('note (nostr-share-browse-note))
    ('profile (nostr-share-browse-profile))
    (_ (user-error "Selected item cannot be opened in a Nostr web client"))))

(provide 'nostr-share)
;;; nostr-share.el ends here
