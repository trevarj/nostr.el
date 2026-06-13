;;; nostr-actions.el --- Public Nostr actions -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Signed public-social actions shared by UI buffers.

;;; Code:

(require 'cl-lib)
(require 'emacsql)
(require 'json)
(require 'subr-x)
(require 'nostr-backend)
(require 'nostr-db)
(require 'nostr-event)
(require 'nostr-relay)

(defvar nostr-current-pubkey)
(declare-function nostr-compose-open "nostr-compose" (&optional reply-to extra-tags))

(defvar nostr-actions-after-send-hook nil
  "Hook run with the signed event after a public action is sent.")

(defun nostr-actions--error-message (response stderr)
  "Return a useful error string from backend RESPONSE and STDERR."
  (let-alist (alist-get 'error response)
    (string-trim
     (string-join
      (delq nil (list .message
                      (unless (string-empty-p (or stderr "")) stderr)))
      ": "))))

(defun nostr-actions--send (kind tags content success-message &optional after-send)
  "Sign KIND event with TAGS and CONTENT, then broadcast it."
  (nostr-backend-sign-event
   kind tags content
   (lambda (response)
     (let ((event (alist-get 'event response))
           (client-message (alist-get 'client_message response)))
       (when client-message
         (nostr-relay-send-client-message client-message))
       (when event
         (nostr-db-store-event (nostr-event-normalize event nil))
         (run-hook-with-args 'nostr-actions-after-send-hook event))
       (when after-send
         (funcall after-send event))
       (message "%s" success-message)))
   (lambda (response stderr _status)
     (user-error "Nostr action failed: %s"
                 (nostr-actions--error-message response stderr)))))

(defun nostr-actions-like (event)
  "Send a positive reaction to EVENT."
  (interactive)
  (nostr-actions--send
   nostr-kind-reaction
   (nostr-event-build-reaction-tags event (car nostr-relay-urls))
   "+"
   "Nostr reaction sent"))

(defun nostr-actions-repost (event)
  "Repost EVENT using NIP-18."
  (interactive)
  (nostr-actions--send
   nostr-kind-repost
   (nostr-event-build-repost-tags event (car nostr-relay-urls))
   (json-encode event)
   "Nostr repost sent"))

(defun nostr-actions-quote (event)
  "Open compose buffer for a quote post of EVENT."
  (interactive)
  (require 'nostr-compose)
  (nostr-compose-open nil `(("q" ,(alist-get 'id event)
                             ,(or (alist-get 'relay event) (car nostr-relay-urls))))))

(defun nostr-actions-current-follow-tags (&optional pubkey-to-add pubkey-to-remove)
  "Return current contact list tags with requested pubkey changes.
PUBKEY-TO-ADD is added when non-nil.  PUBKEY-TO-REMOVE is removed when
non-nil."
  (let ((contacts (and (boundp 'nostr-current-pubkey)
                       nostr-current-pubkey
                       (emacsql nostr-db--connection
                                [:select [contact relay petname]
                                         :from follows
                                         :where (= pubkey $s1)]
                                nostr-current-pubkey)))
        tags)
    (dolist (row contacts)
      (pcase-let ((`(,contact ,relay ,petname) row))
        (unless (equal contact pubkey-to-remove)
          (push (delq nil `("p" ,contact ,relay ,petname)) tags))))
    (when (and pubkey-to-add
               (not (cl-find pubkey-to-add tags :key #'cadr :test #'equal)))
      (push `("p" ,pubkey-to-add) tags))
    (nreverse tags)))

(defun nostr-actions-follow (pubkey)
  "Follow PUBKEY by publishing an updated kind 3 contact list."
  (interactive "sFollow pubkey: ")
  (unless (and (boundp 'nostr-current-pubkey) nostr-current-pubkey)
    (user-error "No current Nostr pubkey is available"))
  (nostr-actions--send
   nostr-kind-contacts
   (nostr-actions-current-follow-tags pubkey nil)
   ""
   "Nostr follow list published"))

(defun nostr-actions-unfollow (pubkey)
  "Unfollow PUBKEY by publishing an updated kind 3 contact list."
  (interactive "sUnfollow pubkey: ")
  (unless (and (boundp 'nostr-current-pubkey) nostr-current-pubkey)
    (user-error "No current Nostr pubkey is available"))
  (nostr-actions--send
   nostr-kind-contacts
   (nostr-actions-current-follow-tags nil pubkey)
   ""
   "Nostr follow list published"))

(provide 'nostr-actions)
;;; nostr-actions.el ends here
