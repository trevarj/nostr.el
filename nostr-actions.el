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
(require 'seq)
(require 'subr-x)
(require 'nostr-backend)
(require 'nostr-db)
(require 'nostr-event)
(require 'nostr-relay)
(require 'transient)

(defvar nostr-current-pubkey)
(declare-function nostr-compose-open "nostr-compose" (&optional reply-to extra-tags))
(declare-function nostr-ui-refresh-note-counts "nostr-ui" (event-id))

(defvar nostr-actions-after-send-hook nil
  "Hook run with the signed event after a public action is sent.")

(defcustom nostr-reaction-choices '("+" "❤️" "😂" "🤙" "👀")
  "Emoji/content choices shown by the Nostr reaction transient."
  :type '(repeat string)
  :group 'nostr)

(defvar nostr-actions--reaction-event nil
  "Event currently targeted by the reaction transient.")

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
         (let ((normalized (nostr-event-normalize event nil)))
           (nostr-db-store-event normalized)
           (when (and (equal (alist-get 'kind normalized) nostr-kind-reaction)
                      (fboundp 'nostr-ui-refresh-note-counts))
             (when-let* ((target (nostr-event-reaction-event-id normalized)))
               (nostr-ui-refresh-note-counts target)))
           (run-hook-with-args 'nostr-actions-after-send-hook normalized)))
       (when after-send
         (funcall after-send event))
       (message "%s" success-message)))
   (lambda (response stderr _status)
     (user-error "Nostr action failed: %s"
                 (nostr-actions--error-message response stderr)))))

(defun nostr-actions-react (event content)
  "Send reaction CONTENT to EVENT."
  (nostr-actions--send
   nostr-kind-reaction
   (nostr-event-build-reaction-tags event (car nostr-relay-urls))
   content
   "Nostr reaction sent"))

(defun nostr-actions-like (event)
  "Send a positive reaction to EVENT."
  (interactive)
  (nostr-actions-react event "+"))

(defun nostr-actions--reaction-choice (index)
  "Return configured reaction at INDEX."
  (or (nth index nostr-reaction-choices) "+"))

(defun nostr-actions--reaction-choice-label (index)
  "Return transient label for configured reaction at INDEX."
  (format "%s" (nostr-actions--reaction-choice index)))

(defun nostr-actions--reaction-choice-label-1 ()
  "Return transient label for the first configured reaction."
  (nostr-actions--reaction-choice-label 0))

(defun nostr-actions--reaction-choice-label-2 ()
  "Return transient label for the second configured reaction."
  (nostr-actions--reaction-choice-label 1))

(defun nostr-actions--reaction-choice-label-3 ()
  "Return transient label for the third configured reaction."
  (nostr-actions--reaction-choice-label 2))

(defun nostr-actions--reaction-choice-label-4 ()
  "Return transient label for the fourth configured reaction."
  (nostr-actions--reaction-choice-label 3))

(defun nostr-actions--reaction-choice-label-5 ()
  "Return transient label for the fifth configured reaction."
  (nostr-actions--reaction-choice-label 4))

(defun nostr-actions--react-choice (index)
  "React to `nostr-actions--reaction-event' with configured INDEX."
  (unless nostr-actions--reaction-event
    (user-error "No note selected"))
  (nostr-actions-react nostr-actions--reaction-event
                       (nostr-actions--reaction-choice index)))

(defun nostr-actions-react-choice-1 ()
  "React with the first configured reaction."
  (interactive)
  (nostr-actions--react-choice 0))

(defun nostr-actions-react-choice-2 ()
  "React with the second configured reaction."
  (interactive)
  (nostr-actions--react-choice 1))

(defun nostr-actions-react-choice-3 ()
  "React with the third configured reaction."
  (interactive)
  (nostr-actions--react-choice 2))

(defun nostr-actions-react-choice-4 ()
  "React with the fourth configured reaction."
  (interactive)
  (nostr-actions--react-choice 3))

(defun nostr-actions-react-choice-5 ()
  "React with the fifth configured reaction."
  (interactive)
  (nostr-actions--react-choice 4))

(defun nostr-actions-react-read ()
  "React with an emoji/content value read from the minibuffer."
  (interactive)
  (unless nostr-actions--reaction-event
    (user-error "No note selected"))
  (nostr-actions-react
   nostr-actions--reaction-event
   (completing-read "Reaction: " nostr-reaction-choices nil nil)))

(transient-define-prefix nostr-actions-reaction-transient ()
  "Choose a reaction for the selected Nostr note."
  [["React"
    ("1" nostr-actions--reaction-choice-label-1 nostr-actions-react-choice-1)
    ("2" nostr-actions--reaction-choice-label-2 nostr-actions-react-choice-2)
    ("3" nostr-actions--reaction-choice-label-3 nostr-actions-react-choice-3)
    ("4" nostr-actions--reaction-choice-label-4 nostr-actions-react-choice-4)
    ("5" nostr-actions--reaction-choice-label-5 nostr-actions-react-choice-5)
    ("e" "Choose emoji" nostr-actions-react-read)]])

(defun nostr-actions-react-menu (event)
  "Open a transient reaction menu for EVENT."
  (interactive)
  (setq nostr-actions--reaction-event event)
  (nostr-actions-reaction-transient))

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

(defun nostr-actions-current-mute-tags (&optional pubkey-to-add pubkey-to-remove)
  "Return current mute list tags with requested pubkey changes.
PUBKEY-TO-ADD is added when non-nil.  PUBKEY-TO-REMOVE is removed when
non-nil."
  (let ((muted (and (boundp 'nostr-current-pubkey)
                    nostr-current-pubkey
                    (nostr-db-select-mutes nostr-current-pubkey)))
        tags)
    (dolist (pubkey muted)
      (unless (equal pubkey pubkey-to-remove)
        (push `("p" ,pubkey) tags)))
    (when (and pubkey-to-add
               (not (cl-find pubkey-to-add tags :key #'cadr :test #'equal)))
      (push `("p" ,pubkey-to-add) tags))
    (nreverse tags)))

(defun nostr-actions-follow (pubkey &optional after-send)
  "Follow PUBKEY by publishing an updated kind 3 contact list."
  (interactive "sFollow pubkey: ")
  (unless (and (boundp 'nostr-current-pubkey) nostr-current-pubkey)
    (user-error "No current Nostr pubkey is available"))
  (nostr-actions--send
   nostr-kind-contacts
   (nostr-actions-current-follow-tags pubkey nil)
   ""
   "Nostr follow list published"
   after-send))

(defun nostr-actions-unfollow (pubkey &optional after-send)
  "Unfollow PUBKEY by publishing an updated kind 3 contact list."
  (interactive "sUnfollow pubkey: ")
  (unless (and (boundp 'nostr-current-pubkey) nostr-current-pubkey)
    (user-error "No current Nostr pubkey is available"))
  (nostr-actions--send
   nostr-kind-contacts
   (nostr-actions-current-follow-tags nil pubkey)
   ""
   "Nostr follow list published"
   after-send))

(defun nostr-actions-mute (pubkey)
  "Mute PUBKEY by publishing an updated NIP-51 kind 10000 mute list."
  (interactive "sMute pubkey: ")
  (unless (and (boundp 'nostr-current-pubkey) nostr-current-pubkey)
    (user-error "No current Nostr pubkey is available"))
  (nostr-actions--send
   nostr-kind-mute-list
   (nostr-actions-current-mute-tags pubkey nil)
   ""
   "Nostr mute list published"))

(defun nostr-actions-unmute (pubkey)
  "Unmute PUBKEY by publishing an updated NIP-51 kind 10000 mute list."
  (interactive "sUnmute pubkey: ")
  (unless (and (boundp 'nostr-current-pubkey) nostr-current-pubkey)
    (user-error "No current Nostr pubkey is available"))
  (nostr-actions--send
   nostr-kind-mute-list
   (nostr-actions-current-mute-tags nil pubkey)
   ""
   "Nostr mute list published"))

(provide 'nostr-actions)
;;; nostr-actions.el ends here
