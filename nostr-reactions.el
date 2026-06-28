;;; nostr-reactions.el --- Nostr reaction detail buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Popup buffer for inspecting the accounts that reacted to a note.

;;; Code:

(require 'subr-x)
(require 'nostr-db)
(require 'nostr-relay)
(require 'nostr-ui)
(require 'transient)

(declare-function nostr-profile-open "nostr-profile" (pubkey))
(declare-function nostr-profile-open-self "nostr-profile" ())

(defcustom nostr-reactions-limit 200
  "Maximum number of cached reactions shown for one note."
  :type 'integer
  :group 'nostr)

(defcustom nostr-reactions-display-buffer-action
  '((display-buffer-in-side-window)
    (side . bottom)
    (slot . 1)
    (window-height . 0.35))
  "Display action used for reaction detail buffers."
  :type 'sexp
  :group 'nostr)

(defvar-local nostr-reactions-event nil
  "Event whose reactions are displayed in the current buffer.")

(defvar nostr-reactions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-reactions-refresh)
    (define-key map (kbd "n") #'nostr-ui-next-section)
    (define-key map (kbd "p") #'nostr-ui-prev-section)
    (define-key map (kbd "TAB") #'nostr-ui-toggle-section)
    (define-key map (kbd "i") #'nostr-profile-open-self)
    (define-key map (kbd "RET") #'nostr-reactions-open-profile)
    (define-key map (kbd "q") #'nostr-reactions-quit)
    (define-key map (kbd "?") #'nostr-reactions-actions)
    map)
  "Keymap for `nostr-reactions-mode'.")

(define-derived-mode nostr-reactions-mode special-mode "Nostr-Reactions"
  "Mode for Nostr reaction detail buffers."
  (setq-local truncate-lines nil))

(transient-define-prefix nostr-reactions-actions ()
  "Actions for cached reaction detail buffers."
  [["Reaction"
    ("RET" "Open profile" nostr-reactions-open-profile)
    ("g" "Refresh" nostr-reactions-refresh)
    ("i" "My profile" nostr-profile-open-self)
    ("q" "Quit" nostr-reactions-quit)]
   ["Navigation"
    ("n" "Next reactor" nostr-ui-next-section)
    ("p" "Previous reactor" nostr-ui-prev-section)
    ("TAB" "Toggle reactor" nostr-ui-toggle-section)]])

(defun nostr-reactions--display-name (reaction)
  "Return display name for REACTION's reactor."
  (or (alist-get 'display-name reaction)
      (alist-get 'name reaction)
      (alist-get 'pubkey reaction)))

(defun nostr-reactions--handle (reaction)
  "Return local handle for REACTION's reactor when useful."
  (let ((name (alist-get 'name reaction))
        (display-name (alist-get 'display-name reaction)))
    (when (and (stringp name)
               (not (string-empty-p name))
               (not (equal name display-name)))
      (format "@%s" name))))

(defun nostr-reactions--identifier (reaction)
  "Return compact identity for REACTION's reactor."
  (let* ((pubkey (alist-get 'pubkey reaction))
         (nip05 (alist-get 'nip05 reaction)))
    (or (nostr-ui-format-nip05 nip05 pubkey)
        (when-let* ((npub (nostr-ui--cached-npub pubkey)))
          (nostr-ui--shorten-identifier npub))
        (nostr-ui--shorten-identifier pubkey))))

(defun nostr-reactions--display-content (content)
  "Return compact display text for reaction CONTENT."
  (if (equal content "+") "♥" (or content "")))

(defun nostr-reactions--row-to-alist (row)
  "Convert reaction detail ROW to an alist."
  (pcase row
    (`(,id ,event-id ,pubkey ,content ,created-at
           ,name ,display-name ,about ,picture ,nip05 ,lud16 ,updated-at)
     `((id . ,id)
       (event-id . ,event-id)
       (pubkey . ,pubkey)
       (content . ,(and (stringp content) content))
       (created-at . ,created-at)
       (name . ,(and (stringp name) name))
       (display-name . ,(and (stringp display-name) display-name))
       (about . ,(and (stringp about) about))
       (picture . ,(and (stringp picture) picture))
       (nip05 . ,(and (stringp nip05) nip05))
       (lud16 . ,(and (stringp lud16) lud16))
       (updated-at . ,updated-at)))))

(defun nostr-reactions--insert-reaction (reaction)
  "Insert one REACTION profile row."
  (let* ((pubkey (alist-get 'pubkey reaction))
         (name (nostr-reactions--display-name reaction))
         (handle (nostr-reactions--handle reaction))
         (identifier (nostr-reactions--identifier reaction))
         (content (nostr-reactions--display-content (alist-get 'content reaction)))
         (created-at (alist-get 'created-at reaction)))
    (nostr-ui-with-section 'profile pubkey reaction
        (lambda (section)
          (insert (propertize content
                              'face 'nostr-ui-meta
                              'nostr-ui-section section))
          (insert "  ")
          (nostr-ui-insert-avatar (alist-get 'picture reaction) nostr-ui-avatar-size)
          (insert " ")
          (insert (propertize name
                              'face 'nostr-ui-author
                              'nostr-ui-section section))
          (when handle
            (insert (propertize (format "  %s" handle)
                                'face 'nostr-ui-meta
                                'nostr-ui-section section)))
          (when created-at
            (insert (propertize (format "  %s"
                                        (nostr-ui-format-time created-at 'feed))
                                'face 'nostr-ui-meta
                                'nostr-ui-section section)))
          (insert "\n"))
      (insert "     ")
      (insert (propertize identifier 'face 'nostr-ui-meta))
      (insert "\n\n"))))

(defun nostr-reactions-refresh ()
  "Refresh the current reaction detail buffer."
  (interactive)
  (unless nostr-reactions-event
    (user-error "No note is associated with this reaction buffer"))
  (let* ((event-id (alist-get 'id nostr-reactions-event))
         (inhibit-read-only t)
         (position-state (nostr-ui-capture-position))
         (reactions (mapcar #'nostr-reactions--row-to-alist
                            (nostr-db-select-reactions-for-event
                             event-id nostr-reactions-limit))))
    (dolist (reaction reactions)
      (nostr-relay-fetch-profile (alist-get 'pubkey reaction)))
    (nostr-ui-clear)
    (nostr-ui-insert-status-header
     "Reactions"
     nil
     (format "%d cached reaction%s for %s"
             (length reactions)
             (if (= (length reactions) 1) "" "s")
             (nostr-ui--shorten-identifier event-id)))
    (if reactions
        (dolist (reaction reactions)
          (nostr-reactions--insert-reaction reaction))
      (nostr-ui-insert-empty-state
       "No cached reactions for this note."
       "Keep the note visible while relay sync backfills reaction events."))
    (nostr-ui-insert-footer
     '("g refresh" "RET open profile" "n next" "p prev" "TAB toggle" "q quit" "? actions"))
    (nostr-ui-finish-refresh position-state)))

(defun nostr-reactions--selected-profile-pubkey ()
  "Return the selected reactor pubkey or signal a user error."
  (let ((section (nostr-ui-section-at-point)))
    (unless section
      (user-error "No reactor selected"))
    (unless (eq (nostr-ui-section-type section) 'profile)
      (user-error "Selected item is not a reactor profile"))
    (or (alist-get 'pubkey (nostr-ui-section-data section))
        (user-error "Selected reactor has no pubkey"))))

(defun nostr-reactions-open-profile ()
  "Open the selected reaction actor profile."
  (interactive)
  (nostr-profile-open (nostr-reactions--selected-profile-pubkey)))

(defun nostr-reactions-quit ()
  "Quit the reaction popup and kill its temporary buffer."
  (interactive)
  (quit-window t))

(defun nostr-reactions-open (event)
  "Open a popup listing cached reactions to EVENT."
  (interactive)
  (unless (alist-get 'id event)
    (user-error "Selected note has no event id"))
  (let ((buffer (get-buffer-create
                 (format " *Nostr Reactions %s*"
                         (nostr-ui--shorten-identifier (alist-get 'id event))))))
    (with-current-buffer buffer
      (nostr-reactions-mode)
      (setq-local nostr-reactions-event event)
      (nostr-reactions-refresh))
    (select-window
     (display-buffer buffer nostr-reactions-display-buffer-action))))

(provide 'nostr-reactions)
;;; nostr-reactions.el ends here
