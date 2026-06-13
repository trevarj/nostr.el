;;; nostr-profile.el --- Nostr profile buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Profile view backed by the local Nostr cache.

;;; Code:

(require 'cl-lib)
(require 'emacsql)
(require 'nostr-actions)
(require 'nostr-db)
(require 'nostr-nip)
(require 'nostr-relay)
(require 'nostr-share)
(require 'nostr-thread)
(require 'nostr-ui)
(require 'transient)

(defvar-local nostr-profile-pubkey nil
  "Pubkey displayed by the current profile buffer.")

(defvar nostr-current-pubkey)

(defvar nostr-profile-note-limit 50
  "Maximum number of recent notes displayed in a profile buffer.")

(defvar nostr-profile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-profile-refresh)
    (define-key map (kbd "n") #'nostr-ui-next-section)
    (define-key map (kbd "p") #'nostr-ui-prev-section)
    (define-key map (kbd "TAB") #'nostr-ui-toggle-section)
    (define-key map (kbd "RET") #'nostr-profile-open-at-point)
    (define-key map (kbd "f") #'nostr-profile-follow)
    (define-key map (kbd "u") #'nostr-profile-unfollow)
    (define-key map (kbd "v") #'nostr-profile-verify-nip05)
    (define-key map (kbd "m") #'nostr-ui-toggle-note-media)
    (define-key map (kbd "w") #'nostr-share-copy)
    (define-key map (kbd "y") #'nostr-share-copy-raw-id)
    (define-key map (kbd "b") #'nostr-share-browse)
    (define-key map (kbd "?") #'nostr-profile-actions)
    map)
  "Keymap for `nostr-profile-mode'.")

(define-derived-mode nostr-profile-mode special-mode "Nostr-Profile"
  "Mode for Nostr profile buffers."
  (setq-local truncate-lines nil))

(transient-define-prefix nostr-profile-actions ()
  "Actions for the current Nostr profile."
  [["Profile"
    ("f" "Follow" nostr-profile-follow)
    ("u" "Unfollow" nostr-profile-unfollow)
    ("v" "Verify NIP-05" nostr-profile-verify-nip05)]
   ["Notes"
    ("RET" "Open selected" nostr-profile-open-at-point)
    ("m" "Toggle media" nostr-ui-toggle-note-media)
    ("g" "Refresh" nostr-profile-refresh)
    ("n" "Next section" nostr-ui-next-section)
    ("p" "Previous section" nostr-ui-prev-section)
    ("TAB" "Toggle section" nostr-ui-toggle-section)]
   ["Share"
    ("w" "Copy NIP-19" nostr-share-copy)
    ("y" "Copy raw id" nostr-share-copy-raw-id)
    ("b" "Browse" nostr-share-browse)]])

(defun nostr-profile--row-to-alist (row pubkey)
  "Convert profile ROW for PUBKEY to an alist."
  (pcase row
    (`(,row-pubkey ,name ,display-name ,about ,picture ,nip05 ,lud16 ,updated-at)
     `((pubkey . ,row-pubkey)
       (name . ,(and (stringp name) name))
       (display-name . ,(and (stringp display-name) display-name))
       (about . ,(and (stringp about) about))
       (picture . ,(and (stringp picture) picture))
       (nip05 . ,(and (stringp nip05) nip05))
       (lud16 . ,(and (stringp lud16) lud16))
       (updated-at . ,updated-at)))
    (_ `((pubkey . ,pubkey)))))

(defun nostr-profile--display-name (profile)
  "Return the best display name for PROFILE."
  (or (alist-get 'display-name profile)
      (alist-get 'name profile)
      (alist-get 'pubkey profile)))

(defun nostr-profile--relay-summary (pubkey)
  "Return compact relay preference summary for PUBKEY."
  (let ((read 0)
        (write 0)
        (both 0))
    (dolist (relay (nostr-db-select-relay-list pubkey))
      (let ((can-read (alist-get 'read relay))
            (can-write (alist-get 'write relay)))
        (cond
         ((and can-read can-write) (setq both (1+ both)))
         (can-read (setq read (1+ read)))
         (can-write (setq write (1+ write))))))
    (if (zerop (+ read write both))
        "none cached"
      (format "%d read  %d write  %d both" read write both))))

(defun nostr-profile--social-lines (profile)
  "Return social context lines for PROFILE."
  (let* ((pubkey (alist-get 'pubkey profile))
         (current (and (boundp 'nostr-current-pubkey) nostr-current-pubkey))
         (following (nostr-db-following-count pubkey))
         (followers (nostr-db-follower-count pubkey))
         (you-follow (and current (nostr-db-follows-p current pubkey))))
    `(("You follow" . ,(if you-follow "yes" "no"))
      ("Followers" . ,(number-to-string followers))
      ("Following" . ,(number-to-string following))
      ("Relays" . ,(nostr-profile--relay-summary pubkey)))))

(defun nostr-profile--select-notes (pubkey &optional limit)
  "Return recent note events authored by PUBKEY."
  (mapcar #'nostr-db--event-row-to-alist
          (emacsql nostr-db--connection
                   [:select
                    [events:id events:pubkey events:created_at events:kind events:tags
                               events:content events:sig events:relay events:root_id events:reply_id
                               events:quote_id profiles:name profiles:display_name profiles:picture]
                    :from events
                    :left-join profiles :on (= events:pubkey profiles:pubkey)
                    :where (and (= events:kind 1)
                                (= events:pubkey $s1))
                    :order-by [(desc events:created_at)]
                    :limit $s2]
                   pubkey (or limit nostr-profile-note-limit))))

(defun nostr-profile--backfill-visible-metadata (notes)
  "Request relay metadata for visible profile NOTES."
  (nostr-relay-fetch-event-metadata
   (mapcar (lambda (event) (alist-get 'id event)) notes)))

(defun nostr-profile--insert-header (profile)
  "Insert PROFILE metadata section."
  (nostr-ui-with-section 'profile (alist-get 'pubkey profile) profile
      (format "Profile: %s" (nostr-profile--display-name profile))
    (nostr-ui-insert-avatar
     (alist-get 'picture profile)
     nostr-ui-profile-avatar-size)
    (insert "\n\n")
    (dolist (line (nostr-profile--social-lines profile))
      (insert (propertize (format "%-10s " (car line)) 'face 'nostr-ui-meta))
      (insert (format "%s\n" (cdr line))))
    (insert "\n")
    (dolist (field '((pubkey . "Pubkey")
                     (name . "Name")
                     (display-name . "Display")
                     (nip05 . "NIP-05")
                     (lud16 . "Lightning")
                     (picture . "Picture")
                     (about . "About")))
      (when-let* ((value (alist-get (car field) profile)))
        (insert (propertize (format "%-10s " (cdr field)) 'face 'nostr-ui-meta))
        (insert (format "%s\n" value))))
    (insert "\n")))

(defun nostr-profile-refresh ()
  "Refresh the current profile buffer."
  (interactive)
  (unless nostr-profile-pubkey
    (user-error "No profile pubkey is associated with this buffer"))
  (let* ((inhibit-read-only t)
         (position-state (nostr-ui-capture-position))
         (profile (nostr-profile--row-to-alist
                   (nostr-db-select-profile nostr-profile-pubkey)
                   nostr-profile-pubkey))
         (notes (nostr-profile--select-notes nostr-profile-pubkey)))
    (nostr-profile--backfill-visible-metadata notes)
    (nostr-ui-clear)
    (nostr-ui-insert-status-header
     "Profile"
     (nostr-profile--display-name profile)
     (format "%d recent cached note%s"
             (length notes)
             (if (= (length notes) 1) "" "s")))
    (nostr-profile--insert-header profile)
    (insert (propertize "Recent notes\n\n" 'face 'nostr-ui-section-heading))
    (if notes
        (dolist (event notes)
          (nostr-ui-insert-note event))
      (nostr-ui-insert-empty-state
       "No cached notes for this profile."
       "Use f to follow, u to unfollow, or w to copy the profile id."))
    (nostr-ui-insert-footer
     '("g refresh" "RET open" "f follow" "u unfollow" "v verify" "w copy" "b browse" "? actions"))
    (nostr-ui-finish-refresh position-state)))

(defun nostr-profile-open-at-point ()
  "Open the note or profile at point."
  (interactive)
  (when-let* ((section (nostr-ui-section-at-point)))
    (pcase (nostr-ui-section-type section)
      ('note (nostr-thread-open (nostr-ui-section-data section)))
      ('profile (message "Profile %s" (alist-get 'pubkey (nostr-ui-section-data section)))))))

(defun nostr-profile-follow ()
  "Follow the profile shown by this buffer."
  (interactive)
  (unless nostr-profile-pubkey
    (user-error "No profile pubkey is associated with this buffer"))
  (nostr-actions-follow nostr-profile-pubkey))

(defun nostr-profile-unfollow ()
  "Unfollow the profile shown by this buffer."
  (interactive)
  (unless nostr-profile-pubkey
    (user-error "No profile pubkey is associated with this buffer"))
  (nostr-actions-unfollow nostr-profile-pubkey))

(defun nostr-profile-verify-nip05 ()
  "Verify this profile's cached NIP-05 identifier."
  (interactive)
  (unless nostr-profile-pubkey
    (user-error "No profile pubkey is associated with this buffer"))
  (let* ((profile (nostr-profile--row-to-alist
                   (nostr-db-select-profile nostr-profile-pubkey)
                   nostr-profile-pubkey))
         (identifier (alist-get 'nip05 profile)))
    (unless identifier
      (user-error "Profile has no cached NIP-05 identifier"))
    (nostr-nip05-verify
     identifier nostr-profile-pubkey
     (lambda (result)
       (message "NIP-05 %s: %s"
                (if (alist-get 'verified result) "verified" "mismatch")
                identifier))
     (lambda (message)
       (message "NIP-05 verification failed: %s" message)))))

;;;###autoload
(defun nostr-profile-open (pubkey)
  "Open a profile buffer for PUBKEY."
  (interactive "sNostr pubkey: ")
  (let ((buffer (get-buffer-create
                 (format "*Nostr Profile %s*" (truncate-string-to-width pubkey 12)))))
    (with-current-buffer buffer
      (nostr-profile-mode)
      (setq-local nostr-profile-pubkey pubkey)
      (nostr-profile-refresh))
    (pop-to-buffer buffer)))

(provide 'nostr-profile)
;;; nostr-profile.el ends here
