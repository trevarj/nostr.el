;;; nostr-profile.el --- Nostr profile buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Profile view backed by the local Nostr cache.

;;; Code:

(require 'cl-lib)
(require 'emacsql)
(require 'subr-x)
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

(defvar nostr-profile-list-limit 200
  "Maximum number of cached follow profiles displayed in a profile list buffer.")

(defvar-local nostr-profile-list-owner-pubkey nil
  "Pubkey whose social list is displayed in the current buffer.")

(defvar-local nostr-profile-list-kind nil
  "Social list kind displayed in the current buffer: `followers' or `following'.")

(defvar nostr-profile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-profile-refresh)
    (define-key map (kbd "n") #'nostr-ui-next-section)
    (define-key map (kbd "p") #'nostr-ui-prev-section)
    (define-key map (kbd "TAB") #'nostr-ui-toggle-section)
    (define-key map (kbd "RET") #'nostr-profile-open-at-point)
    (define-key map (kbd "f") #'nostr-profile-follow)
    (define-key map (kbd "u") #'nostr-profile-unfollow)
    (define-key map (kbd "M") #'nostr-profile-mute)
    (define-key map (kbd "U") #'nostr-profile-unmute)
    (define-key map (kbd "v") #'nostr-profile-verify-nip05)
    (define-key map (kbd "m") #'nostr-ui-toggle-note-media)
    (define-key map (kbd "w") #'nostr-share-copy)
    (define-key map (kbd "y") #'nostr-share-copy-raw-id)
    (define-key map (kbd "b") #'nostr-share-browse)
    (define-key map (kbd "?") #'nostr-profile-actions)
    map)
  "Keymap for `nostr-profile-mode'.")

(defvar nostr-profile-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-profile-list-refresh)
    (define-key map (kbd "n") #'nostr-ui-next-section)
    (define-key map (kbd "p") #'nostr-ui-prev-section)
    (define-key map (kbd "RET") #'nostr-profile-list-open-at-point)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `nostr-profile-list-mode'.")

(define-derived-mode nostr-profile-mode special-mode "Nostr-Profile"
  "Mode for Nostr profile buffers."
  (setq-local truncate-lines nil))

(define-derived-mode nostr-profile-list-mode special-mode "Nostr-Profile-List"
  "Mode for cached Nostr profile relationship lists."
  (setq-local truncate-lines nil))

(transient-define-prefix nostr-profile-actions ()
  "Actions for the current Nostr profile."
  [["Profile"
    ("f" "Follow" nostr-profile-follow)
    ("u" "Unfollow" nostr-profile-unfollow)
    ("M" "Mute" nostr-profile-mute)
    ("U" "Unmute" nostr-profile-unmute)
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

(defun nostr-profile--handle (profile)
  "Return PROFILE's local handle, when distinct from its display name."
  (let ((name (alist-get 'name profile))
        (display-name (alist-get 'display-name profile)))
    (when (and (stringp name)
               (not (string-empty-p name))
               (not (equal name display-name)))
      (format "@%s" name))))

(defun nostr-profile--identifier (profile)
  "Return a compact verified identity, npub, or pubkey fallback for PROFILE."
  (let* ((pubkey (alist-get 'pubkey profile))
         (nip05 (alist-get 'nip05 profile)))
    (or (nostr-ui-format-nip05 nip05 pubkey)
        (when-let* ((npub (nostr-ui--cached-npub pubkey)))
          (nostr-ui--shorten-identifier npub))
        (nostr-ui--shorten-identifier pubkey))))

(defun nostr-profile--about-preview (profile)
  "Return a single-line about preview for PROFILE."
  (when-let* ((about (alist-get 'about profile)))
    (let ((text (string-trim (replace-regexp-in-string "[[:space:]\n]+" " " about))))
      (unless (string-empty-p text)
        (truncate-string-to-width text 140 nil nil "...")))))

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

(defun nostr-profile--insert-social-count-button (profile label count kind)
  "Insert clickable social COUNT for PROFILE with LABEL and KIND."
  (insert-text-button
   (number-to-string count)
   'follow-link t
   'help-echo (format "Open cached %s" label)
   'action (lambda (_button)
             (nostr-profile-open-list (alist-get 'pubkey profile) kind))))

(defun nostr-profile--insert-social-lines (profile)
  "Insert social context rows for PROFILE."
  (let* ((pubkey (alist-get 'pubkey profile))
         (current (and (boundp 'nostr-current-pubkey) nostr-current-pubkey))
         (following (nostr-db-following-count pubkey))
         (followers (nostr-db-follower-count pubkey))
         (you-follow (and current (nostr-db-follows-p current pubkey)))
         (you-muted (and current (nostr-db-muted-p current pubkey))))
    (dolist (line `(("You follow" . ,(if you-follow "yes" "no"))
                    ("Muted" . ,(if you-muted "yes" "no"))))
      (insert (propertize (format "%-10s " (car line)) 'face 'nostr-ui-meta))
      (insert (format "%s\n" (cdr line))))
    (insert (propertize (format "%-10s " "Followers") 'face 'nostr-ui-meta))
    (nostr-profile--insert-social-count-button profile "followers" followers 'followers)
    (insert "\n")
    (insert (propertize (format "%-10s " "Following") 'face 'nostr-ui-meta))
    (nostr-profile--insert-social-count-button profile "following" following 'following)
    (insert "\n")
    (insert (propertize (format "%-10s " "Relays") 'face 'nostr-ui-meta))
    (insert (format "%s\n" (nostr-profile--relay-summary pubkey)))))

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
    (nostr-profile--insert-social-lines profile)
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
        (insert (format "%s\n"
                        (if (eq (car field) 'nip05)
                            (nostr-ui-format-nip05 value (alist-get 'pubkey profile))
                          value)))))
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
       "Use f to follow, M to mute, or w to copy the profile id."))
    (nostr-ui-insert-footer
     '("g refresh" "RET open" "f follow" "u unfollow" "M mute" "U unmute" "v verify" "w copy" "b browse" "? actions"))
    (nostr-ui-finish-refresh position-state)))

(defun nostr-profile-open-at-point ()
  "Open the note or profile at point."
  (interactive)
  (unless (nostr-ui-activate-button-at-point)
    (when-let* ((section (nostr-ui-section-at-point)))
      (pcase (nostr-ui-section-type section)
        ('note (nostr-thread-open (nostr-ui-section-data section)))
        ('profile (message "Profile %s" (alist-get 'pubkey (nostr-ui-section-data section))))))))

(defun nostr-profile--refresh-buffer-after-action (buffer)
  "Return callback that refreshes profile BUFFER after an action succeeds."
  (lambda (_event)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'nostr-profile-mode)
          (nostr-profile-refresh))))))

(defun nostr-profile-follow ()
  "Follow the profile shown by this buffer."
  (interactive)
  (unless nostr-profile-pubkey
    (user-error "No profile pubkey is associated with this buffer"))
  (nostr-actions-follow
   nostr-profile-pubkey
   (nostr-profile--refresh-buffer-after-action (current-buffer))))

(defun nostr-profile-unfollow ()
  "Unfollow the profile shown by this buffer."
  (interactive)
  (unless nostr-profile-pubkey
    (user-error "No profile pubkey is associated with this buffer"))
  (nostr-actions-unfollow
   nostr-profile-pubkey
   (nostr-profile--refresh-buffer-after-action (current-buffer))))

(defun nostr-profile-mute ()
  "Mute the profile shown by this buffer."
  (interactive)
  (unless nostr-profile-pubkey
    (user-error "No profile pubkey is associated with this buffer"))
  (nostr-actions-mute nostr-profile-pubkey))

(defun nostr-profile-unmute ()
  "Unmute the profile shown by this buffer."
  (interactive)
  (unless nostr-profile-pubkey
    (user-error "No profile pubkey is associated with this buffer"))
  (nostr-actions-unmute nostr-profile-pubkey))

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
       (when (alist-get 'verified result)
         (nostr-ui-record-nip05-verification nostr-profile-pubkey identifier)
         (when (derived-mode-p 'nostr-profile-mode)
           (nostr-profile-refresh)))
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
    (nostr-relay-fetch-author pubkey nostr-profile-note-limit)
    (nostr-relay-fetch-author-from-urls
     pubkey nostr-profile-note-limit nostr-relay-search-author-urls)
    (pop-to-buffer buffer)))

(defun nostr-profile-list--title ()
  "Return the title for the current profile list."
  (pcase nostr-profile-list-kind
    ('followers "Followers")
    ('following "Following")
    (_ "Profiles")))

(defun nostr-profile-list--select ()
  "Return cached profile rows for the current profile list buffer."
  (pcase nostr-profile-list-kind
    ('followers
     (nostr-db-select-follower-profiles
      nostr-profile-list-owner-pubkey nostr-profile-list-limit))
    ('following
     (nostr-db-select-following-profiles
      nostr-profile-list-owner-pubkey nostr-profile-list-limit))
    (_ nil)))

(defun nostr-profile-list--insert-profile (profile)
  "Insert one PROFILE card in a profile list buffer."
  (let* ((pubkey (alist-get 'pubkey profile))
         (name (nostr-profile--display-name profile))
         (handle (nostr-profile--handle profile))
         (identifier (nostr-profile--identifier profile))
         (about (nostr-profile--about-preview profile))
         (followers (nostr-db-follower-count pubkey))
         (following (nostr-db-following-count pubkey)))
    (nostr-ui-with-section 'profile pubkey profile
        (lambda (section)
          (insert (propertize "▾" 'nostr-ui-section section))
          (insert " ")
          (nostr-ui-insert-avatar (alist-get 'picture profile) nostr-ui-avatar-size)
          (insert " ")
          (insert (propertize name
                              'face 'nostr-ui-author
                              'nostr-ui-section section))
          (when handle
            (insert (propertize (format "  %s" handle)
                                'face 'nostr-ui-meta
                                'nostr-ui-section section)))
          (insert "\n"))
      (insert "  ")
      (insert (propertize identifier 'face 'nostr-ui-meta))
      (insert "\n")
      (nostr-ui-insert-badge-line
       (list (format "%d follower%s"
                     followers
                     (if (= followers 1) "" "s"))
             (format "%d following" following)
             (format "relays %s" (nostr-profile--relay-summary pubkey)))
       "  ")
      (when about
        (insert "  ")
        (insert (propertize about 'face 'nostr-ui-content))
        (insert "\n"))
      (insert "\n"))))

(defun nostr-profile-list-refresh ()
  "Refresh the current profile relationship list buffer."
  (interactive)
  (unless (and nostr-profile-list-owner-pubkey nostr-profile-list-kind)
    (user-error "No profile list is associated with this buffer"))
  (let* ((inhibit-read-only t)
         (position-state (nostr-ui-capture-position))
         (owner (nostr-profile--row-to-alist
                 (nostr-db-select-profile nostr-profile-list-owner-pubkey)
                 nostr-profile-list-owner-pubkey))
         (profiles (mapcar (lambda (row)
                             (nostr-profile--row-to-alist row (car row)))
                           (nostr-profile-list--select))))
    (dolist (profile profiles)
      (nostr-relay-fetch-profile (alist-get 'pubkey profile)))
    (nostr-ui-clear)
    (nostr-ui-insert-status-header
     (nostr-profile-list--title)
     (nostr-profile--display-name owner)
     (format "%d cached profile%s"
             (length profiles)
             (if (= (length profiles) 1) "" "s")))
    (if profiles
        (dolist (profile profiles)
          (nostr-profile-list--insert-profile profile))
      (nostr-ui-insert-empty-state
       "No cached profiles for this list."
       "Refresh relays or open more profiles to grow the local cache."))
    (nostr-ui-insert-footer
     '("g refresh" "RET open profile" "n next" "p prev" "q quit"))
    (nostr-ui-finish-refresh position-state)))

(defun nostr-profile-list-open-at-point ()
  "Open the selected profile list row."
  (interactive)
  (when-let* ((section (nostr-ui-section-at-point))
              ((eq (nostr-ui-section-type section) 'profile))
              (pubkey (alist-get 'pubkey (nostr-ui-section-data section))))
    (nostr-profile-open pubkey)))

(defun nostr-profile-open-list (pubkey kind)
  "Open cached social list KIND for profile PUBKEY."
  (let* ((title (pcase kind
                  ('followers "Followers")
                  ('following "Following")
                  (_ "Profiles")))
         (buffer (get-buffer-create
                  (format "*Nostr %s %s*"
                          title
                          (truncate-string-to-width pubkey 12)))))
    (with-current-buffer buffer
      (nostr-profile-list-mode)
      (setq-local nostr-profile-list-owner-pubkey pubkey)
      (setq-local nostr-profile-list-kind kind)
      (nostr-profile-list-refresh))
    (pop-to-buffer buffer)))

(provide 'nostr-profile)
;;; nostr-profile.el ends here
