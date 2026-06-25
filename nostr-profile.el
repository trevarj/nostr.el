;;; nostr-profile.el --- Nostr profile buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Profile view backed by the local Nostr cache.

;;; Code:

(require 'cl-lib)
(require 'emacsql)
(require 'json)
(require 'subr-x)
(require 'nostr-actions)
(require 'nostr-db)
(require 'nostr-nip)
(require 'nostr-relay)
(require 'nostr-share)
(require 'nostr-thread)
(require 'nostr-upload)
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

(defvar-local nostr-profile-edit-target-buffer nil
  "Profile buffer refreshed after the current edit buffer publishes.")

(defvar-local nostr-profile-edit-pubkey nil
  "Pubkey being edited by the current profile edit buffer.")

(defvar-local nostr-profile-edit-original nil
  "Original profile alist used to populate the current edit buffer.")

(defvar-local nostr-profile-edit-content-start nil
  "Marker for editable profile form content.")

(defvar-local nostr-profile-edit-dirty nil
  "Whether the current profile edit buffer has unsaved changes.")

(defvar-local nostr-profile-edit--publishing nil
  "Whether the current profile edit buffer is publishing metadata.")

(defvar-local nostr-profile-edit--uploading nil
  "Whether the current profile edit buffer is uploading an avatar.")

(defvar nostr-profile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-profile-refresh)
    (define-key map (kbd "i") #'nostr-profile-open-self)
    (define-key map (kbd "n") #'nostr-ui-next-section)
    (define-key map (kbd "p") #'nostr-ui-prev-section)
    (define-key map (kbd "TAB") #'nostr-ui-toggle-section)
    (define-key map (kbd "RET") #'nostr-profile-open-at-point)
    (define-key map (kbd "e") #'nostr-profile-edit)
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

(defvar nostr-profile-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'nostr-profile-edit-publish)
    (define-key map (kbd "C-c C-a") #'nostr-profile-edit-attach-avatar)
    (define-key map (kbd "C-c C-k") #'nostr-profile-edit-cancel)
    (define-key map (kbd "?") #'nostr-profile-edit-actions)
    map)
  "Keymap for `nostr-profile-edit-mode'.")

(defvar nostr-profile-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-profile-list-refresh)
    (define-key map (kbd "n") #'nostr-ui-next-section)
    (define-key map (kbd "p") #'nostr-ui-prev-section)
    (define-key map (kbd "i") #'nostr-profile-open-self)
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

(define-derived-mode nostr-profile-edit-mode text-mode "Nostr-Profile-Edit"
  "Mode for editing your own Nostr profile metadata."
  (setq-local header-line-format '(:eval (nostr-profile-edit--header-line)))
  (add-hook 'after-change-functions #'nostr-profile-edit--after-change nil t)
  (add-hook 'kill-buffer-query-functions #'nostr-profile-edit--confirm-kill nil t))

(transient-define-prefix nostr-profile-actions ()
  "Actions for the current Nostr profile."
  [["Profile"
    ("i" "My profile" nostr-profile-open-self)
    ("e" "Edit profile" nostr-profile-edit)
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

(transient-define-prefix nostr-profile-edit-actions ()
  "Actions for the current Nostr profile edit buffer."
  [["Edit Profile"
    ("C-c C-c" "Publish" nostr-profile-edit-publish)
    ("C-c C-a" "Upload avatar" nostr-profile-edit-attach-avatar)
    ("C-c C-k" "Cancel" nostr-profile-edit-cancel)]])

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
       (raw-content . nil)
       (updated-at . ,updated-at)))
    (`(,row-pubkey ,name ,display-name ,about ,picture ,nip05 ,lud16 ,content ,updated-at)
     `((pubkey . ,row-pubkey)
       (name . ,(and (stringp name) name))
       (display-name . ,(and (stringp display-name) display-name))
       (about . ,(and (stringp about) about))
       (picture . ,(and (stringp picture) picture))
       (nip05 . ,(and (stringp nip05) nip05))
       (lud16 . ,(and (stringp lud16) lud16))
       (raw-content . ,(and (stringp content) content))
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

(defun nostr-profile--own-profile-p (&optional pubkey)
  "Return non-nil when PUBKEY, or current profile, is the local account."
  (and (boundp 'nostr-current-pubkey)
       nostr-current-pubkey
       (equal (or pubkey nostr-profile-pubkey) nostr-current-pubkey)))

(defun nostr-profile-open-self ()
  "Open the current account's profile."
  (interactive)
  (unless (and (boundp 'nostr-current-pubkey) nostr-current-pubkey)
    (user-error "No current Nostr pubkey is available"))
  (nostr-profile-open nostr-current-pubkey))

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
  ;; Eager avatars for every author appearing in the profile notes.
  (nostr-relay-fetch-profiles-batch
   (mapcar (lambda (event) (alist-get 'pubkey event)) notes))
  (let ((ids (mapcar (lambda (event) (alist-get 'id event)) notes)))
    (nostr-relay-fetch-event-metadata ids)
    (nostr-relay-subscribe-visible-reactions ids)))

(defun nostr-profile-edit--display-action ()
  "Return automatic display action for profile edit buffers."
  (let ((wide (>= (frame-width) 120)))
    `((display-buffer-in-side-window)
      (side . ,(if wide 'right 'bottom))
      (slot . 0)
      ,@(if wide
            '((window-width . 0.38))
          '((window-height . 0.34))))))

(defun nostr-profile-edit--header-line ()
  "Return profile editor header line text."
  (string-join
   (delq nil
         (list "Nostr Profile Edit"
               (when nostr-profile-edit--uploading "uploading avatar")
               (when nostr-profile-edit--publishing "publishing")
               "C-c C-c publish"
               "C-c C-a avatar"
               "C-c C-k cancel"))
   "  |  "))

(defun nostr-profile-edit--after-change (&rest _ignored)
  "Mark the current profile edit buffer dirty after edits."
  (when (and (markerp nostr-profile-edit-content-start)
             (>= (point-max) nostr-profile-edit-content-start))
    (setq nostr-profile-edit-dirty t)))

(defun nostr-profile-edit--confirm-kill ()
  "Ask before killing a dirty profile editor."
  (or (not nostr-profile-edit-dirty)
      (yes-or-no-p "Discard unsaved Nostr profile edits? ")))

(defun nostr-profile-edit--field-value (field)
  "Return single-line FIELD value in the current edit buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^%s:[ \t]*\\(.*\\)$" (regexp-quote field)) nil t)
      (string-trim (match-string-no-properties 1)))))

(defun nostr-profile-edit--about-value ()
  "Return multi-line About value in the current edit buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^About:[ \t]*$" nil t)
      (string-trim (buffer-substring-no-properties (point) (point-max))))))

(defun nostr-profile-edit--set-field (field value)
  "Set single-line FIELD to VALUE in the current edit buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward (format "^%s:[ \t]*\\(.*\\)$" (regexp-quote field)) nil t)
        (user-error "No %s field in profile editor" field))
      (replace-match (or value "") t t nil 1))))

(defun nostr-profile-edit--form-alist ()
  "Return profile edit form values as an alist."
  `((name . ,(nostr-profile-edit--field-value "Name"))
    (display_name . ,(nostr-profile-edit--field-value "Display name"))
    (nip05 . ,(nostr-profile-edit--field-value "NIP-05"))
    (lud16 . ,(nostr-profile-edit--field-value "Lightning"))
    (picture . ,(nostr-profile-edit--field-value "Picture"))
    (about . ,(nostr-profile-edit--about-value))))

(defun nostr-profile-edit--url-p (value)
  "Return non-nil when VALUE is an HTTP(S) URL or blank."
  (or (string-empty-p value)
      (string-match-p "\\`https?://[^[:space:]]+\\'" value)))

(defun nostr-profile-edit--address-p (value)
  "Return non-nil when VALUE is an address-like identifier or blank."
  (or (string-empty-p value)
      (string-match-p "\\`[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+\\'" value)))

(defun nostr-profile-edit--validate (fields)
  "Validate profile edit FIELDS."
  (let ((picture (alist-get 'picture fields))
        (nip05 (alist-get 'nip05 fields))
        (lud16 (alist-get 'lud16 fields)))
    (unless (nostr-profile-edit--url-p picture)
      (user-error "Picture must be blank or an http(s) URL"))
    (unless (nostr-profile-edit--address-p nip05)
      (user-error "NIP-05 must be blank or address-like"))
    (unless (nostr-profile-edit--address-p lud16)
      (user-error "Lightning must be blank or address-like"))))

(defconst nostr-profile-edit--metadata-keys
  '(name username display_name displayName about picture nip05 lud16)
  "Kind-0 metadata keys managed by the profile editor.")

(defun nostr-profile-edit--raw-metadata (profile)
  "Return cached raw metadata alist for PROFILE."
  (or (when-let* ((content (alist-get 'raw-content profile)))
        (ignore-errors
          (json-parse-string content
                             :object-type 'alist
                             :array-type 'list
                             :null-object nil
                             :false-object json-false)))
      (delq nil
            `((name . ,(alist-get 'name profile))
              (display_name . ,(alist-get 'display-name profile))
              (about . ,(alist-get 'about profile))
              (picture . ,(alist-get 'picture profile))
              (nip05 . ,(alist-get 'nip05 profile))
              (lud16 . ,(alist-get 'lud16 profile))))))

(defun nostr-profile-edit--metadata-json (profile fields)
  "Return kind-0 JSON for PROFILE with edited FIELDS merged."
  (nostr-profile-edit--validate fields)
  (let ((metadata (copy-alist (nostr-profile-edit--raw-metadata profile))))
    (dolist (key nostr-profile-edit--metadata-keys)
      (setq metadata (assq-delete-all key metadata)))
    (dolist (entry fields)
      (let ((value (cdr entry)))
        (unless (string-empty-p value)
          (push (cons (car entry) value) metadata))))
    (json-encode (nreverse metadata))))

(defun nostr-profile-edit--insert-form (profile)
  "Insert editable form for PROFILE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "Name: %s\n" (or (alist-get 'name profile) "")))
    (insert (format "Display name: %s\n" (or (alist-get 'display-name profile) "")))
    (insert (format "NIP-05: %s\n" (or (alist-get 'nip05 profile) "")))
    (insert (format "Lightning: %s\n" (or (alist-get 'lud16 profile) "")))
    (insert (format "Picture: %s\n" (or (alist-get 'picture profile) "")))
    (insert "About:\n")
    (insert (or (alist-get 'about profile) ""))
    (setq nostr-profile-edit-content-start (copy-marker (point-min)))))

(defun nostr-profile-edit--image-file-p (file)
  "Return non-nil when FILE has a supported avatar image extension."
  (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|webp\\)\\'" (downcase file)))

(defun nostr-profile-edit-attach-avatar (file)
  "Upload FILE to Blossom and set it as the Picture URL."
  (interactive "fAvatar image: ")
  (unless (nostr-profile-edit--image-file-p file)
    (user-error "Avatar file must be png, jpg, jpeg, gif, or webp"))
  (let ((buffer (current-buffer)))
    (setq nostr-profile-edit--uploading t)
    (force-mode-line-update)
    (nostr-upload-file
     file
     (lambda (_file url)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq nostr-profile-edit--uploading nil)
           (nostr-profile-edit--set-field "Picture" url)
           (setq nostr-profile-edit-dirty t)
           (force-mode-line-update)
           (message "Nostr avatar uploaded"))))
     (lambda (_file message)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq nostr-profile-edit--uploading nil)
           (force-mode-line-update)))
       (message "%s" message)))))

(defun nostr-profile-edit-publish ()
  "Publish the edited profile metadata."
  (interactive)
  (when nostr-profile-edit--uploading
    (user-error "Wait for avatar upload to finish"))
  (when nostr-profile-edit--publishing
    (user-error "Profile publish already in progress"))
  (unless nostr-profile-edit-original
    (user-error "No profile metadata is associated with this editor"))
  (let* ((buffer (current-buffer))
         (target-buffer nostr-profile-edit-target-buffer)
         (json (nostr-profile-edit--metadata-json
                nostr-profile-edit-original
                (nostr-profile-edit--form-alist))))
    (setq nostr-profile-edit--publishing t)
    (force-mode-line-update)
    (nostr-actions-publish-profile
     json
     (lambda (_event)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq nostr-profile-edit-dirty nil
                 nostr-profile-edit--publishing nil))
         (kill-buffer buffer))
       (when (buffer-live-p target-buffer)
         (with-current-buffer target-buffer
           (nostr-profile-refresh))))
     (lambda (_response _stderr)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq nostr-profile-edit--publishing nil)
           (force-mode-line-update)))))))

(defun nostr-profile-edit-cancel (&optional force)
  "Cancel profile editing.
With FORCE, close without prompting."
  (interactive "P")
  (when (or force (nostr-profile-edit--confirm-kill))
    (setq nostr-profile-edit-dirty nil)
    (kill-buffer (current-buffer))
    (message "Nostr profile edit cancelled")))

(defun nostr-profile-edit ()
  "Edit the local account profile."
  (interactive)
  (unless (nostr-profile--own-profile-p)
    (user-error "Only your own profile can be edited"))
  (let* ((pubkey nostr-profile-pubkey)
         (profile (nostr-profile--row-to-alist
                   (nostr-db-select-profile pubkey)
                   pubkey))
         (target-buffer (current-buffer))
         (buffer (generate-new-buffer "*Nostr Profile Edit*")))
    (with-current-buffer buffer
      (nostr-profile-edit-mode)
      (setq-local nostr-profile-edit-pubkey pubkey)
      (setq-local nostr-profile-edit-original profile)
      (setq-local nostr-profile-edit-target-buffer target-buffer)
      (nostr-profile-edit--insert-form profile)
      (setq nostr-profile-edit-dirty nil))
    (select-window (display-buffer buffer (nostr-profile-edit--display-action)))))

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
     (append '("g refresh" "RET open" "i my profile")
             (when (nostr-profile--own-profile-p) '("e edit"))
             '("f follow" "u unfollow" "M mute" "U unmute" "v verify" "w copy" "b browse" "? actions")))
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
     '("g refresh" "RET open profile" "i my profile" "n next" "p prev" "q quit"))
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
