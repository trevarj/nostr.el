;;; nostr-ui.el --- Custom Nostr UI primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Lightweight section rendering without magit-section.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'nostr-db)
(require 'nostr-event)
(require 'nostr-media)
(require 'nostr-nip)

(defface nostr-ui-author
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for note authors."
  :group 'nostr)

(defface nostr-ui-meta
  '((t :inherit shadow))
  "Face for note metadata."
  :group 'nostr)

(defface nostr-ui-content
  '((t :inherit default))
  "Face for note content."
  :group 'nostr)

(defface nostr-ui-card
  '((t :inherit default))
  "Face for note card bodies."
  :group 'nostr)

(defface nostr-ui-card-border
  '((t :inherit shadow))
  "Face for subtle note card separators."
  :group 'nostr)

(defface nostr-ui-badge
  '((t :inherit shadow :box (:line-width (1 . -1) :style released-button)))
  "Face for compact metadata badges."
  :group 'nostr)

(defface nostr-ui-status-title
  '((t :inherit font-lock-keyword-face :weight bold :height 1.15))
  "Face for page status titles."
  :group 'nostr)

(defface nostr-ui-status-detail
  '((t :inherit shadow))
  "Face for page status detail text."
  :group 'nostr)

(defface nostr-ui-avatar-placeholder
  '((t :inherit button))
  "Face for unloaded avatar placeholders."
  :group 'nostr)

(defface nostr-ui-section-heading
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section headings."
  :group 'nostr)

(defface nostr-ui-selected-section
  '((t :inherit highlight :extend t))
  "Face for the currently selected section."
  :group 'nostr)

(defface nostr-ui-nav-active
  '((t :inherit mode-line :weight bold))
  "Face for the active primary navigation item."
  :group 'nostr)

(defface nostr-ui-nav-inactive
  '((t :inherit button))
  "Face for inactive primary navigation items."
  :group 'nostr)

(cl-defstruct nostr-ui-section
  id type start content-start end overlay data folded parent)

(defvar-local nostr-ui--sections nil
  "Sections rendered in the current buffer.")

(defvar-local nostr-ui--current-parent nil
  "Parent section currently being rendered.")

(defvar-local nostr-ui--event-counts-cache nil
  "Buffer-local memoization of `nostr-db-event-counts' keyed by event id.")

(defvar-local nostr-ui--selection-overlay nil
  "Overlay highlighting the current section heading.")

(defvar nostr-ui--npub-cache (make-hash-table :test #'equal)
  "Cache of hex pubkeys to encoded npub strings.")

(defcustom nostr-ui-show-avatars t
  "Whether note cards and profile headers show profile picture placeholders."
  :type 'boolean
  :group 'nostr)

(defcustom nostr-ui-avatar-size 32
  "Maximum pixel size for small avatars in note cards."
  :type 'integer
  :group 'nostr)

(defcustom nostr-ui-avatar-ascent 82
  "Image ascent percentage for inline avatars.
Values above 50 move the rendered avatar upward relative to the text baseline."
  :type 'integer
  :group 'nostr)

(defcustom nostr-ui-profile-avatar-size 72
  "Maximum pixel size for avatars in profile headers."
  :type 'integer
  :group 'nostr)

(defcustom nostr-ui-card-fill-column 88
  "Preferred fill column for note card body text."
  :type 'integer
  :group 'nostr)

(defcustom nostr-ui-default-avatar-file
  (expand-file-name "assets/default-avatar-ostrich.png"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Default avatar image used when a profile has no picture URL."
  :type 'file
  :group 'nostr)

(defconst nostr-ui-primary-nav-items
  '((feed "f" "Feed" nostr-timeline-feed)
    (conversations "C" "Conversations" nostr-timeline-conversations)
    (global "G" "Global" nostr-timeline-global)
    (my-posts "P" "My Posts" nostr-timeline-my-posts)
    (notifications "N" "Notifications" nostr-notifications-open))
  "Primary Nostr destinations as KIND, key, label, and command.")

(defun nostr-ui-clear ()
  "Clear current buffer UI state."
  (setq nostr-ui--sections nil
        nostr-ui--current-parent nil
        nostr-ui--event-counts-cache nil)
  (remove-overlays (point-min) (point-max) 'nostr-ui-section t)
  (when (overlayp nostr-ui--selection-overlay)
    (delete-overlay nostr-ui--selection-overlay))
  (setq nostr-ui--selection-overlay nil)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defmacro nostr-ui-with-section (type id data title &rest body)
  "Insert a custom section of TYPE, ID, DATA and TITLE around BODY.
TITLE may be a string or a function called with the new section."
  (declare (indent 4))
  `(let* ((start (point))
          (parent nostr-ui--current-parent)
          (section (make-nostr-ui-section :id ,id :type ,type :start start
                                          :data ,data :parent parent)))
     (if (functionp ,title)
         (funcall ,title section)
       (insert-text-button
        (format "▾ %s\n" ,title)
        'follow-link t
        'nostr-ui-section section
        'action (lambda (_button) (nostr-ui-toggle-section))))
     (setf (nostr-ui-section-content-start section) (point))
     (let ((nostr-ui--current-parent section))
       ,@body)
     (setf (nostr-ui-section-end section) (point))
     (let ((ov (make-overlay (nostr-ui-section-content-start section)
                             (nostr-ui-section-end section))))
       (overlay-put ov 'nostr-ui-section t)
       (setf (nostr-ui-section-overlay section) ov))
     (push section nostr-ui--sections)
     section))

(defun nostr-ui--sections-by-start ()
  "Return `nostr-ui--sections' as a fresh list sorted by ascending start."
  (sort (copy-sequence nostr-ui--sections)
        (lambda (a b) (< (nostr-ui-section-start a)
                         (nostr-ui-section-start b)))))

(defun nostr-ui-section-at-point ()
  "Return the nearest Nostr UI section at point."
  (or (get-text-property (point) 'nostr-ui-section)
      (cl-find-if
       (lambda (section)
         (and (>= (point) (nostr-ui-section-start section))
              (< (point) (nostr-ui-section-end section))))
       nostr-ui--sections)
      (let ((pos (line-end-position)))
        (cl-find-if
         (lambda (section)
           (= (nostr-ui-section-start section) (1+ pos)))
         nostr-ui--sections))))

(defun nostr-ui-selected-data ()
  "Return data for the section at point."
  (when-let* ((section (nostr-ui-section-at-point)))
    (nostr-ui-section-data section)))

(defun nostr-ui-update-selection ()
  "Highlight the section at point."
  (interactive)
  (when (overlayp nostr-ui--selection-overlay)
    (delete-overlay nostr-ui--selection-overlay)
    (setq nostr-ui--selection-overlay nil))
  (when-let* ((section (nostr-ui-section-at-point)))
    (setq nostr-ui--selection-overlay
          (make-overlay (nostr-ui-section-start section)
                        (max (nostr-ui-section-start section)
                             (1- (nostr-ui-section-end section)))))
    (overlay-put nostr-ui--selection-overlay 'face 'nostr-ui-selected-section)
    (overlay-put nostr-ui--selection-overlay 'priority 10)
    (overlay-put nostr-ui--selection-overlay 'nostr-ui-selection t)))

(defun nostr-ui-toggle-section ()
  "Toggle visibility of the section at point."
  (interactive)
  (when-let* ((section (nostr-ui-section-at-point))
              (overlay (nostr-ui-section-overlay section)))
    (setf (nostr-ui-section-folded section)
          (not (nostr-ui-section-folded section)))
    (overlay-put overlay 'invisible (nostr-ui-section-folded section))
    (save-excursion
      (goto-char (nostr-ui-section-start section))
      ;; The fold glyph is not necessarily the first character at
      ;; section-start: note headings capture `start' before inserting
      ;; indentation, so search the heading line for the actual glyph.
      (when (re-search-forward "[▾▸]" (line-end-position) t)
        (let ((inhibit-read-only t))
          (delete-char -1)
          (insert (if (nostr-ui-section-folded section) "▸" "▾")))))
    (nostr-ui-update-selection)))

(defun nostr-ui-next-section ()
  "Move point to the next section."
  (interactive)
  (let ((pos (point)))
    (when-let* ((next (cl-find-if
                       (lambda (section) (> (nostr-ui-section-start section) pos))
                       (nostr-ui--sections-by-start))))
      (goto-char (nostr-ui-section-start next))
      (nostr-ui-update-selection))))

(defun nostr-ui-prev-section ()
  "Move point to the previous section."
  (interactive)
  (let ((pos (point))
        best)
    (dolist (section nostr-ui--sections)
      (when (and (< (nostr-ui-section-start section) pos)
                 (or (not best)
                     (> (nostr-ui-section-start section)
                        (nostr-ui-section-start best))))
        (setq best section)))
    (when best
      (goto-char (nostr-ui-section-start best))
      (nostr-ui-update-selection))))

(defun nostr-ui-goto-first-section ()
  "Move point to the first rendered section and highlight it."
  (when-let* ((first (car (nostr-ui--sections-by-start))))
    (goto-char (nostr-ui-section-start first))
    (nostr-ui-update-selection)))

(defun nostr-ui--section-position-state (&optional position)
  "Return a restorable position state for POSITION or point."
  (save-excursion
    (when position
      (goto-char position))
    (if-let* ((section (nostr-ui-section-at-point)))
        `((type . ,(nostr-ui-section-type section))
          (id . ,(nostr-ui-section-id section))
          (offset . ,(max 0 (- (point) (nostr-ui-section-start section))))
          (fallback . ,(point)))
      `((fallback . ,(point))))))

(defun nostr-ui--window-position-states ()
  "Return restorable point/window-start states for windows showing this buffer."
  (delq nil
        (mapcar
         (lambda (window)
           (when (eq (window-buffer window) (current-buffer))
             `((window . ,window)
               (point . ,(nostr-ui--section-position-state
                          (window-point window)))
               (start . ,(nostr-ui--section-position-state
                          (window-start window))))))
         (window-list nil 'no-minibuf))))

(defun nostr-ui-capture-position ()
  "Capture current buffer and visible window positions."
  `((point . ,(nostr-ui--section-position-state))
    (windows . ,(nostr-ui--window-position-states))))

(defun nostr-ui--find-section (type id)
  "Return rendered section matching TYPE and ID."
  (cl-find-if
   (lambda (section)
     (and (eq (nostr-ui-section-type section) type)
          (equal (nostr-ui-section-id section) id)))
   nostr-ui--sections))

(defun nostr-ui--resolve-position (state)
  "Resolve saved position STATE in the current buffer."
  (let* ((type (alist-get 'type state))
         (id (alist-get 'id state))
         (offset (or (alist-get 'offset state) 0))
         (fallback (or (alist-get 'fallback state) (point-min)))
         (section (and type id (nostr-ui--find-section type id))))
    (if section
        (min (max (nostr-ui-section-start section)
                  (+ (nostr-ui-section-start section) offset))
             (max (nostr-ui-section-start section)
                  (1- (nostr-ui-section-end section))))
      (min (max fallback (point-min)) (point-max)))))

(defun nostr-ui-restore-position (state)
  "Restore buffer and visible window positions from STATE.
Return non-nil when a usable STATE was restored."
  (when state
    (let ((point-state (alist-get 'point state))
          (window-states (alist-get 'windows state)))
      (goto-char (nostr-ui--resolve-position point-state))
      (dolist (window-state window-states)
        (let ((window (alist-get 'window window-state)))
          (when (window-live-p window)
            (set-window-point
             window
             (nostr-ui--resolve-position (alist-get 'point window-state)))
            (set-window-start
             window
             (nostr-ui--resolve-position (alist-get 'start window-state))
             t))))
      (nostr-ui-update-selection)
      t)))

(defun nostr-ui-finish-refresh (&optional position-state)
  "Restore POSITION-STATE or move to the first rendered section."
  (or (nostr-ui-restore-position position-state)
      (nostr-ui-goto-first-section)
      (goto-char (point-min))))

(defun nostr-ui-insert-empty-state (message &optional action-hint)
  "Insert empty-state MESSAGE and optional ACTION-HINT."
  (insert (propertize message 'face 'nostr-ui-meta))
  (insert "\n")
  (when action-hint
    (insert (propertize action-hint 'face 'nostr-ui-meta))
    (insert "\n"))
  (insert "\n"))

(defun nostr-ui-insert-footer (commands)
  "Insert compact footer from COMMANDS.
COMMANDS is a list of strings such as \"g refresh\"."
  (when commands
    (insert "\n")
    (insert (propertize (string-join commands "   ") 'face 'nostr-ui-meta))
    (insert "\n")))

(defun nostr-ui-insert-status-header (title &optional subtitle details)
  "Insert a compact page status header.
TITLE is the page title.  SUBTITLE and DETAILS are optional strings."
  (insert (propertize title 'face 'nostr-ui-status-title))
  (when subtitle
    (insert (propertize (format "  %s" subtitle) 'face 'nostr-ui-status-detail)))
  (insert "\n")
  (when details
    (insert (propertize details 'face 'nostr-ui-meta))
    (insert "\n"))
  (insert "\n"))

(defun nostr-ui-insert-badge (text &optional face)
  "Insert TEXT as a compact metadata badge."
  (insert (propertize (format " %s " text) 'face (or face 'nostr-ui-badge))))

(defun nostr-ui-insert-badge-line (badges &optional indent)
  "Insert BADGES as a line of compact metadata values.
BADGES is a list of strings.  INDENT is inserted before the badge line."
  (let ((items (seq-filter (lambda (badge)
                             (and (stringp badge)
                                  (not (string-empty-p badge))))
                           badges)))
    (when items
      (insert (or indent ""))
      (cl-loop for badge in items
               for first = t then nil
               do (progn
                    (unless first (insert " "))
                    (nostr-ui-insert-badge badge)))
      (insert "\n"))))

(defun nostr-ui-insert-primary-nav (items active)
  "Insert primary navigation ITEMS and mark ACTIVE.
ITEMS is a list of (KIND KEY LABEL COMMAND)."
  (insert (propertize "Views  " 'face 'nostr-ui-meta))
  (dolist (item items)
    (pcase-let ((`(,kind ,key ,label ,command) item))
      (let* ((selected (eq kind active))
             (text (if selected
                       (format "[%s %s]" key label)
                     (format " %s %s " key label))))
        (insert-text-button
         text
         'face (if selected 'nostr-ui-nav-active 'nostr-ui-nav-inactive)
         'follow-link t
         'help-echo (format "Show %s" label)
         'action (lambda (_button) (funcall command)))
        (insert " "))))
  (insert "\n\n"))

(defun nostr-ui-format-exact-time (unix-time)
  "Format UNIX-TIME as an exact human-readable timestamp."
  (let* ((system-time-locale "C")
         (time (seconds-to-time (or unix-time 0)))
         (day (string-trim-left (format-time-string "%e" time))))
    (format "%s %s" day (format-time-string "%B %Y at %H:%M" time))))

(defun nostr-ui-format-relative-time (unix-time &optional now)
  "Format UNIX-TIME relative to NOW.
Near-future clock skew renders as \"just now\"; larger future timestamps render
as \"in N minutes\", \"in N hours\", or \"in N days\"."
  (let* ((timestamp (or unix-time 0))
         (current (or now (truncate (float-time))))
         (delta (- current timestamp))
         (future (< delta -59))
         (seconds (abs delta)))
    (cond
     ((< seconds 60) "just now")
     ((< seconds 3600)
      (let ((minutes (/ seconds 60)))
        (format "%s%d minute%s%s"
                (if future "in " "")
                minutes
                (if (= minutes 1) "" "s")
                (if future "" " ago"))))
     ((< seconds 86400)
      (let ((hours (/ seconds 3600)))
        (format "%s%d hour%s%s"
                (if future "in " "")
                hours
                (if (= hours 1) "" "s")
                (if future "" " ago"))))
     (t
      (let ((days (/ seconds 86400)))
        (format "%s%d day%s%s"
                (if future "in " "")
                days
                (if (= days 1) "" "s")
                (if future "" " ago")))))))

(defun nostr-ui-format-time (unix-time &optional style)
  "Format UNIX-TIME for display using STYLE.
STYLE may be `relative', `exact', `feed', `thread', or nil.  Feed style uses
relative dates; thread/detail style uses exact dates."
  (if (memq style '(exact thread detail))
      (nostr-ui-format-exact-time unix-time)
    (nostr-ui-format-relative-time unix-time)))

(defun nostr-ui--shorten-identifier (value &optional prefix)
  "Shorten VALUE with optional PREFIX for compact display."
  (let* ((text (if (stringp value) value "unknown"))
         (prefix (or prefix "")))
    (if (<= (length text) 16)
        (concat prefix text)
      (format "%s%s...%s"
              prefix
              (substring text 0 8)
              (substring text -8)))))

(defun nostr-ui--string-value (&rest values)
  "Return the first non-empty string from VALUES."
  (seq-find (lambda (value)
              (and (stringp value)
                   (not (string-empty-p value))))
            values))

(defun nostr-ui--event-nip05 (event)
  "Return EVENT's cached NIP-05 identifier, when present."
  (nostr-ui--string-value
   (alist-get 'nip05 event)
   (alist-get 'author-nip05 event)
   (alist-get 'profile-nip05 event)
   (when (and nostr-db--connection (alist-get 'pubkey event))
     (nth 5 (nostr-db-select-profile (alist-get 'pubkey event))))))

(defun nostr-ui--event-profile-name (event)
  "Return cached author profile name for EVENT, when available."
  (let ((profile (when (and nostr-db--connection (alist-get 'pubkey event))
                   (nostr-db-select-profile (alist-get 'pubkey event)))))
    (nostr-ui--string-value
     (alist-get 'author event)
     (alist-get 'display-name event)
     (alist-get 'display_name event)
     (alist-get 'name event)
     (nth 2 profile)
     (nth 1 profile))))

(defun nostr-ui--event-picture (event)
  "Return cached author picture URL for EVENT, when available."
  (nostr-ui--string-value
   (alist-get 'picture event)
   (alist-get 'profile-picture event)
   (when (and nostr-db--connection (alist-get 'pubkey event))
     (nth 4 (nostr-db-select-profile (alist-get 'pubkey event))))))

(defun nostr-ui--image-display-props (size)
  "Return display image props constrained to SIZE pixels."
  (list :max-width size :max-height size :ascent nostr-ui-avatar-ascent))

(defun nostr-ui--display-image-string (file label size &optional url)
  "Return display string for FILE with LABEL constrained to SIZE.
When URL is non-nil, add media properties for the source URL."
  (if (and file
           (file-exists-p file)
           (display-images-p)
           (image-supported-file-p file))
      (propertize " "
                  'display (apply #'create-image
                                  file nil nil
                                  (nostr-ui--image-display-props size))
                  'nostr-media-url url
                  'nostr-media-cache-file file)
    (propertize label
                'face 'nostr-ui-avatar-placeholder
                'nostr-media-url url
                'nostr-media-cache-file file)))

(defun nostr-ui--default-avatar-string (size)
  "Return the default avatar string constrained to SIZE."
  (nostr-ui--display-image-string nostr-ui-default-avatar-file "[Ostrich]" size))

(defun nostr-ui--avatar-string (url size)
  "Return a display string or button label for avatar URL at SIZE."
  (let* ((url (and (stringp url) url))
         (file (and url (nostr-media-cache-file url))))
    (cond
     ((and file
           (file-exists-p file)
           (display-images-p)
           (image-supported-file-p file))
      (propertize " "
                  'display (apply #'create-image
                                  file nil nil
                                  (nostr-ui--image-display-props size))
                  'nostr-media-url url
                  'nostr-media-cache-file file))
     (url
      (nostr-ui--display-image-string file "[Avatar]" size url))
     (t
      (nostr-ui--default-avatar-string size)))))

(defun nostr-ui--replace-avatar-button (button url size)
  "Replace BUTTON with cached avatar URL at SIZE."
  (let ((start (button-start button))
        (end (button-end button)))
    (let ((inhibit-read-only t))
      (delete-region start end)
      (goto-char start)
      (insert (nostr-ui--avatar-string url size)))))

(defun nostr-ui-load-avatar-at-point (&optional button)
  "Load avatar at point or BUTTON and replace the placeholder in-place."
  (interactive)
  (let* ((button (or button
                     (button-at (point))
                     (button-at (max (point-min) (1- (point))))
                     (user-error "No avatar placeholder at point")))
         (url (or (button-get button 'nostr-media-url)
                  (user-error "No avatar URL at point")))
         (size (or (button-get button 'nostr-avatar-size)
                   nostr-ui-avatar-size))
         (file (nostr-media-cache-file url)))
    (if (file-exists-p file)
        (nostr-ui--replace-avatar-button button url size)
      (let ((buf (current-buffer)))
        (nostr-media-fetch
         url
         (lambda (headers data)
           (condition-case err
               (progn
                 (nostr-media--write-cache url headers data)
                 ;; A `g' refresh (erase-buffer) or buffer kill may have
                 ;; happened during the network fetch; only touch the buffer
                 ;; if it is still live and the captured button still exists
                 ;; at its recorded position with the same media URL.
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     ;; Buttons here are text-property buttons, so a fresh
                     ;; `button-at' returns a distinct object; match on the URL
                     ;; property rather than `eq' identity.
                     (let* ((start (ignore-errors (button-start button)))
                            (current (and (integerp start)
                                          (<= (point-min) start)
                                          (< start (point-max))
                                          (button-at start))))
                       (when (and current
                                  (equal (button-get current 'nostr-media-url)
                                         url))
                         (nostr-ui--replace-avatar-button current url size))))))
             (error (message "[nostr] %s" (error-message-string err)))))
         (lambda (message)
           (message "[nostr] %s" message)))))
    url))

(defun nostr-ui-insert-avatar (url &optional size)
  "Insert avatar for URL using SIZE.
Cached images render inline.  Uncached URLs render as a button that uses the
shared media cache and replaces itself in-place."
  (when nostr-ui-show-avatars
    (let ((size (or size nostr-ui-avatar-size))
          (url (and (stringp url) url)))
      (if url
          (let ((file (nostr-media-cache-file url)))
            (if (and (file-exists-p file)
                     (display-images-p)
                     (image-supported-file-p file))
                (insert (nostr-ui--avatar-string url size))
              (insert-text-button
               (nostr-ui--avatar-string url size)
               'follow-link t
               'nostr-media-url url
               'nostr-avatar-size size
               'help-echo "Load this profile picture"
               'action (lambda (button) (nostr-ui-load-avatar-at-point button)))))
        (insert (nostr-ui--default-avatar-string size))))))

(defun nostr-ui--cached-npub (pubkey)
  "Return cached or synchronously encodable npub for PUBKEY."
  (when (stringp pubkey)
    (or (gethash pubkey nostr-ui--npub-cache)
        (when (fboundp 'nostr-nip19-encode-sync)
          (ignore-errors
            (let ((npub (alist-get 'value
                                   (nostr-nip19-encode-sync "npub" pubkey))))
              (when (and (stringp npub) (string-prefix-p "npub1" npub))
                (puthash pubkey npub nostr-ui--npub-cache)
                npub)))))))

(defun nostr-ui-format-author (event)
  "Return formatted author identity for EVENT."
  (let* ((pubkey (alist-get 'pubkey event))
         (name (nostr-ui--event-profile-name event))
         (nip05 (nostr-ui--event-nip05 event))
         (identifier (or nip05
                         (when-let* ((npub (nostr-ui--cached-npub pubkey)))
                           (nostr-ui--shorten-identifier npub))
                         (nostr-ui--shorten-identifier pubkey))))
    (if (and name (not (equal name pubkey)))
        (format "%s · %s" name identifier)
      identifier)))

(defun nostr-ui--publish-receipt-summary (event-id)
  "Return a compact publish receipt summary for EVENT-ID, when available."
  (when (and (boundp 'nostr-db--connection) nostr-db--connection event-id)
    (let ((counts nil))
      (dolist (receipt (nostr-db-select-publish-receipts event-id))
        (let ((state (alist-get 'state receipt)))
          (setf (alist-get state counts nil nil #'equal)
                (1+ (or (alist-get state counts nil nil #'equal) 0)))))
      (when counts
        (string-join
         (delq nil
               (mapcar (lambda (state)
                         (when-let* ((count (alist-get state counts nil nil #'equal)))
                           (format "%s:%d" state count)))
                       '("pending" "accepted" "rejected")))
         "  ")))))

(defun nostr-ui--insert-publish-receipts (event indent)
  "Insert publish receipts for EVENT using INDENT."
  (when-let* ((event-id (alist-get 'id event))
              (summary (nostr-ui--publish-receipt-summary event-id)))
    (insert indent)
    (insert (propertize (format "Publish  %s\n" summary) 'face 'nostr-ui-meta))))

(defun nostr-ui--event-counts (event)
  "Return the full interaction counts alist for EVENT, memoized per buffer.
Counts are fetched from the database at most once per event id, so a single
render or refresh runs `nostr-db-event-counts' once rather than once per key."
  (when-let* ((id (alist-get 'id event)))
    (unless nostr-ui--event-counts-cache
      (setq nostr-ui--event-counts-cache (make-hash-table :test #'equal)))
    (let ((cached (gethash id nostr-ui--event-counts-cache 'missing)))
      (if (not (eq cached 'missing))
          cached
        (puthash id
                 (and nostr-db--connection (nostr-db-event-counts id))
                 nostr-ui--event-counts-cache)))))

(defun nostr-ui--event-count (event key)
  "Return EVENT count KEY, loading cached counts lazily when needed.
A count carried inline on EVENT takes precedence; otherwise the value is
looked up from the memoized full counts alist for the event id."
  (if-let* ((existing (assoc key event)))
      (or (cdr existing) 0)
    (or (alist-get key (nostr-ui--event-counts event)) 0)))

(defun nostr-ui--note-option (options key default)
  "Return OPTIONS plist KEY or DEFAULT."
  (if (and (consp options) (keywordp (car options)))
      (plist-get options key)
    default))

(defun nostr-ui--note-depth (options)
  "Return note indentation depth from OPTIONS."
  (if (numberp options)
      options
    (or (nostr-ui--note-option options :depth 0) 0)))

(defun nostr-ui--note-style (options)
  "Return note time style from OPTIONS."
  (cond
   ((numberp options) 'thread)
   ((and (consp options) (keywordp (car options)))
    (or (plist-get options :style) 'feed))
   (t 'feed)))

(defun nostr-ui--event-stat (event keys)
  "Return the first non-nil numeric stat from EVENT by KEYS."
  (or (cl-some (lambda (key)
                 (let ((value (alist-get key event)))
                   (when value
                     (if (numberp value)
                         value
                       (string-to-number (format "%s" value))))))
               keys)
      0))

(defun nostr-ui--note-footer (event)
  "Return compact stats footer for EVENT."
  (let* ((zaps (nostr-ui--event-count event 'zaps))
         (zap-msats (nostr-ui--event-count event 'zap-msats))
         (sats (max (nostr-ui--event-stat event '(zap-sats zap_sats sats))
                    (floor (/ (or zap-msats 0) 1000))))
         (parts (list
                 (format "↩ %s" (nostr-ui--event-count event 'replies))
                 (format "♥ %s" (nostr-ui--event-count event 'reactions))
                 (format "↻ %s" (nostr-ui--event-count event 'reposts))
                 (if (> sats 0)
                     (format "⚡ %s (%s sats)" zaps sats)
                   (format "⚡ %s" zaps)))))
    (string-join parts "   ")))

(defun nostr-ui--note-badges (event style)
  "Return compact metadata badges for EVENT using STYLE."
  (let* ((created (or (alist-get 'created-at event) (alist-get 'created_at event)))
         (time (nostr-ui-format-time created style))
         (relay (alist-get 'relay event))
         (reply-id (or (alist-get 'reply-id event) (alist-get 'reply_id event)))
         (root-id (or (alist-get 'root-id event) (alist-get 'root_id event)))
         (quote-id (or (alist-get 'quote-id event) (alist-get 'quote_id event)))
         (media-count (length (nostr-event-media-urls (alist-get 'content event)))))
    (delq nil
          (list time
                (when relay (format "relay %s" relay))
                (when reply-id "reply")
                (when (and root-id (not reply-id)) "conversation")
                (when quote-id "quote")
                (when (> media-count 0)
                  (format "media %d" media-count))))))

(defun nostr-ui--insert-note-heading (section indent picture author)
  "Insert note SECTION heading with INDENT, PICTURE, and AUTHOR."
  (insert indent)
  (insert-text-button
   "▾"
   'follow-link t
   'nostr-ui-section section
   'action (lambda (_button) (nostr-ui-toggle-section)))
  (insert " ")
  (nostr-ui-insert-avatar picture nostr-ui-avatar-size)
  (insert " ")
  (insert-text-button
   (format "%s\n" author)
   'face 'nostr-ui-author
   'follow-link t
   'nostr-ui-section section
   'action (lambda (_button) (nostr-ui-toggle-section))))

(defun nostr-ui--insert-filled-content (content indent)
  "Insert CONTENT as filled card body text using INDENT."
  (let* ((prefix (concat indent "  "))
         (paragraphs (split-string (or content "") "\n\n"))
         (fill-column nostr-ui-card-fill-column)
         (adaptive-fill-mode nil))
    (dolist (paragraph paragraphs)
      (if (string-empty-p paragraph)
          (insert prefix "\n")
        (let ((start (point))
              (fill-prefix prefix))
          (insert prefix)
          (insert (propertize (string-trim paragraph) 'face 'nostr-ui-content))
          (fill-region-as-paragraph start (point))
          (insert "\n"))))))

(defun nostr-ui--section-media-region (section)
  "Return cons of content bounds for SECTION."
  (let* ((start (nostr-ui-section-content-start section))
         (section-start (nostr-ui-section-start section))
         (next (cl-find-if
                (lambda (candidate)
                  (> (nostr-ui-section-start candidate) section-start))
                (nostr-ui--sections-by-start))))
    (cons start (if next
                    (nostr-ui-section-start next)
                  (point-max)))))

(defun nostr-ui--media-placeholder-position (url start end)
  "Return position of URL media placeholder between START and END."
  (let ((pos start)
        found)
    (while (and (not found) (< pos end))
      (when (and (equal (get-text-property pos 'nostr-media-url) url)
                 (not (get-text-property pos 'nostr-media-rendered)))
        (setq found pos))
      (setq pos (next-single-property-change pos 'nostr-media-url nil end)))
    found))

(defun nostr-ui-toggle-note-media ()
  "Toggle rendered media previews for the selected note."
  (interactive)
  (let* ((section (or (nostr-ui-section-at-point)
                      (user-error "No note selected")))
         (event (nostr-ui-section-data section)))
    (unless (eq (nostr-ui-section-type section) 'note)
      (user-error "No note selected"))
    (let* ((urls (nostr-event-media-urls (alist-get 'content event)))
           (bounds (nostr-ui--section-media-region section))
           (start (car bounds))
           (end (cdr bounds)))
      (unless urls
        (user-error "Selected note has no supported media"))
      (if (nostr-media-rendered-in-region-p start end)
          (message "Removed %d media preview%s"
                   (nostr-media-remove-rendered-in-region start end)
                   (if (= (length urls) 1) "" "s"))
        (dolist (url urls)
          (when-let* ((position (nostr-ui--media-placeholder-position url start end)))
            (save-excursion
              (goto-char position)
              (nostr-media-load-at-point))))
        (message "Loading %d media preview%s"
                 (length urls)
                 (if (= (length urls) 1) "" "s"))))))

(defun nostr-ui-insert-note (event &optional options)
  "Insert EVENT as a note section.
OPTIONS may be a numeric thread DEPTH for compatibility, or a plist accepting
`:depth' and `:style'.  Feed style uses relative dates; thread/detail style uses
exact dates."
  (let* ((depth (nostr-ui--note-depth options))
         (style (nostr-ui--note-style options))
         (indent (make-string (* depth 2) ?\s))
         (author (nostr-ui-format-author event))
         (content (or (alist-get 'content event) ""))
         (picture (nostr-ui--event-picture event)))
    (insert indent)
    (insert (propertize "────────────────────────────────────────\n"
                        'face 'nostr-ui-card-border))
    (nostr-ui-with-section 'note (alist-get 'id event) event
        (lambda (section)
          (nostr-ui--insert-note-heading section indent picture author))
      (nostr-ui-insert-badge-line (nostr-ui--note-badges event style)
                                  (concat indent "  "))
      (nostr-ui--insert-filled-content content indent)
      (nostr-ui--insert-publish-receipts event indent)
      (dolist (url (nostr-event-media-urls (alist-get 'content event)))
        (insert indent)
        (insert (propertize "  " 'face 'nostr-ui-card-border))
        (insert-text-button
         (format "[image: %s]" url)
         'follow-link t
         'nostr-media-url url
         'help-echo "Load this image inline"
         'action (lambda (_button) (nostr-media-load-at-point)))
        (insert "\n"))
      (nostr-ui-insert-badge-line (split-string (nostr-ui--note-footer event) "   ")
                                  (concat indent "  "))
      (insert "\n")
      (insert "\n"))))

(provide 'nostr-ui)
;;; nostr-ui.el ends here
