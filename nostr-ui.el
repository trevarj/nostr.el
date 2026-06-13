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

;; Defined in nostr-setup.el / set by the account derivation flow.
(defvar nostr-current-pubkey)

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

(defvar-local nostr-ui--profile-cache nil
  "Buffer-local memoization of `nostr-db-select-profile' keyed by pubkey.
Reset each render and primed once per feed so the per-note author lookups
(name, nip05, picture) share a single batched query instead of one per note.")

(defvar-local nostr-ui--selection-overlay nil
  "Overlay highlighting the current section heading.")

(defvar-local nostr-ui--open-media-notes nil
  "Hash table of note ids whose media previews are open in this buffer.")

(defvar-local nostr-ui--folded-sections nil
  "Hash table of section keys folded in this buffer.")

(defvar nostr-ui--npub-cache (make-hash-table :test #'equal)
  "Cache of hex pubkeys to encoded npub strings.")

(defvar nostr-ui--image-cache (make-hash-table :test #'equal)
  "Cache of decoded image descriptors keyed by (FILE SIZE MTIME).
Persists across renders so a repeated avatar is decoded once, not per note.")

(defvar nostr-ui--verified-nip05-cache (make-hash-table :test #'equal)
  "Verified NIP-05 identifiers keyed by pubkey and identifier.")

(defvar nostr-ui--avatar-fetches (make-hash-table :test #'equal)
  "Avatar URLs currently being fetched automatically.")

(defvar nostr-ui--nevent-cache (make-hash-table :test #'equal)
  "Cache of nevent strings to decoded event ids or `invalid'.")

(declare-function nostr-relay-fetch-events-by-id "nostr-relay" (event-ids &optional limit))

(defcustom nostr-ui-show-avatars t
  "Whether note cards and profile headers show profile picture placeholders."
  :type 'boolean
  :group 'nostr)

(defcustom nostr-ui-auto-load-avatars t
  "Whether unloaded avatar placeholders should fetch images automatically.
When nil, `[Avatar]' remains a manual button that can be loaded with RET or
mouse activation."
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

(defcustom nostr-ui-relay-count-icon "◉"
  "Compact icon used before the inbound relay count on note cards."
  :type 'string
  :group 'nostr)

(defcustom nostr-ui-default-avatar-file
  (expand-file-name "assets/default-avatar-ostrich.png"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Default avatar image used when a profile has no picture URL."
  :type 'file
  :group 'nostr)

(defcustom nostr-ui-logo-file
  (expand-file-name "assets/logo.png"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Logo image shown at the start of page status headers."
  :type 'file
  :group 'nostr)

(defcustom nostr-ui-logo-size 48
  "Maximum pixel size for the header logo image."
  :type 'integer
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
        nostr-ui--event-counts-cache nil
        nostr-ui--profile-cache nil)
  (remove-overlays (point-min) (point-max) 'nostr-ui-section t)
  (when (overlayp nostr-ui--selection-overlay)
    (delete-overlay nostr-ui--selection-overlay))
  (setq nostr-ui--selection-overlay nil)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun nostr-ui--section-key (section)
  "Return stable fold-state key for SECTION."
  (list (nostr-ui-section-type section)
        (nostr-ui-section-id section)))

(defun nostr-ui--folded-sections ()
  "Return the buffer-local folded section state table."
  (unless (hash-table-p nostr-ui--folded-sections)
    (setq nostr-ui--folded-sections (make-hash-table :test #'equal)))
  nostr-ui--folded-sections)

(defun nostr-ui--set-section-glyph (section folded)
  "Set SECTION heading glyph according to FOLDED."
  (save-excursion
    (goto-char (nostr-ui-section-start section))
    (when (re-search-forward "[▾▸]" (line-end-position) t)
      (let ((inhibit-read-only t))
        (delete-char -1)
        (insert (if folded "▸" "▾"))))))

(defun nostr-ui--apply-section-fold-state (section)
  "Apply remembered fold state to SECTION."
  (let ((folded (gethash (nostr-ui--section-key section)
                         (nostr-ui--folded-sections))))
    (when folded
      (setf (nostr-ui-section-folded section) t)
      (when-let* ((overlay (nostr-ui-section-overlay section)))
        (overlay-put overlay 'invisible t))
      (nostr-ui--set-section-glyph section t))))

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
     (nostr-ui--apply-section-fold-state section)
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
    (if (nostr-ui-section-folded section)
        (puthash (nostr-ui--section-key section) t
                 (nostr-ui--folded-sections))
      (remhash (nostr-ui--section-key section)
               (nostr-ui--folded-sections)))
    (overlay-put overlay 'invisible (nostr-ui-section-folded section))
    (nostr-ui--set-section-glyph section (nostr-ui-section-folded section))
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
TITLE is the page title.  SUBTITLE and DETAILS are optional strings.
A Nostr logo image is shown before TITLE."
  (insert (nostr-ui--display-image-string
           nostr-ui-logo-file "[Nostr]" nostr-ui-logo-size)
          "  ")
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

(defun nostr-ui--cached-profile (pubkey)
  "Return the stored profile row for PUBKEY, memoized per buffer for this render.
Populated in one batch by `nostr-ui-prime-caches'; falls back to a single query
on a miss so callers work without priming."
  (when (and nostr-db--connection (stringp pubkey))
    (unless nostr-ui--profile-cache
      (setq nostr-ui--profile-cache (make-hash-table :test #'equal)))
    (let ((cached (gethash pubkey nostr-ui--profile-cache 'missing)))
      (if (not (eq cached 'missing))
          cached
        (puthash pubkey (nostr-db-select-profile pubkey)
                 nostr-ui--profile-cache)))))

(defun nostr-ui--event-nip05 (event)
  "Return EVENT's cached NIP-05 identifier, when present."
  (nostr-ui--string-value
   (alist-get 'nip05 event)
   (alist-get 'author-nip05 event)
   (alist-get 'profile-nip05 event)
   (nth 5 (nostr-ui--cached-profile (alist-get 'pubkey event)))))

(defun nostr-ui--event-profile-name (event)
  "Return cached author profile name for EVENT, when available."
  (let ((profile (nostr-ui--cached-profile (alist-get 'pubkey event))))
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
   (nth 4 (nostr-ui--cached-profile (alist-get 'pubkey event)))))

(defun nostr-ui--image-display-props (size)
  "Return display image props constrained to SIZE pixels."
  (list :max-width size :max-height size :ascent nostr-ui-avatar-ascent))

(defun nostr-ui--cached-image (file size)
  "Return a `create-image' descriptor for FILE at SIZE, decoded at most once.
Avatars repeat across notes and refreshes; caching the descriptor avoids
re-decoding the same image on every render.  Re-decodes only when FILE's
modification time changes."
  (let* ((mtime (file-attribute-modification-time (file-attributes file)))
         (key (list file size mtime)))
    (or (gethash key nostr-ui--image-cache)
        (puthash key
                 (apply #'create-image file nil nil
                        (nostr-ui--image-display-props size))
                 nostr-ui--image-cache))))

(defun nostr-ui--display-image-string (file label size &optional url)
  "Return display string for FILE with LABEL constrained to SIZE.
When URL is non-nil, add media properties for the source URL."
  (if (and file
           (file-exists-p file)
           (display-images-p)
           (image-supported-file-p file))
      (propertize " "
                  'display (nostr-ui--cached-image file size)
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
                  'display (nostr-ui--cached-image file size)
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

(defun nostr-ui--replace-avatar-placeholders (url size)
  "Replace all live avatar placeholders for URL in the current buffer."
  (save-excursion
    (let ((pos (point-min))
          (inhibit-read-only t))
      (while (< pos (point-max))
        (let ((next (next-single-property-change pos 'nostr-media-url nil
                                                 (point-max))))
          (when (and (equal (get-text-property pos 'nostr-media-url) url)
                     (button-at pos))
            (let ((button (button-at pos)))
              (delete-region (button-start button) (button-end button))
              (goto-char pos)
              (insert (nostr-ui--avatar-string url size))))
          (setq pos (if (= next pos) (1+ pos) next)))))))

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
        (nostr-ui--replace-avatar-placeholders url size)
      (let ((buf (current-buffer)))
        (nostr-media-fetch
         url
         (lambda (headers data)
           (condition-case err
               (progn
                 (nostr-media--write-cache url headers data)
                 ;; A `g' refresh (erase-buffer) or buffer kill may have
                 ;; happened during the network fetch; only touch the buffer
                 ;; if it is still live.
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (nostr-ui--replace-avatar-placeholders url size))))
             (error (message "[nostr] %s" (error-message-string err)))))
         (lambda (message)
           (message "[nostr] %s" message)))))
    url))

(defun nostr-ui--maybe-auto-load-avatar (button url)
  "Auto-load avatar BUTTON for URL when customization allows it."
  (when (and nostr-ui-auto-load-avatars
             (not (gethash url nostr-ui--avatar-fetches)))
    (puthash url t nostr-ui--avatar-fetches)
    (let ((buffer (current-buffer)))
      (run-at-time
       0 nil
       (lambda ()
         (unwind-protect
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (and (ignore-errors (button-start button))
                            (equal (button-get button 'nostr-media-url) url))
                   (nostr-ui-load-avatar-at-point button))))
           (remhash url nostr-ui--avatar-fetches)))))))

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
               'action (lambda (button) (nostr-ui-load-avatar-at-point button)))
              (nostr-ui--maybe-auto-load-avatar (button-at (1- (point))) url)))
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

(defun nostr-ui-record-nip05-verification (pubkey identifier)
  "Record that IDENTIFIER has verified for PUBKEY in this Emacs session."
  (when (and (stringp pubkey) (stringp identifier))
    (puthash (cons pubkey identifier) t nostr-ui--verified-nip05-cache)))

(defun nostr-ui-nip05-verified-p (pubkey identifier)
  "Return non-nil when IDENTIFIER is verified for PUBKEY."
  (and (stringp pubkey)
       (stringp identifier)
       (gethash (cons pubkey identifier) nostr-ui--verified-nip05-cache)))

(defun nostr-ui-format-nip05 (identifier &optional pubkey)
  "Format NIP-05 IDENTIFIER for display.
The special local part `_@domain' is shown as `domain'.  A checkmark is shown
only when IDENTIFIER is known verified for PUBKEY."
  (when (stringp identifier)
    (let* ((parsed (ignore-errors (nostr-nip05-parse-identifier identifier)))
           (name (alist-get 'name parsed))
           (domain (alist-get 'domain parsed))
           (display (if (and name domain (string= name "_"))
                        domain
                      identifier)))
      (if (nostr-ui-nip05-verified-p pubkey identifier)
          (format "✓ %s" display)
        display))))

(defun nostr-ui-format-author (event)
  "Return formatted author identity for EVENT."
  (let* ((pubkey (alist-get 'pubkey event))
         (name (nostr-ui--event-profile-name event))
         (nip05 (nostr-ui--event-nip05 event))
         (identifier (or (nostr-ui-format-nip05 nip05 pubkey)
                         (when-let* ((npub (nostr-ui--cached-npub pubkey)))
                           (nostr-ui--shorten-identifier npub))
                         (nostr-ui--shorten-identifier pubkey))))
    (if (and name (not (equal name pubkey)))
        (format "%s · %s" name identifier)
      identifier)))

(defun nostr-ui-format-reposter (event)
  "Return formatted repost attribution for EVENT, when present."
  (when-let* ((pubkey (alist-get 'reposted-by event)))
    (let* ((name (nostr-ui--string-value (alist-get 'reposted-by-name event)))
           (nip05 (nostr-ui--string-value (alist-get 'reposted-by-nip05 event)))
           (identifier (or (nostr-ui-format-nip05 nip05 pubkey)
                           (when-let* ((npub (nostr-ui--cached-npub pubkey)))
                             (nostr-ui--shorten-identifier npub))
                           (nostr-ui--shorten-identifier pubkey))))
      (if (and name (not (equal name pubkey)))
          (format "%s · %s" name identifier)
        identifier))))

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
  "Insert publish receipts for EVENT using INDENT.
Publish receipts only exist for the local account's own published notes, so
skip the per-note query for other authors once the current pubkey is known."
  (when (or (not (bound-and-true-p nostr-current-pubkey))
            (equal (alist-get 'pubkey event) nostr-current-pubkey))
    (when-let* ((event-id (alist-get 'id event))
                (summary (nostr-ui--publish-receipt-summary event-id)))
      (insert indent)
      (insert (propertize (format "Publish  %s\n" summary) 'face 'nostr-ui-meta)))))

(defun nostr-ui-prime-caches (events)
  "Warm the per-render interaction-count and profile caches for EVENTS.
Loads counts and profiles in a fixed number of batched queries so rendering a
feed of N notes does not run O(N) per-note queries.  Call after `nostr-ui-clear'
and before inserting notes."
  (when nostr-db--connection
    (unless nostr-ui--event-counts-cache
      (setq nostr-ui--event-counts-cache (make-hash-table :test #'equal)))
    (unless nostr-ui--profile-cache
      (setq nostr-ui--profile-cache (make-hash-table :test #'equal)))
    (let ((ids (delq nil (mapcar (lambda (event) (alist-get 'id event)) events)))
          (pubkeys (delq nil (mapcar (lambda (event) (alist-get 'pubkey event))
                                     events))))
      (dolist (pair (nostr-db-event-counts-batch ids))
        (puthash (car pair) (cdr pair) nostr-ui--event-counts-cache))
      (let ((profiles (nostr-db-select-profiles-batch pubkeys)))
        ;; Store nil for pubkeys with no profile so a miss is not re-queried.
        (dolist (pubkey (delete-dups (seq-filter #'stringp pubkeys)))
          (puthash pubkey (cdr (assoc pubkey profiles))
                   nostr-ui--profile-cache))))))

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
         (relay-count (nostr-ui--event-count event 'relay-count))
         (reply-id (or (alist-get 'reply-id event) (alist-get 'reply_id event)))
         (root-id (or (alist-get 'root-id event) (alist-get 'root_id event)))
         (quote-id (or (alist-get 'quote-id event) (alist-get 'quote_id event)))
         (media-count (length (nostr-event-media-items (alist-get 'content event)))))
    (delq nil
          (list time
                (when (> relay-count 0)
                  (format "%s %d" nostr-ui-relay-count-icon relay-count))
                (when (or reply-id root-id) "reply")
                (when quote-id "quote")
                (when (> media-count 0)
                  (format "media %d" media-count))))))

(defun nostr-ui--strip-nostr-uri (value)
  "Return VALUE without a nostr URI prefix."
  (if (and (stringp value)
           (string-prefix-p "nostr:" value))
      (substring value 6)
    value))

(defun nostr-ui--short-id (value)
  "Return shortened display text for VALUE."
  (if (and (stringp value) (> (length value) 16))
      (concat (substring value 0 8) "…" (substring value -8))
    (or value "")))

(defun nostr-ui--decode-nevent-event-id (nevent)
  "Return event id decoded from NEVENT, caching the backend result."
  (let* ((value (nostr-ui--strip-nostr-uri nevent))
         (cached (gethash value nostr-ui--nevent-cache)))
    (cond
     ((eq cached 'invalid) nil)
     (cached cached)
     (t
      (condition-case nil
          (let* ((decoded (nostr-nip19-decode-sync value))
                 (entity (alist-get 'entity decoded nil nil #'equal))
                 (event-id (and (equal entity "nevent")
                                (alist-get 'event_id decoded))))
            (if (stringp event-id)
                (puthash value event-id nostr-ui--nevent-cache)
              (puthash value 'invalid nostr-ui--nevent-cache)
              nil))
        (error
         (puthash value 'invalid nostr-ui--nevent-cache)
         nil))))))

(defun nostr-ui--event-by-id (event-id)
  "Return cached EVENT-ID as an event alist."
  (car (nostr-db-select-thread event-id)))

(defun nostr-ui--maybe-fetch-embedded-event (event-id)
  "Request EVENT-ID from relays when the relay module is loaded."
  (when (and (stringp event-id)
             (fboundp 'nostr-relay-fetch-events-by-id))
    (nostr-relay-fetch-events-by-id (list event-id) 1)))

(defun nostr-ui--embedded-event-summary (event)
  "Return one-line summary for embedded EVENT."
  (let* ((author (nostr-ui-format-author event))
         (content (string-trim (or (alist-get 'content event) "")))
         (content (replace-regexp-in-string "[\n\t ]+" " " content)))
    (string-trim
     (concat author
             (when (not (string-empty-p content))
               (concat " · " (truncate-string-to-width content 72 nil nil "…")))))))

(defun nostr-ui--insert-nevent-summaries (event indent)
  "Insert compact nested nevent summaries for EVENT at INDENT."
  (dolist (nevent (nostr-event-nevents (alist-get 'content event)))
    (let* ((event-id (nostr-ui--decode-nevent-event-id nevent))
           (embedded (and event-id (nostr-ui--event-by-id event-id))))
      (insert indent)
      (insert (propertize "  ↳ " 'face 'nostr-ui-meta))
      (insert (propertize
               (if embedded
                   (format "Quoted %s" (nostr-ui--embedded-event-summary embedded))
                 (format "Quoted %s" (nostr-ui--short-id
                                      (or event-id (nostr-ui--strip-nostr-uri nevent)))))
               'face 'nostr-ui-meta))
      (insert "\n")
      (unless embedded
        (nostr-ui--maybe-fetch-embedded-event event-id)))))

(defun nostr-ui--insert-nevent-embeds (event depth style embed-depth)
  "Insert embedded nevent cards for EVENT."
  (dolist (nevent (nostr-event-nevents (alist-get 'content event)))
    (let* ((event-id (nostr-ui--decode-nevent-event-id nevent))
           (embedded (and event-id (nostr-ui--event-by-id event-id)))
           (indent (make-string (* (1+ depth) 2) ?\s)))
      (cond
       (embedded
        (nostr-ui-insert-note
         embedded
         (list :depth (1+ depth)
               :style style
               :embed-depth (1+ embed-depth))))
       (event-id
        (insert indent)
        (insert (propertize
                 (format "↳ Quoted note %s is not cached yet.\n"
                         (nostr-ui--short-id event-id))
                 'face 'nostr-ui-meta))
        (nostr-ui--maybe-fetch-embedded-event event-id))
       (t
        (insert indent)
        (insert (propertize
                 (format "↳ Quoted %s could not be decoded.\n"
                         (nostr-ui--short-id
                          (nostr-ui--strip-nostr-uri nevent)))
                 'face 'nostr-ui-meta)))))))

(defun nostr-ui--insert-media-placeholder (item indent)
  "Insert media placeholder ITEM with INDENT."
  (let ((url (alist-get 'url item))
        (type (alist-get 'type item)))
    (insert indent)
    (insert (propertize "  " 'face 'nostr-ui-card-border))
    (insert-text-button
     (format "[%s: %s]" (if (eq type 'video) "video" "image") url)
     'follow-link t
     'nostr-media-url url
     'nostr-media-type type
     'help-echo (if (eq type 'video)
                    "Show an external video link"
                  "Load this image inline")
     'action (lambda (_button) (nostr-media-load-at-point)))
    (insert "\n")))

(defun nostr-ui--insert-note-heading (section indent picture author)
  "Insert note SECTION heading with INDENT, PICTURE, and AUTHOR."
  (insert indent)
  (insert (propertize "▾" 'nostr-ui-section section))
  (insert " ")
  (nostr-ui-insert-avatar picture nostr-ui-avatar-size)
  (insert " ")
  (insert (propertize (format "%s\n" author)
                      'face 'nostr-ui-author
                      'nostr-ui-section section)))

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
  (let ((overlay (nostr-ui-section-overlay section)))
    (cons (nostr-ui-section-content-start section)
          (if (overlayp overlay)
              (overlay-end overlay)
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

(defun nostr-ui--open-media-notes ()
  "Return the buffer-local note media state table."
  (unless (hash-table-p nostr-ui--open-media-notes)
    (setq nostr-ui--open-media-notes (make-hash-table :test #'equal)))
  nostr-ui--open-media-notes)

(defun nostr-ui--restore-note-media (section)
  "Restore open media previews for SECTION after a buffer refresh."
  (let ((explicit (gethash (nostr-ui-section-id section)
                           (nostr-ui--open-media-notes))))
    (when (or explicit nostr-media-auto-preview)
      (let* ((event (nostr-ui-section-data section))
             (urls (nostr-event-media-urls (alist-get 'content event)))
             (urls (if explicit
                       urls
                     (seq-take urls (max 0 nostr-media-auto-preview-max-per-note))))
             (bounds (nostr-ui--section-media-region section))
             (start (car bounds))
             (end (cdr bounds)))
        (unless (nostr-media-rendered-in-region-p start end)
          (dolist (url urls)
            (when-let* ((position (nostr-ui--media-placeholder-position url start end)))
              (save-excursion
                (goto-char position)
                ;; Refresh restores explicit media only from cache.  Network
                ;; downloads happen on direct user action, or through the
                ;; non-default `nostr-media-auto-preview' setting.
                (nostr-media-load-at-point (not nostr-media-auto-preview))))))))))

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
          (progn
            (remhash (nostr-ui-section-id section) (nostr-ui--open-media-notes))
            (message "Removed %d media preview%s"
                     (nostr-media-remove-rendered-in-region start end)
                     (if (= (length urls) 1) "" "s")))
        (puthash (nostr-ui-section-id section) t (nostr-ui--open-media-notes))
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
`:depth', `:style', and `:embed-depth'.  Feed style uses relative dates;
thread/detail style uses exact dates."
  (let* ((depth (nostr-ui--note-depth options))
         (style (nostr-ui--note-style options))
         (embed-depth (or (nostr-ui--note-option options :embed-depth 0) 0))
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
			   (when-let* ((reposter (nostr-ui-format-reposter event)))
			     (insert indent)
			     (insert (propertize (format "  ↻ Reposted by %s\n" reposter)
						 'face 'nostr-ui-meta)))
			   (nostr-ui--insert-filled-content content indent)
			   (nostr-ui--insert-publish-receipts event indent)
			   (dolist (item (nostr-event-media-items (alist-get 'content event)))
                             (nostr-ui--insert-media-placeholder item indent))
			   (nostr-ui--restore-note-media section)
                           (if (> embed-depth 0)
                               (nostr-ui--insert-nevent-summaries event indent)
                             (nostr-ui--insert-nevent-embeds event depth style embed-depth))
			   (nostr-ui-insert-badge-line (split-string (nostr-ui--note-footer event) "   ")
						       (concat indent "  "))
			   (insert "\n")
			   (insert "\n"))))

(provide 'nostr-ui)
;;; nostr-ui.el ends here
