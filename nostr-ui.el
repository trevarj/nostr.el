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
(defvar nostr-relay-publish-pending-stale-seconds)
(declare-function nostr-relay-retry-publish "nostr-relay" (event))

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
  '((t :inherit hl-line :extend nil))
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

(defvar nostr-ui--npub-decode-cache (make-hash-table :test #'equal)
  "Cache of npub strings to decoded hex pubkeys or `invalid'.")

(defvar nostr-ui--nip19-decode-cache (make-hash-table :test #'equal)
  "Cache of public NIP-19 strings to decoded backend alists or `invalid'.")

(declare-function nostr-relay-fetch-events-by-id "nostr-relay" (event-ids &optional limit))
(declare-function nostr-relay-fetch-addressable-event "nostr-relay"
                  (kind pubkey identifier &optional relays))
(declare-function nostr-relay-fetch-profile "nostr-relay" (pubkey &optional url))
(declare-function nostr-profile-open "nostr-profile" (pubkey))

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

(defcustom nostr-ui-show-global nil
  "Whether to show the old Global relay stream in primary navigation."
  :type 'boolean
  :group 'nostr)

(defconst nostr-ui-primary-nav-items
  '((feed "f" "Feed" nostr-timeline-feed)
    (conversations "C" "Conversations" nostr-timeline-conversations)
    (discover "d" "Discover" nostr-timeline-discover)
    (global "G" "Global" nostr-timeline-global)
    (my-posts "P" "My Posts" nostr-timeline-my-posts)
    (notifications "N" "Notifications" nostr-notifications-open))
  "Primary Nostr destinations as KIND, key, label, and command.")

(defun nostr-ui--visible-primary-nav-items (items)
  "Return ITEMS filtered for current navigation customization."
  (seq-filter (lambda (item)
                (or nostr-ui-show-global
                    (not (eq (car item) 'global))))
              items))

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
  (let ((start-sym (cl-gensym "start-"))
        (parent-sym (cl-gensym "parent-"))
        (section-sym (cl-gensym "section-"))
        (title-sym (cl-gensym "title-"))
        (overlay-sym (cl-gensym "overlay-")))
    `(let* ((,start-sym (point))
            (,parent-sym nostr-ui--current-parent)
            (,title-sym ,title)
            (,section-sym (make-nostr-ui-section :id ,id :type ,type
                                                 :start ,start-sym
                                                 :data ,data
                                                 :parent ,parent-sym)))
       (if (functionp ,title-sym)
           (funcall ,title-sym ,section-sym)
         (insert-text-button
          (format "▾ %s\n" ,title-sym)
          'follow-link t
          'nostr-ui-section ,section-sym
          'action (lambda (_button) (nostr-ui-toggle-section))))
       (setf (nostr-ui-section-content-start ,section-sym) (point))
       (let ((nostr-ui--current-parent ,section-sym))
         ,@body)
       (setf (nostr-ui-section-end ,section-sym) (point))
       (let ((,overlay-sym (make-overlay
                            (nostr-ui-section-content-start ,section-sym)
                            (nostr-ui-section-end ,section-sym))))
         (overlay-put ,overlay-sym 'nostr-ui-section t)
         (setf (nostr-ui-section-overlay ,section-sym) ,overlay-sym))
       (nostr-ui--apply-section-fold-state ,section-sym)
       (push ,section-sym nostr-ui--sections)
       ,section-sym)))

(defun nostr-ui--sections-by-start ()
  "Return `nostr-ui--sections' as a fresh list sorted by ascending start."
  (sort (copy-sequence nostr-ui--sections)
        (lambda (a b) (< (nostr-ui-section-start a)
                         (nostr-ui-section-start b)))))

(defun nostr-ui--top-level-sections-by-start ()
  "Return top-level sections sorted by ascending start.
Inline embedded cards still have sections so point-local commands work inside
them, but timeline navigation should move between primary cards only."
  (seq-filter (lambda (section)
                (null (nostr-ui-section-parent section)))
              (nostr-ui--sections-by-start)))

(defun nostr-ui-rendered-note-ids ()
  "Return top-level rendered note event ids in display order."
  (delete-dups
   (seq-filter
    #'stringp
    (mapcar (lambda (section)
              (when (eq (nostr-ui-section-type section) 'note)
                (nostr-ui-section-id section)))
            (nostr-ui--top-level-sections-by-start)))))

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

(defun nostr-ui-activate-button-at-point ()
  "Activate a text button at point, returning non-nil when one was handled."
  (when-let* ((button (or (button-at (point))
                          (button-at (max (point-min) (1- (point)))))))
    (button-activate button)
    t))

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
                       (nostr-ui--top-level-sections-by-start))))
      (goto-char (nostr-ui-section-start next))
      (nostr-ui-update-selection))))

(defun nostr-ui-prev-section ()
  "Move point to the previous section."
  (interactive)
  (let ((pos (point))
        best)
    (dolist (section (nostr-ui--top-level-sections-by-start))
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
  (when-let* ((first (car (nostr-ui--top-level-sections-by-start))))
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
      (not (null (nostr-ui-section-at-point))))))

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
  (dolist (item (nostr-ui--visible-primary-nav-items items))
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

(defun nostr-ui-load-avatar-at-point (&optional button force)
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
           (message "[nostr] %s" message))
         (or force (called-interactively-p 'interactive)))))
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
               'action (lambda (button) (nostr-ui-load-avatar-at-point button t)))
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

(defun nostr-ui--publish-receipt-counts (receipts)
  "Return state counts for publish RECEIPTS."
  (let (counts)
    (dolist (receipt receipts)
      (let ((state (alist-get 'state receipt)))
        (setf (alist-get state counts nil nil #'equal)
              (1+ (or (alist-get state counts nil nil #'equal) 0)))))
    counts))

(defun nostr-ui--stale-pending-receipts-p (receipts)
  "Return non-nil when RECEIPTS include stale pending publish state."
  (let ((now (truncate (float-time))))
    (seq-some (lambda (receipt)
                (and (equal (alist-get 'state receipt) "pending")
                     (> (- now (or (alist-get 'updated-at receipt) 0))
                        (or (and (boundp 'nostr-relay-publish-pending-stale-seconds)
                                 nostr-relay-publish-pending-stale-seconds)
                            60))))
              receipts)))

(defun nostr-ui--publish-receipt-chips (event-id)
  "Return compact publish receipt chips for EVENT-ID, when available."
  (when (and (boundp 'nostr-db--connection) nostr-db--connection event-id)
    (let* ((receipts (nostr-db-select-publish-receipts event-id))
           (counts (nostr-ui--publish-receipt-counts receipts))
           (accepted (alist-get "accepted" counts nil nil #'equal))
           (rejected (alist-get "rejected" counts nil nil #'equal))
           (pending (alist-get "pending" counts nil nil #'equal))
           (stale-pending (nostr-ui--stale-pending-receipts-p receipts)))
      (when receipts
        (delq nil
              (list (when accepted (format "✓ %d" accepted))
                    (when rejected (format "! %d" rejected))
                    (when (and pending (or (not accepted) stale-pending))
                      (format "… %d" pending))))))))

(defun nostr-ui--publish-footer-chips (event)
  "Return publish receipt chips for EVENT's footer."
  (when (or (not (bound-and-true-p nostr-current-pubkey))
            (equal (alist-get 'pubkey event) nostr-current-pubkey))
    (when-let* ((event-id (alist-get 'id event))
                (chips (nostr-ui--publish-receipt-chips event-id)))
      chips)))

(defun nostr-ui--publish-details-text (event receipts)
  "Return details text for EVENT publish RECEIPTS."
  (let ((event-id (alist-get 'id event)))
    (concat
     (format "Publish details for %s\n\n" event-id)
     (if receipts
         (string-join
          (mapcar (lambda (receipt)
                    (format "%-8s %s  %s%s"
                            (alist-get 'state receipt)
                            (format-time-string
                             "%Y-%m-%d %H:%M:%S"
                             (seconds-to-time (or (alist-get 'updated-at receipt) 0)))
                            (alist-get 'url receipt)
                            (if-let* ((message (alist-get 'message receipt)))
                                (format "\n         %s" message)
                              "")))
                  receipts)
          "\n")
       "No publish receipts for this note")
     "\n")))

(defun nostr-ui-retry-publish (event)
  "Retry publishing EVENT to failed or stale relay targets."
  (interactive (list (or (nostr-ui-selected-data)
                         (user-error "No note selected"))))
  (require 'nostr-relay)
  (let ((sent (nostr-relay-retry-publish event)))
    (message "Re-published to %d relay%s"
             sent
             (if (= sent 1) "" "s"))))

(defun nostr-ui-show-publish-details ()
  "Show publish receipt details for the selected note."
  (interactive)
  (let* ((event (or (nostr-ui-selected-data)
                    (user-error "No note selected")))
         (event-id (or (alist-get 'id event)
                       (user-error "Selected note has no event id")))
         (receipts (and nostr-db--connection
                        (nostr-db-select-publish-receipts event-id)))
         (buffer (get-buffer-create "*Nostr Publish Details*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (nostr-ui--publish-details-text event receipts))
        (when (equal (alist-get 'pubkey event) nostr-current-pubkey)
          (insert "\n")
          (insert-text-button
           "[Retry failed/stale relays]"
           'follow-link t
           'help-echo "Re-publish this event to failed or stale relays"
           'action (lambda (_button)
                     (nostr-ui-retry-publish event))))))
    (display-buffer buffer)))

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
A count carried inline on EVENT is treated as a floor; database counts may be
newer for normal cached feeds, while provider-ranked feeds such as Discover
may carry richer inline stats."
  (max (or (cdr (assoc key event)) 0)
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
         (parts (append
                 (list
                  (format "↩ %s" (nostr-ui--event-count event 'replies))
                  (format "♥ %s" (nostr-ui--event-count event 'reactions))
                  (format "↻ %s" (nostr-ui--event-count event 'reposts))
                  (if (> sats 0)
                      (format "⚡ %s (%s sats)" zaps sats)
                    (format "⚡ %s" zaps)))
                 (nostr-ui--publish-footer-chips event))))
    (string-join parts "   ")))

(defun nostr-ui--insert-note-footer (event indent)
  "Insert EVENT's stats footer with metadata for targeted refresh."
  (let ((start (point))
        (event-id (alist-get 'id event)))
    (nostr-ui-insert-badge-line (split-string (nostr-ui--note-footer event) "   ")
                                 indent)
    (add-text-properties
     start (point)
     `(nostr-ui-note-footer ,event-id
       nostr-ui-note-footer-indent ,indent
       rear-nonsticky t))))

(defun nostr-ui--shift-section-position (value pivot delta)
  "Return VALUE shifted by DELTA when it is after PIVOT."
  (if (and (integerp value) (>= value pivot))
      (+ value delta)
    value))

(defun nostr-ui--note-footer-position (event-id start end)
  "Return footer position for EVENT-ID between START and END."
  (let (pos found)
    (setq pos start)
    (while (and (not found) (< pos end))
      (when (equal (get-text-property pos 'nostr-ui-note-footer) event-id)
        (setq found pos))
      (setq pos (next-single-property-change
                 pos 'nostr-ui-note-footer nil end)))
    found))

(defun nostr-ui--refresh-section-note-footer (section)
  "Refresh SECTION's note footer from current cached counts."
  (when (and (eq (nostr-ui-section-type section) 'note)
             (nostr-ui-section-id section))
    (let* ((event (nostr-ui-section-data section))
           (event-id (nostr-ui-section-id section))
           (start (nostr-ui-section-content-start section))
           (end (nostr-ui-section-end section))
           (pos (and start end
                     (nostr-ui--note-footer-position event-id start end))))
      (when pos
        (let* ((indent (or (get-text-property pos 'nostr-ui-note-footer-indent)
                           ""))
               (old-start (save-excursion
                            (goto-char pos)
                            (line-beginning-position)))
               (old-end (save-excursion
                          (goto-char pos)
                          (line-beginning-position 2)))
               (new-text (with-temp-buffer
                           (nostr-ui--insert-note-footer event indent)
                           (buffer-string)))
               (old-len (- old-end old-start))
               (new-len (length new-text))
               (delta (- new-len old-len))
               (inhibit-read-only t))
          (delete-region old-start old-end)
          (goto-char old-start)
          (insert new-text)
          (when (/= delta 0)
            (dolist (candidate nostr-ui--sections)
              (setf (nostr-ui-section-start candidate)
                    (nostr-ui--shift-section-position
                     (nostr-ui-section-start candidate) old-end delta))
              (setf (nostr-ui-section-content-start candidate)
                    (nostr-ui--shift-section-position
                     (nostr-ui-section-content-start candidate) old-end delta))
              (setf (nostr-ui-section-end candidate)
                    (nostr-ui--shift-section-position
                     (nostr-ui-section-end candidate) old-end delta)))
            (when-let* ((overlay (nostr-ui-section-overlay section)))
              (move-overlay overlay
                            (nostr-ui-section-content-start section)
                            (nostr-ui-section-end section))))
          t)))))

(defun nostr-ui-refresh-note-counts (event-id)
  "Refresh visible note-count footers for EVENT-ID across Nostr buffers."
  (when (stringp event-id)
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when nostr-ui--sections
            (let ((position-state (nostr-ui-capture-position)))
              (when (hash-table-p nostr-ui--event-counts-cache)
                (remhash event-id nostr-ui--event-counts-cache))
              (dolist (section nostr-ui--sections)
                (when (equal (nostr-ui-section-id section) event-id)
                  (nostr-ui--refresh-section-note-footer section)))
              (nostr-ui-restore-position position-state))))))))

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

(defun nostr-ui--strip-profile-reference (value)
  "Return VALUE without profile mention prefixes."
  (let ((text (nostr-ui--strip-nostr-uri value)))
    (if (and (stringp text)
             (string-prefix-p "@" text))
        (substring text 1)
      text)))

(defun nostr-ui--strip-public-identifier (value)
  "Return VALUE without mention or NIP-21 URI prefixes."
  (let ((text value))
    (when (and (stringp text) (string-prefix-p "@" text))
      (setq text (substring text 1)))
    (nostr-ui--strip-nostr-uri text)))

(defun nostr-ui--short-id (value)
  "Return shortened display text for VALUE."
  (if (and (stringp value) (> (length value) 16))
      (concat (substring value 0 8) "…" (substring value -8))
    (or value "")))

(defun nostr-ui--decode-public-identifier (value)
  "Return decoded public NIP-19 VALUE, caching backend results."
  (let* ((identifier (nostr-ui--strip-public-identifier value))
         (cached (gethash identifier nostr-ui--nip19-decode-cache)))
    (cond
     ((eq cached 'invalid) nil)
     (cached cached)
     (t
      (condition-case nil
          (let ((decoded (nostr-nip19-decode-sync identifier)))
            (if (member (alist-get 'entity decoded nil nil #'equal)
                        '("npub" "nprofile" "note" "nevent" "naddr"))
                (puthash identifier decoded nostr-ui--nip19-decode-cache)
              (puthash identifier 'invalid nostr-ui--nip19-decode-cache)
              nil))
        (error
         (puthash identifier 'invalid nostr-ui--nip19-decode-cache)
         nil))))))

(defun nostr-ui--decode-event-reference-id (value)
  "Return event id decoded from public event identifier VALUE."
  (let* ((identifier (nostr-ui--strip-public-identifier value))
         (cached (and (string-prefix-p "nevent1" identifier)
                      (gethash identifier nostr-ui--nevent-cache))))
    (cond
     ((eq cached 'invalid) nil)
     (cached cached)
     (t
      (let* ((decoded (nostr-ui--decode-public-identifier identifier))
             (entity (alist-get 'entity decoded nil nil #'equal))
             (event-id (and (member entity '("note" "nevent"))
                            (alist-get 'event_id decoded))))
        (when (string-prefix-p "nevent1" identifier)
          (puthash identifier (or event-id 'invalid) nostr-ui--nevent-cache))
        event-id)))))

(defun nostr-ui--decode-nevent-event-id (nevent)
  "Return event id decoded from NEVENT, caching the backend result."
  (nostr-ui--decode-event-reference-id nevent))

(defun nostr-ui--decode-profile-pubkey (value)
  "Return pubkey decoded from NPUB or NPROFILE."
  (let* ((identifier (nostr-ui--strip-profile-reference value))
         (cached (and (string-prefix-p "npub1" identifier)
                      (gethash identifier nostr-ui--npub-decode-cache))))
    (cond
     ((eq cached 'invalid) nil)
     (cached cached)
     (t
      (let* ((decoded (nostr-ui--decode-public-identifier identifier))
             (entity (alist-get 'entity decoded nil nil #'equal))
             (pubkey (and (member entity '("npub" "nprofile"))
                          (alist-get 'pubkey decoded))))
        (when (string-prefix-p "npub1" identifier)
          (puthash identifier (or pubkey 'invalid) nostr-ui--npub-decode-cache))
        pubkey)))))

(defun nostr-ui--decode-npub-pubkey (npub)
  "Return pubkey decoded from NPUB, caching the backend result."
  (nostr-ui--decode-profile-pubkey npub))

(defun nostr-ui--profile-mention-data (reference)
  "Return display data for a profile REFERENCE."
  (let* ((decoded (nostr-ui--decode-public-identifier reference))
         (pubkey (or (and (member (alist-get 'entity decoded nil nil #'equal)
                                 '("npub" "nprofile"))
                          (alist-get 'pubkey decoded))
                     (nostr-ui--decode-profile-pubkey reference)))
         (relays (alist-get 'relays decoded))
         (profile (and pubkey (nostr-ui--cached-profile pubkey)))
         (name (nostr-ui--string-value
                (nth 2 profile)
                (nth 1 profile)
                (nostr-ui-format-nip05 (nth 5 profile) pubkey))))
    (when (and pubkey
               (not profile)
               (bound-and-true-p nostr-db--connection)
               (fboundp 'nostr-relay-fetch-profile))
      (nostr-relay-fetch-profile pubkey (car relays)))
    `((label . ,(cond
                 (name (concat "@" name))
                 (pubkey (format "@%s" (nostr-ui--shorten-identifier
                                        (or (nostr-ui--cached-npub pubkey)
                                            (nostr-ui--strip-profile-reference reference)))))
                 (t reference)))
      (pubkey . ,pubkey))))

(defun nostr-ui--format-profile-mention (reference)
  "Return compact mention text for profile REFERENCE."
  (alist-get 'label (nostr-ui--profile-mention-data reference)))

(defun nostr-ui--insert-profile-mention (reference)
  "Insert REFERENCE as a clickable profile mention when it decodes."
  (let* ((data (nostr-ui--profile-mention-data reference))
         (label (alist-get 'label data))
         (pubkey (alist-get 'pubkey data)))
    (if pubkey
        (insert-text-button
         label
         'follow-link t
         'face 'button
         'nostr-profile-pubkey pubkey
         'help-echo "Open profile"
         'action (lambda (button)
                   (unless (fboundp 'nostr-profile-open)
                     (require 'nostr-profile))
                   (nostr-profile-open (button-get button 'nostr-profile-pubkey))))
      (insert (propertize label 'face 'nostr-ui-content)))))

(defconst nostr-ui-profile-mention-regexp
  (concat "\\(?:@\\)?\\(?:" nostr-event-npub-regexp
          "\\|" nostr-event-nprofile-regexp "\\)")
  "Regexp matching profile references rendered as profile mentions.")

(defun nostr-ui--event-reference-label (reference)
  "Return compact label for an event REFERENCE."
  (let* ((event-id (nostr-ui--decode-event-reference-id reference))
         (event (and event-id (nostr-ui--event-by-id event-id))))
    (if event
        (format "note %s" (nostr-ui--short-id event-id))
      (format "note %s" (nostr-ui--short-id
                         (or event-id
                             (nostr-ui--strip-public-identifier reference)))))))

(defun nostr-ui--insert-event-reference (reference)
  "Insert REFERENCE as a clickable note link."
  (let ((event-id (nostr-ui--decode-event-reference-id reference))
        (label (nostr-ui--event-reference-label reference)))
    (when (and event-id
               (not (nostr-ui--event-by-id event-id)))
      (nostr-ui--maybe-fetch-embedded-event event-id))
    (insert-text-button
     label
     'follow-link t
     'face 'button
     'nostr-identifier (nostr-ui--strip-public-identifier reference)
     'help-echo "Open note"
     'action (lambda (button)
               (require 'nostr-dispatch)
               (nostr-open-identifier
                (button-get button 'nostr-identifier))))))

(defun nostr-ui--address-reference-event (decoded)
  "Return cached event matching decoded NADDR data."
  (when (and (equal (alist-get 'entity decoded nil nil #'equal) "naddr")
             (bound-and-true-p nostr-db--connection))
    (nostr-db-select-addressable-event
     (alist-get 'kind decoded)
     (alist-get 'pubkey decoded)
     (alist-get 'identifier decoded))))

(defun nostr-ui--address-reference-label (reference)
  "Return compact label for NADDR REFERENCE."
  (let* ((decoded (nostr-ui--decode-public-identifier reference))
         (event (nostr-ui--address-reference-event decoded)))
    (cond
     (event (format "address %s" (nostr-ui--embedded-event-summary event)))
     (decoded
      (format "kind %s by %s"
              (alist-get 'kind decoded)
              (nostr-ui--short-id (alist-get 'pubkey decoded))))
     (t (nostr-ui--short-id (nostr-ui--strip-public-identifier reference))))))

(defun nostr-ui--insert-address-reference (reference)
  "Insert NADDR REFERENCE as a clickable addressable event link."
  (let* ((decoded (nostr-ui--decode-public-identifier reference))
         (label (nostr-ui--address-reference-label reference)))
    (unless (nostr-ui--address-reference-event decoded)
      (when (and (fboundp 'nostr-relay-fetch-addressable-event)
                 (alist-get 'kind decoded)
                 (alist-get 'pubkey decoded))
        (nostr-relay-fetch-addressable-event
         (alist-get 'kind decoded)
         (alist-get 'pubkey decoded)
         (alist-get 'identifier decoded)
         (alist-get 'relays decoded))))
    (insert-text-button
     label
     'follow-link t
     'face 'button
     'nostr-identifier (nostr-ui--strip-public-identifier reference)
     'help-echo "Open addressable event"
     'action (lambda (button)
               (require 'nostr-dispatch)
               (nostr-open-identifier
                (button-get button 'nostr-identifier))))))

(defun nostr-ui--insert-public-identifier (reference)
  "Insert public NIP-19 REFERENCE with a friendly representation."
  (let* ((decoded (nostr-ui--decode-public-identifier reference))
         (entity (alist-get 'entity decoded nil nil #'equal)))
    (pcase entity
      ((or "npub" "nprofile") (nostr-ui--insert-profile-mention reference))
      ((or "note" "nevent") (nostr-ui--insert-event-reference reference))
      ("naddr" (nostr-ui--insert-address-reference reference))
      (_ (insert (propertize reference 'face 'nostr-ui-content))))))

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

(defun nostr-ui--insert-event-reference-summaries (event indent)
  "Insert compact nested event reference summaries for EVENT at INDENT."
  (dolist (reference (nostr-event-embedded-event-identifiers
                      (alist-get 'content event)))
    (let* ((event-id (nostr-ui--decode-event-reference-id reference))
           (embedded (and event-id (nostr-ui--event-by-id event-id))))
      (insert indent)
      (insert (propertize "  ↳ " 'face 'nostr-ui-meta))
      (insert (propertize
               (if embedded
                   (format "Quoted %s" (nostr-ui--embedded-event-summary embedded))
                 (format "Quoted %s" (nostr-ui--short-id
                                      (or event-id
                                          (nostr-ui--strip-public-identifier reference)))))
               'face 'nostr-ui-meta))
      (insert "\n")
      (unless embedded
        (nostr-ui--maybe-fetch-embedded-event event-id)))))

(defun nostr-ui--insert-nevent-summaries (event indent)
  "Insert compact nested event summaries for EVENT at INDENT."
  (nostr-ui--insert-event-reference-summaries event indent))

(defun nostr-ui--represented-event-identifiers (event)
  "Return event refs from EVENT that already have cached cards."
  (let (represented)
    (dolist (reference (nostr-event-embedded-event-identifiers
                        (alist-get 'content event)))
      ;; Only hide raw identifiers once the card can render the target.
      (when-let* ((event-id (nostr-ui--decode-event-reference-id reference)))
        (when (nostr-ui--event-by-id event-id)
          (push reference represented))))
    (nreverse represented)))

(defun nostr-ui--represented-nevents (event)
  "Return event refs from EVENT that already have cached cards."
  (nostr-ui--represented-event-identifiers event))

(defun nostr-ui--content-without-event-identifiers (content identifiers)
  "Return CONTENT with represented IDENTIFIERS removed."
  (let ((text (or content "")))
    (dolist (identifier identifiers)
      (setq text (replace-regexp-in-string
                  (regexp-quote identifier) "" text t t)))
    (string-join
     (mapcar (lambda (line)
               (string-trim (replace-regexp-in-string "[ \t]+" " " line)))
             (split-string text "\n"))
     "\n")))

(defun nostr-ui--content-without-nevents (content nevents)
  "Return CONTENT with represented NEVENTS removed."
  (nostr-ui--content-without-event-identifiers content nevents))

(defun nostr-ui--insert-event-reference-embeds (event depth style embed-depth)
  "Insert embedded event reference cards for EVENT."
  (dolist (reference (nostr-event-embedded-event-identifiers
                      (alist-get 'content event)))
    (let* ((event-id (nostr-ui--decode-event-reference-id reference))
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
                          (nostr-ui--strip-public-identifier reference)))
                 'face 'nostr-ui-meta)))))))

(defun nostr-ui--insert-nevent-embeds (event depth style embed-depth)
  "Insert embedded event reference cards for EVENT."
  (nostr-ui--insert-event-reference-embeds event depth style embed-depth))

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
                    "Play this video"
                  "Load this image inline")
     'action (lambda (button)
               (if (eq (button-get button 'nostr-media-type) 'video)
                   (nostr-media-play-video-at-point button)
                 (save-excursion
                   ;; Button actions may run with point elsewhere, especially
                   ;; when activated by mouse or tests.
                   (goto-char (button-start button))
                   (nostr-media-load-at-point nil t)))))
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
          (nostr-ui--insert-content-with-profile-mentions
           (string-trim paragraph))
          (fill-region-as-paragraph start (point))
          (insert "\n"))))))

(defun nostr-ui--insert-content-with-profile-mentions (content)
  "Insert CONTENT, rendering public NIP-19 references as buttons."
  (let ((start 0)
        (case-fold-search nil))
    (while (string-match nostr-event-public-identifier-regexp content start)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0))
            (reference (match-string 0 content)))
        (when (> match-start start)
          (insert (propertize (substring content start match-start)
                              'face 'nostr-ui-content)))
        (nostr-ui--insert-public-identifier reference)
        (setq start match-end)))
    (when (< start (length content))
      (insert (propertize (substring content start)
                          'face 'nostr-ui-content)))))

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

(defun nostr-ui--media-item-urls (items type)
  "Return URL values from ITEMS matching media TYPE."
  (delq nil
        (mapcar (lambda (item)
                  (when (eq (alist-get 'type item) type)
                    (alist-get 'url item)))
                items)))

(defun nostr-ui--choose-video-url (urls)
  "Return the video URL to play from URLS."
  (cond
   ((null urls) nil)
   ((null (cdr urls)) (car urls))
   (t (completing-read "Play video: " urls nil t))))

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
             (items (nostr-event-media-items (alist-get 'content event)))
             (urls (nostr-ui--media-item-urls items 'image))
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
  "Toggle selected note image previews and play video media."
  (interactive)
  (let* ((section (or (nostr-ui-section-at-point)
                      (user-error "No note selected")))
         (event (nostr-ui-section-data section)))
    (unless (eq (nostr-ui-section-type section) 'note)
      (user-error "No note selected"))
    (let* ((items (nostr-event-media-items (alist-get 'content event)))
           (image-urls (nostr-ui--media-item-urls items 'image))
           (video-urls (nostr-ui--media-item-urls items 'video))
           (video-url (nostr-ui--choose-video-url video-urls))
           (bounds (nostr-ui--section-media-region section))
           (start (car bounds))
           (end (cdr bounds)))
      (unless items
        (user-error "Selected note has no supported media"))
      (when video-url
        (nostr-media-play-video-url video-url))
      (cond
       ((not image-urls)
        (message "Playing video"))
       ((nostr-media-rendered-in-region-p start end)
        (remhash (nostr-ui-section-id section) (nostr-ui--open-media-notes))
        (message "Removed %d media preview%s"
                 (nostr-media-remove-rendered-in-region start end)
                 (if (= (length image-urls) 1) "" "s")))
       (t
        (puthash (nostr-ui-section-id section) t (nostr-ui--open-media-notes))
        (dolist (url image-urls)
          (when-let* ((position (nostr-ui--media-placeholder-position url start end)))
            (save-excursion
              (goto-char position)
              (nostr-media-load-at-point nil t))))
        (message "Loading %d media preview%s"
                 (length image-urls)
                 (if (= (length image-urls) 1) "" "s")))))))

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
         (represented-identifiers (nostr-ui--represented-event-identifiers event))
         (display-content (nostr-ui--content-without-event-identifiers
                           content represented-identifiers))
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
      (nostr-ui--insert-filled-content display-content indent)
      (dolist (item (nostr-event-media-items (alist-get 'content event)))
        (nostr-ui--insert-media-placeholder item indent))
      (nostr-ui--restore-note-media nostr-ui--current-parent)
      (if (> embed-depth 0)
          (nostr-ui--insert-event-reference-summaries event indent)
        (nostr-ui--insert-event-reference-embeds event depth style embed-depth))
      (nostr-ui--insert-note-footer event (concat indent "  "))
      (insert "\n")
      (insert "\n"))))

(provide 'nostr-ui)
;;; nostr-ui.el ends here
