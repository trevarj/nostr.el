;;; nostr-search.el --- Nostr search buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Search view backed by local cache, with a relay/NIP-50 command stub.

;;; Code:

(require 'emacsql)
(require 'subr-x)
(require 'nostr-db)
(require 'nostr-nip)
(require 'nostr-profile)
(require 'nostr-relay)
(require 'nostr-share)
(require 'nostr-thread)
(require 'nostr-ui)
(require 'transient)

(defvar-local nostr-search-query nil
  "Search query displayed by the current search buffer.")

(defvar nostr-search-limit 100
  "Maximum number of local search results displayed.")

(defvar nostr-search--refresh-timer nil
  "Pending timer for visible search-result refreshes.")

(defvar nostr--refresh-dirty)

(defcustom nostr-search-auto-relay t
  "Whether opening a search buffer immediately queries connected relays."
  :type 'boolean
  :group 'nostr)

(defvar nostr-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-search-refresh)
    (define-key map (kbd "n") #'nostr-ui-next-section)
    (define-key map (kbd "p") #'nostr-ui-prev-section)
    (define-key map (kbd "TAB") #'nostr-ui-toggle-section)
    (define-key map (kbd "RET") #'nostr-search-open-at-point)
    (define-key map (kbd "i") #'nostr-profile-open-self)
    (define-key map (kbd "s") #'nostr-search-relay)
    (define-key map (kbd "m") #'nostr-ui-toggle-note-media)
    (define-key map (kbd "w") #'nostr-share-copy)
    (define-key map (kbd "y") #'nostr-share-copy-raw-id)
    (define-key map (kbd "b") #'nostr-share-browse)
    (define-key map (kbd "?") #'nostr-search-actions)
    map)
  "Keymap for `nostr-search-mode'.")

(define-derived-mode nostr-search-mode special-mode "Nostr-Search"
  "Mode for Nostr search buffers."
  (setq-local truncate-lines nil))

(transient-define-prefix nostr-search-actions ()
  "Actions for Nostr search results."
  [["Search"
    ("g" "Refresh local results" nostr-search-refresh)
    ("i" "My profile" nostr-profile-open-self)
    ("s" "Search relays" nostr-search-relay)]
   ["Result"
    ("RET" "Open result" nostr-search-open-at-point)
    ("m" "Toggle media" nostr-ui-toggle-note-media)
    ("n" "Next result" nostr-ui-next-section)
    ("p" "Previous result" nostr-ui-prev-section)
    ("TAB" "Toggle result" nostr-ui-toggle-section)]
   ["Share"
    ("w" "Copy NIP-19" nostr-share-copy)
    ("y" "Copy raw id" nostr-share-copy-raw-id)
    ("b" "Browse" nostr-share-browse)]])

(defun nostr-search--like-pattern (query)
  "Return a SQL LIKE pattern for QUERY."
  (concat "%" query "%"))

(defun nostr-search--profile-query (query)
  "Return QUERY normalized for cached profile field matching."
  (string-remove-prefix "@" (string-trim query)))

(defun nostr-search--hex-pubkey-p (query)
  "Return non-nil when QUERY is a raw hex pubkey."
  (string-match-p "\\`[0-9a-fA-F]\\{64\\}\\'" query))

(defun nostr-search--query-pubkey (query)
  "Return QUERY as a hex pubkey when it names an author."
  (cond
   ((nostr-search--hex-pubkey-p query)
    (downcase query))
   ((string-prefix-p "npub1" query)
    (ignore-errors
      (let ((decoded (nostr-nip19-decode-sync query)))
        (when (equal (alist-get 'entity decoded nil nil #'equal) "npub")
          (alist-get 'pubkey decoded)))))
   (t nil)))

(defun nostr-search--select-local (query &optional limit)
  "Return local note results matching QUERY."
  (let* ((pubkey (nostr-search--query-pubkey query))
         (profile-query (nostr-search--profile-query query)))
    (mapcar #'nostr-db--event-row-to-alist
            (emacsql nostr-db--connection
                     [:select
                      [events:id events:pubkey events:created_at events:kind events:tags
                                 events:content events:sig events:relay events:root_id events:reply_id
                                 events:quote_id profiles:name profiles:display_name profiles:picture]
                      :from events
                      :left-join profiles :on (= events:pubkey profiles:pubkey)
                      :where (and (= events:kind 1)
                                  (or (like events:content $s1)
                                      (= events:id $s2)
                                      (= events:pubkey $s2)
                                      (= events:pubkey $s3)
                                      (like profiles:name $s4)
                                      (like profiles:display_name $s4)
                                      (like profiles:nip05 $s4)))
                      :order-by [(desc events:created_at)]
                      :limit $s5]
                     (nostr-search--like-pattern query)
                     query
                     pubkey
                     (nostr-search--like-pattern profile-query)
                      (or limit nostr-search-limit)))))

(defun nostr-search--profile-row-to-alist (row)
  "Convert profile ROW into an alist."
  (pcase row
    (`(,pubkey ,name ,display-name ,about ,picture ,nip05 ,lud16 ,updated-at)
     `((pubkey . ,pubkey)
       (name . ,(and (stringp name) name))
       (display-name . ,(and (stringp display-name) display-name))
       (about . ,(and (stringp about) about))
       (picture . ,(and (stringp picture) picture))
       (nip05 . ,(and (stringp nip05) nip05))
       (lud16 . ,(and (stringp lud16) lud16))
       (updated-at . ,updated-at)))))

(defun nostr-search--profile-display-name (profile)
  "Return display name for PROFILE."
  (or (alist-get 'display-name profile)
      (alist-get 'name profile)
      (alist-get 'nip05 profile)
      (alist-get 'pubkey profile)))

(defun nostr-search--select-profiles (query &optional limit)
  "Return cached profile results matching QUERY."
  (let* ((pubkey (nostr-search--query-pubkey query))
         (profile-query (nostr-search--profile-query query))
         (pattern (nostr-search--like-pattern profile-query)))
    (mapcar #'nostr-search--profile-row-to-alist
            (emacsql nostr-db--connection
                     [:select [pubkey name display_name about picture nip05 lud16 updated_at]
                              :from profiles
                              :where (or (= pubkey $s1)
                                         (= pubkey $s2)
                                         (like name $s3)
                                         (like display_name $s3)
                                         (like nip05 $s3))
                              :order-by [(desc updated_at)
                                         (asc display_name)
                                         (asc name)
                                         (asc nip05)]
                              :limit $s4]
                     query
                     pubkey
                     pattern
                     (or limit nostr-search-limit)))))

(defun nostr-search--insert-profile (profile)
  "Insert PROFILE as a compact search result row."
  (let ((pubkey (alist-get 'pubkey profile))
        (name (nostr-search--profile-display-name profile)))
    (nostr-ui-with-section 'profile pubkey profile
        (format "Profile: %s" name)
      (nostr-ui-insert-avatar (alist-get 'picture profile) nostr-ui-avatar-size)
      (insert "\n")
      (when-let* ((nip05 (alist-get 'nip05 profile)))
        (insert (propertize "  NIP-05 " 'face 'nostr-ui-meta))
        (insert (nostr-ui-format-nip05 nip05 pubkey))
        (insert "\n"))
      (when-let* ((about (alist-get 'about profile)))
        (insert (propertize "  About  " 'face 'nostr-ui-meta))
        (insert (truncate-string-to-width
                 (replace-regexp-in-string "[\n\r]+" " " about)
                 120 nil nil t))
        (insert "\n"))
      (insert (propertize "  Pubkey " 'face 'nostr-ui-meta))
      (insert (format "%s\n\n" pubkey)))))

(defun nostr-search-refresh ()
  "Refresh the current search buffer."
  (interactive)
  (unless nostr-search-query
    (user-error "No search query is associated with this buffer"))
  (let* ((inhibit-read-only t)
         (position-state (nostr-ui-capture-position))
         (profiles (nostr-search--select-profiles nostr-search-query))
         (results (nostr-search--select-local nostr-search-query))
         (total (+ (length profiles) (length results))))
    ;; Eager avatars for every author in the search results.
    (nostr-relay-fetch-profiles-batch
     (mapcar (lambda (event) (alist-get 'pubkey event)) results))
    (let ((ids (mapcar (lambda (event) (alist-get 'id event)) results)))
      (nostr-relay-fetch-event-metadata ids))
    (nostr-ui-clear)
    (nostr-ui-insert-status-header
     "Search"
     nostr-search-query
     (format "%d cached result%s (%d profile%s, %d note%s)"
             total
             (if (= total 1) "" "s")
             (length profiles)
             (if (= (length profiles) 1) "" "s")
             (length results)
             (if (= (length results) 1) "" "s")))
    (if (> total 0)
        (progn
          (dolist (profile profiles)
            (nostr-search--insert-profile profile))
          (dolist (event results)
            (nostr-ui-insert-note event)))
      (nostr-ui-insert-empty-state
       "No cached search results."
       "Use s to query connected relays."))
    (nostr-ui-insert-footer
     '("g refresh" "s relay search" "RET open" "w copy" "b browse" "? actions"))
    (nostr-ui-finish-refresh position-state)
    (nostr-relay-sync-visible-reactions)))

(defun nostr-search-open-at-point ()
  "Open the selected search result."
  (interactive)
  (when-let* ((section (nostr-ui-section-at-point))
              (data (nostr-ui-section-data section)))
    (pcase (nostr-ui-section-type section)
      ('profile (nostr-profile-open (alist-get 'pubkey data)))
      ('note (nostr-thread-open data)))))

(defun nostr-search-refresh-visible-buffers ()
  "Refresh visible search buffers after relay-backed results arrive.
Hidden search buffers are marked dirty for the shared window-change
refresh path."
  (setq nostr-search--refresh-timer nil)
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq major-mode 'nostr-search-mode)
          (if (get-buffer-window buffer 'visible)
              (progn
                (setq nostr--refresh-dirty nil)
                (nostr-search-refresh))
            (setq nostr--refresh-dirty t)))))))

(defun nostr-search--schedule-refresh (&rest _ignored)
  "Schedule a debounced refresh for visible search buffers."
  (unless (timerp nostr-search--refresh-timer)
    (setq nostr-search--refresh-timer
          (run-at-time 0.4 nil #'nostr-search-refresh-visible-buffers))))

(defun nostr-search--ensure-refresh-hook ()
  "Ensure relay ingestion refreshes visible search buffers."
  (add-hook 'nostr-relay-event-hook #'nostr-search--schedule-refresh))

(defun nostr-search-relay ()
  "Start relay-backed search for the current query."
  (interactive)
  (unless nostr-search-query
    (user-error "No search query is associated with this buffer"))
  (nostr-search--ensure-refresh-hook)
  (if-let* ((pubkey (nostr-search--query-pubkey nostr-search-query)))
      (let ((sub-id (nostr-relay-fetch-author pubkey nostr-search-limit)))
        (message "Nostr author lookup started: %s" sub-id))
    (let ((sub-id (nostr-relay-search nostr-search-query nostr-search-limit)))
      (message "Nostr relay search started: %s" sub-id))))

;;;###autoload
(defun nostr-search-open (query)
  "Open a search buffer for QUERY."
  (interactive "sNostr search: ")
  (let ((query (string-trim query)))
    (when (string-empty-p query)
      (user-error "Search query cannot be empty"))
    (let ((buffer (get-buffer-create (format "*Nostr Search: %s*" query))))
      (with-current-buffer buffer
        (nostr-search-mode)
        (setq-local nostr-search-query query)
        (nostr-search-refresh)
        (when nostr-search-auto-relay
          (nostr-search-relay)))
      (pop-to-buffer buffer))))

(provide 'nostr-search)
;;; nostr-search.el ends here
