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
(require 'nostr-relay)
(require 'nostr-share)
(require 'nostr-thread)
(require 'nostr-ui)
(require 'transient)

(defvar-local nostr-search-query nil
  "Search query displayed by the current search buffer.")

(defvar nostr-search-limit 100
  "Maximum number of local search results displayed.")

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
    ("s" "Search relays" nostr-search-relay)]
   ["Result"
    ("RET" "Open thread" nostr-search-open-at-point)
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
  (let ((pubkey (nostr-search--query-pubkey query)))
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
                                      (= events:pubkey $s3)))
                      :order-by [(desc events:created_at)]
                      :limit $s4]
                     (nostr-search--like-pattern query)
                     query
                     pubkey
                     (or limit nostr-search-limit)))))

(defun nostr-search-refresh ()
  "Refresh the current search buffer."
  (interactive)
  (unless nostr-search-query
    (user-error "No search query is associated with this buffer"))
  (let ((inhibit-read-only t)
        (position-state (nostr-ui-capture-position))
        (results (nostr-search--select-local nostr-search-query)))
    (nostr-relay-fetch-event-metadata
     (mapcar (lambda (event) (alist-get 'id event)) results))
    (nostr-ui-clear)
    (nostr-ui-insert-status-header
     "Search"
     nostr-search-query
     (format "%d local cached result%s"
             (length results)
             (if (= (length results) 1) "" "s")))
    (if results
        (dolist (event results)
          (nostr-ui-insert-note event))
      (nostr-ui-insert-empty-state
       "No cached search results."
       "Use s to query connected NIP-50 relays."))
    (nostr-ui-insert-footer
     '("g refresh" "s relay search" "RET open" "w copy" "b browse" "? actions"))
    (nostr-ui-finish-refresh position-state)))

(defun nostr-search-open-at-point ()
  "Open the selected search result."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data)))
    (nostr-thread-open event)))

(defun nostr-search-relay ()
  "Start relay-backed search for the current query."
  (interactive)
  (unless nostr-search-query
    (user-error "No search query is associated with this buffer"))
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
