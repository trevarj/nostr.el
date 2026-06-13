;;; nostr-timeline.el --- Nostr timeline UI -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Main follows feed buffer.

;;; Code:

(require 'transient)
(require 'nostr-actions)
(require 'nostr-compose)
(require 'nostr-db)
(require 'nostr-dispatch)
(require 'nostr-notifications)
(require 'nostr-profile)
(require 'nostr-relay)
(require 'nostr-relays)
(require 'nostr-search)
(require 'nostr-setup)
(require 'nostr-share)
(require 'nostr-ui)

(defvar nostr-timeline-current-pubkey nil
  "Pubkey whose follows feed is displayed.")

(defvar-local nostr-timeline-feed-kind 'feed
  "Feed kind displayed in the current timeline buffer.")

(defcustom nostr-timeline-limit 100
  "Maximum number of notes shown in timeline buffers."
  :type 'integer
  :group 'nostr)

(defvar nostr-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-timeline-refresh)
    (define-key map (kbd "n") #'nostr-ui-next-section)
    (define-key map (kbd "p") #'nostr-ui-prev-section)
    (define-key map (kbd "TAB") #'nostr-ui-toggle-section)
    (define-key map (kbd "RET") #'nostr-timeline-open-thread)
    (define-key map (kbd "a") #'nostr-timeline-open-author)
    (define-key map (kbd "c") #'nostr-compose-open)
    (define-key map (kbd "/") #'nostr-search-open)
    (define-key map (kbd "o") #'nostr-open-identifier)
    (define-key map (kbd "N") #'nostr-notifications-open)
    (define-key map (kbd "L") #'nostr-relays-open)
    (define-key map (kbd "S") #'nostr-setup-status)
    (define-key map (kbd "r") #'nostr-timeline-reply)
    (define-key map (kbd "l") #'nostr-timeline-like)
    (define-key map (kbd "m") #'nostr-ui-toggle-note-media)
    (define-key map (kbd "R") #'nostr-timeline-repost)
    (define-key map (kbd "Q") #'nostr-timeline-quote)
    (define-key map (kbd "w") #'nostr-share-copy)
    (define-key map (kbd "y") #'nostr-share-copy-raw-id)
    (define-key map (kbd "b") #'nostr-share-browse)
    (define-key map (kbd "?") #'nostr-timeline-actions)
    (define-key map (kbd "f") #'nostr-timeline-feed)
    (define-key map (kbd "h") #'nostr-timeline-feed)
    (define-key map (kbd "C") #'nostr-timeline-conversations)
    (define-key map (kbd "e") #'nostr-timeline-conversations)
    (define-key map (kbd "G") #'nostr-timeline-global)
    (define-key map (kbd "P") #'nostr-timeline-my-posts)
    map)
  "Keymap for `nostr-timeline-mode'.")

(define-derived-mode nostr-timeline-mode special-mode "Nostr"
  "Mode for the main Nostr feed."
  (setq-local truncate-lines nil))

(transient-define-prefix nostr-timeline-actions ()
  "Actions for the selected Nostr item."
  [["Note"
    ("r" "Reply" nostr-timeline-reply)
    ("RET" "Open thread" nostr-timeline-open-thread)
    ("a" "Open author" nostr-timeline-open-author)]
   ["Feed"
    ("g" "Refresh" nostr-timeline-refresh)
    ("f" "Feed" nostr-timeline-feed)
    ("C" "Conversations" nostr-timeline-conversations)
    ("G" "Global" nostr-timeline-global)
    ("P" "My posts" nostr-timeline-my-posts)
    ("c" "Compose" nostr-compose-open)
    ("/" "Search" nostr-search-open)
    ("o" "Open id" nostr-open-identifier)]
   ["Social"
    ("l" "Like" nostr-timeline-like)
    ("m" "Toggle media" nostr-ui-toggle-note-media)
    ("R" "Repost" nostr-timeline-repost)
    ("Q" "Quote" nostr-timeline-quote)]
   ["Share"
    ("w" "Copy NIP-19" nostr-share-copy)
    ("y" "Copy raw id" nostr-share-copy-raw-id)
    ("b" "Browse" nostr-share-browse)]
   ["Views"
    ("N" "Notifications" nostr-notifications-open)
    ("L" "Relays" nostr-relays-open)
    ("S" "Setup" nostr-setup-status)]])

(defun nostr-timeline--feed-title ()
  "Return title for the current timeline feed."
  (pcase nostr-timeline-feed-kind
    ('feed "Feed")
    ('home "Feed")
    ('conversations "Conversations")
    ('replies "Conversations")
    ('global "Global")
    ('my-posts "My Posts")
    ('media "Media")
    (_ "Feed")))

(defun nostr-timeline--feed-detail ()
  "Return status detail for the current timeline feed."
  (pcase nostr-timeline-feed-kind
    ((or 'feed 'home) "Notes from accounts you follow.")
    ((or 'conversations 'replies) "Replies by accounts you follow.")
    ('global "All cached notes from current relays.")
    ('my-posts "Notes authored by the current account.")
    ('media "Followed-account notes that include media links.")
    (_ "Notes from accounts you follow.")))

(defun nostr-timeline--select-events ()
  "Return events for the current timeline feed kind."
  (pcase nostr-timeline-feed-kind
    ((or 'feed 'home)
     (nostr-db-select-account-feed nostr-timeline-current-pubkey nostr-timeline-limit))
    ((or 'conversations 'replies)
     (nostr-db-select-conversations-feed nostr-timeline-current-pubkey nostr-timeline-limit))
    ('global (nostr-db-select-global-feed nostr-timeline-limit))
    ('my-posts (nostr-db-select-author-feed nostr-timeline-current-pubkey nostr-timeline-limit))
    ('media (nostr-db-select-media-feed nostr-timeline-current-pubkey nostr-timeline-limit))
    (_ (nostr-db-select-account-feed nostr-timeline-current-pubkey nostr-timeline-limit))))

(defun nostr-timeline--backfill-profiles (events)
  "Request missing profile metadata for authors in EVENTS."
  (dolist (event events)
    (nostr-relay-fetch-profile (alist-get 'pubkey event))))

(defun nostr-timeline--backfill-visible-metadata (events)
  "Request relay metadata for visible timeline EVENTS."
  (nostr-timeline--backfill-profiles events)
  (nostr-relay-fetch-event-metadata
   (mapcar (lambda (event) (alist-get 'id event)) events)))

(defun nostr-timeline--backfill-missing-reposts ()
  "Request reposted notes that are referenced by followed accounts."
  (when (memq nostr-timeline-feed-kind '(feed home media))
    (nostr-relay-fetch-events-by-id
     (nostr-db-select-missing-repost-targets
      nostr-timeline-current-pubkey
      nostr-timeline-limit))))

(defun nostr-timeline-refresh ()
  "Refresh the current timeline buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (position-state (nostr-ui-capture-position)))
    (nostr-ui-clear)
    (nostr-ui-insert-status-header
     (nostr-timeline--feed-title)
     "Nostr"
     (nostr-timeline--feed-detail))
    (nostr-ui-insert-primary-nav nostr-ui-primary-nav-items
                                 (pcase nostr-timeline-feed-kind
                                   ('home 'feed)
                                   ('replies 'conversations)
                                   (kind kind)))
    (let ((events (nostr-timeline--select-events)))
      (nostr-ui-prime-caches events)
      (nostr-timeline--backfill-missing-reposts)
      (nostr-timeline--backfill-visible-metadata events)
      (if events
          (dolist (event events)
            (nostr-ui-insert-note event))
        (nostr-ui-insert-empty-state
         "No cached notes for this feed."
         "Use g to refresh, / to search, or c to compose.")))
    (nostr-ui-insert-footer
     '("g refresh" "c compose" "/ search" "L relays" "? actions"))
    (nostr-ui-finish-refresh position-state)))

(defun nostr-timeline-set-feed (kind)
  "Set timeline feed KIND and refresh."
  ;; The notifications view can switch this buffer to `nostr-notifications-mode'
  ;; in place; re-establish the timeline mode (before `setq-local', since
  ;; `kill-all-local-variables' would otherwise clear the feed kind) so the
  ;; keymap and buffer-local state match the rendered feed.
  (unless (derived-mode-p 'nostr-timeline-mode)
    (nostr-timeline-mode))
  (setq-local nostr-timeline-feed-kind kind)
  (nostr-timeline-refresh))

(defun nostr-timeline-feed ()
  "Show notes from followed accounts."
  (interactive)
  (nostr-timeline-set-feed 'feed))

(defun nostr-timeline-home ()
  "Show notes from followed accounts."
  (interactive)
  (nostr-timeline-feed))

(defun nostr-timeline-conversations ()
  "Show replies from followed accounts."
  (interactive)
  (nostr-timeline-set-feed 'conversations))

(defun nostr-timeline-replies-feed ()
  "Show replies from followed accounts."
  (interactive)
  (nostr-timeline-conversations))

(defun nostr-timeline-global ()
  "Show recent root notes from the local cache."
  (interactive)
  (nostr-timeline-set-feed 'global))

(defun nostr-timeline-my-posts ()
  "Show cached notes authored by the current account."
  (interactive)
  (nostr-timeline-set-feed 'my-posts))

(defun nostr-timeline-media ()
  "Show followed-account notes that contain image URLs."
  (interactive)
  (nostr-timeline-set-feed 'media))

(defun nostr-timeline-reply ()
  "Reply to the selected timeline note."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data)))
    (nostr-compose-open event)))

(defun nostr-timeline-like ()
  "Like the selected timeline note."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data)))
    (nostr-actions-like event)))

(defun nostr-timeline-repost ()
  "Repost the selected timeline note."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data)))
    (nostr-actions-repost event)))

(defun nostr-timeline-quote ()
  "Quote the selected timeline note."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data)))
    (nostr-actions-quote event)))

(defun nostr-timeline-open-author ()
  "Open the selected timeline note's author profile."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data))
              (pubkey (alist-get 'pubkey event)))
    (nostr-profile-open pubkey)))

(defun nostr-timeline-open-thread ()
  "Open selected note thread."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data)))
    (require 'nostr-thread)
    (nostr-thread-open event)))

(provide 'nostr-timeline)
;;; nostr-timeline.el ends here
