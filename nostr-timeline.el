;;; nostr-timeline.el --- Nostr timeline UI -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Main follows feed buffer.

;;; Code:

(require 'transient)
(require 'seq)
(require 'nostr-actions)
(require 'nostr-compose)
(require 'nostr-db)
(require 'nostr-discover)
(require 'nostr-dispatch)
(require 'nostr-notifications)
(require 'nostr-profile)
(require 'nostr-reactions)
(require 'nostr-relay)
(require 'nostr-relays)
(require 'nostr-search)
(require 'nostr-setup)
(require 'nostr-share)
(require 'nostr-ui)

;; Defined in nostr.el, which requires this file (avoid a circular require).
(defvar nostr-debug-logging)
(defvar nostr-current-pubkey)
(defvar nostr-media-auto-preview)
(declare-function nostr-debug-message "nostr" (fmt &rest args))

(defvar nostr-timeline-current-pubkey nil
  "Pubkey whose follows feed is displayed.")

(defvar-local nostr-timeline-feed-kind 'feed
  "Feed kind displayed in the current timeline buffer.")

(defvar-local nostr-timeline--render-limit nil
  "Buffer-local render cap; grows as the user pages back through history.
Defaults to `nostr-timeline-limit'.")

(defvar-local nostr-timeline--oldest-rendered nil
  "created_at of the oldest note rendered in this buffer, the paging cursor.")

(defvar-local nostr-timeline--last-page-until nil
  "Cursor of the last history page requested, so an in-flight page is not
re-requested on every scroll event.")

(defcustom nostr-timeline-limit 100
  "Maximum number of notes shown in timeline buffers."
  :type 'integer
  :group 'nostr)

(defun nostr-timeline--limit ()
  "Return the active render cap for this buffer."
  (or nostr-timeline--render-limit nostr-timeline-limit))

(defcustom nostr-timeline-metadata-backfill-limit 25
  "Maximum rendered notes whose profiles and interactions are backfilled.
This keeps startup refreshes from multiplying into hundreds of profile,
reaction, repost, reply, and zap subscriptions across every connected relay."
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
    (define-key map (kbd "i") #'nostr-profile-open-self)
    (define-key map (kbd "c") #'nostr-compose-open)
    (define-key map (kbd "/") #'nostr-search-open)
    (define-key map (kbd "o") #'nostr-open-identifier)
    (define-key map (kbd "N") #'nostr-notifications-open)
    (define-key map (kbd "L") #'nostr-relays-open)
    (define-key map (kbd "S") #'nostr-setup-status)
    (define-key map (kbd "r") #'nostr-timeline-reply)
    (define-key map (kbd "l") #'nostr-timeline-like)
    (define-key map (kbd "v") #'nostr-timeline-view-reactions)
    (define-key map (kbd "m") #'nostr-ui-toggle-note-media)
    (define-key map (kbd "D") #'nostr-ui-show-publish-details)
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
    (define-key map (kbd "d") #'nostr-timeline-discover)
    (define-key map (kbd ">") #'nostr-discover-load-more)
    (define-key map (kbd "P") #'nostr-timeline-my-posts)
    map)
  "Keymap for `nostr-timeline-mode'.")

(define-derived-mode nostr-timeline-mode special-mode "Nostr"
  "Mode for the main Nostr feed."
  (setq-local truncate-lines nil)
  ;; Auto-load older history when the buffer end scrolls into view.
  (add-hook 'window-scroll-functions #'nostr-timeline--maybe-load-older nil t))

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
    ("d" "Discover" nostr-timeline-discover)
    (">" "Load more" nostr-discover-load-more)
    ("P" "My posts" nostr-timeline-my-posts)
    ("i" "My profile" nostr-profile-open-self)
    ("c" "Compose" nostr-compose-open)
    ("/" "Search" nostr-search-open)
    ("o" "Open id" nostr-open-identifier)]
   ["Social"
    ("l" "React" nostr-timeline-like)
    ("v" "View reactions" nostr-timeline-view-reactions)
    ("m" "Toggle media" nostr-ui-toggle-note-media)
    ("D" "Publish details" nostr-ui-show-publish-details)
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
    ('discover "Discover")
    ('global "Global")
    ('my-posts "My Posts")
    ('media "Media")
    (_ "Feed")))

(defun nostr-timeline--feed-detail ()
  "Return status detail for the current timeline feed."
  (pcase nostr-timeline-feed-kind
    ((or 'feed 'home) "Notes from accounts you follow.")
    ((or 'conversations 'replies) "Replies by accounts you follow.")
    ('discover (nostr-discover-status-line))
    ('global "Recent notes from current relays.")
    ('my-posts "Notes authored by the current account.")
    ('media "Followed-account notes that include media links.")
    (_ "Notes from accounts you follow.")))

(defun nostr-timeline--select-events ()
  "Return events for the current timeline feed kind.
Honors the buffer-local render limit, which grows as the user pages back."
  (let ((limit (nostr-timeline--limit)))
    (pcase nostr-timeline-feed-kind
      ((or 'feed 'home)
       (nostr-db-select-account-feed nostr-timeline-current-pubkey limit))
      ((or 'conversations 'replies)
       (nostr-db-select-conversations-feed nostr-timeline-current-pubkey limit))
      ('discover (nostr-db-select-discover-feed
                  (symbol-name nostr-discover-provider)
                  nostr-discover-scope
                  nostr-discover-timeframe
                  limit))
      ('global (nostr-db-select-global-feed limit))
      ('my-posts (nostr-db-select-author-feed nostr-timeline-current-pubkey limit))
      ('media (nostr-db-select-media-feed nostr-timeline-current-pubkey limit))
      (_ (nostr-db-select-account-feed nostr-timeline-current-pubkey limit)))))

(defun nostr-timeline--backfill-profiles (events)
  "Eagerly fetch missing author profiles for EVENTS in one batched subscription.
Avatars need the kind-0 profile cached; batching every rendered author into a
single auto-closing request loads them eagerly without a subscription per note."
  (nostr-relay-fetch-profiles-batch
   (mapcar (lambda (event) (alist-get 'pubkey event)) events)))

(defun nostr-timeline--backfill-visible-metadata (events)
  "Request reaction and interaction metadata for the top visible EVENTS.
Profiles are handled separately and eagerly; the heavier reaction/reply/zap
backfill stays capped at `nostr-timeline-metadata-backfill-limit'."
  (let* ((capped (seq-take events nostr-timeline-metadata-backfill-limit))
         (ids (mapcar (lambda (event) (alist-get 'id event)) capped)))
    (nostr-relay-fetch-event-metadata ids)
    (nostr-relay-subscribe-visible-reactions ids)))

(defun nostr-timeline--backfill-missing-reposts ()
  "Request reposted notes that are referenced by followed accounts."
  (when (memq nostr-timeline-feed-kind '(feed home media))
    (nostr-relay-fetch-events-by-id
     (nostr-db-select-missing-repost-targets
      nostr-timeline-current-pubkey
      nostr-timeline-limit))))

(defvar-local nostr-timeline--synced-token nil
  "Token identifying the feed last backfilled, so view-entry backfill subscriptions
fire once per view switch rather than on every burst refresh.")

(defun nostr-timeline--feed-entered-p (force)
  "Return non-nil when the active feed was just entered (or FORCE).
Updates the per-buffer sync token so repeated refreshes of the same view do not
re-issue the view backfill subscription."
  (let ((token (list nostr-timeline-feed-kind nostr-timeline-current-pubkey)))
    (prog1 (or force (not (equal token nostr-timeline--synced-token)))
      (setq-local nostr-timeline--synced-token token))))

(defun nostr-timeline--sync-active-feed (&optional force)
  "Start or stop relay work for the active timeline feed.
When FORCE is non-nil, request a fresh provider/relay page.

Each view drives the subscription that backfills what it shows: My Posts asks
relays for the account's own authored history (the follows feed already covers
Feed/Conversations/Media, and Global/Discover have their own paths).  View
backfill is issued on view entry only, so refreshing mid-burst does not respawn
subscriptions."
  (let ((entered (nostr-timeline--feed-entered-p force)))
    (pcase nostr-timeline-feed-kind
      ('global (nostr-relay-subscribe-global force))
      ('discover
       (nostr-relay-close-global)
       (unless nostr-discover--loading
         (pcase-let ((`(,provider ,scope ,timeframe)
                      (list (symbol-name nostr-discover-provider)
                            nostr-discover-scope
                            nostr-discover-timeframe)))
           (when (or force
                     (not (nostr-db-select-discover-feed provider scope timeframe 1)))
             (nostr-discover-refresh)))))
      ('my-posts
       (nostr-relay-close-global)
       (when entered
         (nostr-relay-fetch-my-posts nostr-timeline-current-pubkey)))
      (_ (nostr-relay-close-global)))))

(defun nostr-timeline--view-page-filter (until)
  "Return the relay filter to load history older than UNTIL for the current view.
Return nil for views that page through their own provider (Discover)."
  (pcase nostr-timeline-feed-kind
    ('my-posts (nostr-relay--my-posts-filter nostr-timeline-current-pubkey until))
    ((or 'feed 'home 'media 'conversations 'replies)
     (when-let* ((follows (nostr-db-select-follows nostr-timeline-current-pubkey)))
       (nostr-relay--feed-filter follows until)))
    ('global (nostr-relay--global-filter until))
    (_ nil)))

(defun nostr-timeline-load-older ()
  "Load older history for the current timeline view.
Grows the render window so already-cached older notes appear, and asks relays
for events older than the current oldest note.  No-op while a page for the same
cursor is already in flight, or for Discover (which has its own paging)."
  (interactive)
  (let ((oldest nostr-timeline--oldest-rendered))
    (when (and oldest
               (not (equal oldest nostr-timeline--last-page-until))
               (not (eq nostr-timeline-feed-kind 'discover)))
      (setq-local nostr-timeline--last-page-until oldest)
      (setq-local nostr-timeline--render-limit
                  (+ (nostr-timeline--limit) nostr-timeline-limit))
      (when-let* ((filter (nostr-timeline--view-page-filter (1- oldest))))
        (nostr-relay--fetch
         (nostr-relay--sub-id "page" nostr-timeline-feed-kind oldest)
         (list filter)))
      (nostr-timeline-refresh))))

(defun nostr-timeline--maybe-load-older (&rest _)
  "Auto-load older history when the end of the timeline scrolls into view.
Runs from `window-scroll-functions', so it defers the load out of redisplay and
relies on `nostr-timeline-load-older's cursor guard to coalesce the many scroll
events into one page request."
  (when (and (derived-mode-p 'nostr-timeline-mode)
             nostr-timeline--oldest-rendered
             (not (equal nostr-timeline--oldest-rendered
                         nostr-timeline--last-page-until))
             (not (eq nostr-timeline-feed-kind 'discover))
             (pos-visible-in-window-p (point-max)))
    (let ((buffer (current-buffer)))
      (run-at-time 0 nil
                   (lambda ()
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (nostr-timeline-load-older))))))))

(defun nostr-timeline-refresh (&optional force)
  "Refresh the current timeline buffer."
  (interactive (list t))
  (let ((inhibit-read-only t)
        (position-state (nostr-ui-capture-position))
        (start (and nostr-debug-logging (float-time)))
        (rendered 0))
    (nostr-timeline--sync-active-feed force)
    (nostr-ui-clear)
    (nostr-ui-insert-status-header
     (nostr-timeline--feed-title)
     nil
     (nostr-timeline--feed-detail))
    (nostr-ui-insert-primary-nav nostr-ui-primary-nav-items
                                 (pcase nostr-timeline-feed-kind
                                   ('home 'feed)
                                   ('replies 'conversations)
                                   (kind kind)))
    (let ((events (nostr-timeline--select-events)))
      ;; Track the oldest rendered note as the history-paging cursor.
      (setq-local nostr-timeline--oldest-rendered
                  (and events
                       (apply #'min
                              (or (delq nil
                                        (mapcar (lambda (e)
                                                  (alist-get 'created_at e))
                                                events))
                                  '(0)))))
      (nostr-ui-prime-caches events)
      ;; Avatars load eagerly: fetch missing author profiles on every refresh,
      ;; even mid-sync, in a single batched auto-closing subscription.
      (nostr-timeline--backfill-profiles events)
      ;; The heavier reaction/reply/zap/repost backfill stays deferred until the
      ;; sync settles, so a burst refresh does not spawn waves of relay traffic
      ;; that feed straight back into store -> refresh.
      (unless (nostr-relay-syncing-p)
        (nostr-timeline--backfill-missing-reposts)
        (nostr-timeline--backfill-visible-metadata events))
      (if events
          (dolist (event events)
            (let ((nostr-media-auto-preview
                   (and nostr-media-auto-preview
                        (not (eq nostr-timeline-feed-kind 'discover)))))
              (nostr-ui-insert-note event))
            (setq rendered (1+ rendered)))
        (nostr-ui-insert-empty-state
         (if (eq nostr-timeline-feed-kind 'discover)
             "No Discover notes cached yet."
           "No cached notes for this feed.")
         (if (eq nostr-timeline-feed-kind 'discover)
             "Use g to refresh from Primal or > to load more after results arrive."
           "Use g to refresh, / to search, or c to compose."))))
    (nostr-ui-insert-footer
     (if (eq nostr-timeline-feed-kind 'discover)
         '("g refresh" "> more" "RET thread" "c compose" "? actions")
       '("g refresh" "c compose" "/ search" "L relays" "? actions")))
    (nostr-ui-finish-refresh position-state)
    (when start
      (nostr-debug-message "timeline refresh: %d notes in %.1f ms"
                           rendered (* 1000 (- (float-time) start))))))

(defun nostr-timeline-set-feed (kind)
  "Set timeline feed KIND and refresh."
  ;; The notifications view can switch this buffer to `nostr-notifications-mode'
  ;; in place; re-establish the timeline mode (before `setq-local', since
  ;; `kill-all-local-variables' would otherwise clear the feed kind) so the
  ;; keymap and buffer-local state match the rendered feed.
  (unless (derived-mode-p 'nostr-timeline-mode)
    (nostr-timeline-mode))
  (unless nostr-timeline-current-pubkey
    (setq-local nostr-timeline-current-pubkey nostr-current-pubkey))
  ;; Reset history paging when switching views so each view starts at its top.
  (unless (eq nostr-timeline-feed-kind kind)
    (setq-local nostr-timeline--render-limit nil
                nostr-timeline--oldest-rendered nil
                nostr-timeline--last-page-until nil))
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
  "Show recent notes from connected relays."
  (interactive)
  (nostr-timeline-set-feed 'global))

(defun nostr-timeline-discover ()
  "Show provider-ranked top notes."
  (interactive)
  (nostr-timeline-set-feed 'discover))

(defun nostr-discover-load-more ()
  "Load the next page of Discover notes."
  (interactive)
  (unless (eq nostr-timeline-feed-kind 'discover)
    (user-error "Load more is only available in Discover"))
  (nostr-discover-refresh 'append))

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
  "React to the selected timeline note."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data)))
    (nostr-actions-react-menu event)))

(defun nostr-timeline-view-reactions ()
  "Show cached reactions for the selected timeline note."
  (interactive)
  (when-let* ((event (nostr-ui-selected-data)))
    (nostr-reactions-open event)))

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
  (unless (nostr-ui-activate-button-at-point)
    (when-let* ((event (nostr-ui-selected-data)))
      (require 'nostr-thread)
      (nostr-thread-open event))))

(provide 'nostr-timeline)
;;; nostr-timeline.el ends here
