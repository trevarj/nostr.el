;;; nostr-relay.el --- Nostr relay websocket client -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Emacs-owned relay IO and subscription state.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'nostr-backend)
(require 'nostr-daemon)
(require 'nostr-db)
(require 'nostr-event)

(declare-function nostr-ui-refresh-note-counts "nostr-ui" (event-id))
(declare-function nostr-visible-note-ids "nostr" ())

(defcustom nostr-relay-urls '("wss://relay.primal.net"
                              "wss://relay.damus.io")
  "Relay URLs used by the Nostr client."
  :type '(repeat string)
  :group 'nostr)

(defcustom nostr-relay-indexer-urls '("wss://purplepag.es"
                                      "wss://relay.nostr.band")
  "Relays queried to discover authors' NIP-65 relay lists (kind 10002).
These index relay-list events broadly, so the client can learn which relays an
account actually reads from and writes to even when that account is not present
on the configured relays."
  :type '(repeat string)
  :group 'nostr)

(defcustom nostr-relay-max-discovered-relays 8
  "Maximum number of NIP-65-discovered write relays to auto-connect.
Bounds gossip-style discovery so following many accounts does not open a
connection to every relay any of them uses."
  :type 'integer
  :group 'nostr)

(defcustom nostr-default-feed-limit 100
  "Default relay limit for feed subscriptions."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-personal-limit 50
  "Maximum number of own-account events requested per relay at startup."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-mentions-limit 30
  "Maximum number of account mentions requested per relay at startup."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-follow-metadata-limit 25
  "Maximum number of followed-account metadata events requested per relay."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-startup-window-seconds (* 60 60 24 14)
  "Maximum lookback window for startup relay subscriptions when no cache exists."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-global-limit 30
  "Maximum number of notes requested per relay for the Global timeline."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-global-window-seconds (* 60 60)
  "Seconds of history requested when refreshing the Global timeline."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-global-refresh-interval 60
  "Minimum seconds between automatic Global relay refresh requests."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-publish-pending-stale-seconds 60
  "Seconds after which a pending publish receipt is retryable."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-since-overlap-seconds (* 60 10)
  "Seconds of overlap to use when resubscribing for cached feeds."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-verify-events t
  "Whether to verify event signatures before storing relay events."
  :type 'boolean
  :group 'nostr)

(defcustom nostr-relay-open-timeout-seconds 2
  "Maximum seconds to spend starting a relay websocket connection."
  :type 'number
  :group 'nostr)

(defcustom nostr-relay-fetch-window-seconds 12
  "Seconds a one-shot fetch subscription stays open before it is closed.
One-shot fetches (profiles, author history, event metadata, missing events) span
every connected relay under one id.  Relay-side EOSE auto-close cannot be used:
it tears the subscription down on the FASTEST relay's EOSE, truncating slower
relays that hold most of the results.  Instead the subscription stays open for
this window -- long enough for slow relays to answer -- then is closed to free
the relay's subscription slot."
  :type 'number
  :group 'nostr)

(defcustom nostr-relay-max-concurrent-verifications 8
  "Maximum number of concurrent event-signature verification subprocesses.
Incoming relay events beyond this limit are queued and verified as
in-flight verifications finish, so a busy feed cannot fork an unbounded
number of `nostr-el-backend verify-event' subprocesses."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-max-queued-verifications 256
  "Maximum number of relay events waiting for signature verification.
When the queue is full, additional events are dropped before spawning backend
processes or storing unverified data.  This bounds memory growth when a relay
floods the client faster than signatures can be checked."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-ingestion-batch-size 32
  "Maximum number of verified events stored per background ingestion tick.
Verified events are not stored synchronously in the verification-completion
callback; they are queued and drained in batches from a timer so the main
thread stays responsive during the initial-sync EOSE burst.  Each tick yields
to the command loop, letting redisplay and user input interleave with storage."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-ingestion-interval 0.08
  "Seconds between background ingestion drain ticks.
Pending verified events are stored in batches of
`nostr-relay-ingestion-batch-size' at this cadence.  Short enough that the queue
keeps up with the burst, long enough that each batch yields to the command loop."
  :type 'number
  :group 'nostr)

(defcustom nostr-relay-sync-timeout-seconds 12
  "Maximum seconds to treat the client as still doing its initial sync.
While syncing, UI refreshes are throttled (see `nostr--schedule-refresh').
A relay that never sends EOSE cannot wedge the UI in throttled mode: the
syncing state is force-cleared after this many seconds."
  :type 'number
  :group 'nostr)

(defcustom nostr-relay-connect-interval 0.05
  "Seconds between deferred relay connection attempts."
  :type 'number
  :group 'nostr)

(defcustom nostr-relay-event-metadata-request-ttl 60
  "Seconds to suppress repeated visible-note metadata requests."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-visible-reaction-window-seconds nil
  "Optional recent-reaction window for visible note cards.
When nil, visible-card reaction subscriptions request reactions by event id
without a time cutoff.  This is still scoped to rendered cards, but lets the
client backfill older reactions into the local `reactions' table."
  :type '(choice (const :tag "No time cutoff" nil)
                 integer)
  :group 'nostr)

(defcustom nostr-relay-search-urls '("wss://cache2.primal.net/v1")
  "Additional search-capable relays used for profile searches.
These relays are connected lazily when search is requested and are not used for
startup feed subscriptions unless also present in `nostr-relay-urls'."
  :type '(repeat string)
  :group 'nostr)

(defcustom nostr-relay-search-author-urls '("wss://relay.primal.net")
  "Additional relays used to fetch notes for profile-search matches."
  :type '(repeat string)
  :group 'nostr)

(defcustom nostr-relay-search-timeout-seconds 12
  "Seconds before relay-backed search progress is cleared without EOSE/CLOSED."
  :type 'number
  :group 'nostr)

(defcustom nostr-relay-activity-idle-delay 1.5
  "Seconds to keep the mode-line ingestion indicator visible after events arrive."
  :type 'number
  :group 'nostr)

(defcustom nostr-notifications-mode-line-icon "◉"
  "Compact icon shown in the mode line for unread Nostr notifications."
  :type 'string
  :group 'nostr)

(defvar nostr-relay--connections (make-hash-table :test #'equal)
  "Relay URL to websocket connection map.")

(defvar nostr-relay--connecting (make-hash-table :test #'equal)
  "Relay URL to in-flight reachability probe process map.
A URL is present while its non-blocking TCP probe is pending, so duplicate
connection attempts are suppressed before the websocket exists.")

(defvar nostr-relay--connect-queue nil
  "Pending deferred relay connection requests as (URL . PUBKEY).")

(defvar nostr-relay--connect-timer nil
  "Timer used to drain `nostr-relay--connect-queue'.")

(defvar nostr-relay--subscriptions (make-hash-table :test #'equal)
  "Active subscription IDs.")

(defvar nostr-relay--syncing-subs (make-hash-table :test #'equal)
  "Set of pending initial-sync feed subscriptions awaiting EOSE.
Keys are URL+sub-id strings for personal/follows subscriptions.  The client is
\"syncing\" (see `nostr-relay-syncing-p') while this is non-empty.")

(defvar nostr-relay--sync-timeout-timer nil
  "Safety timer that force-clears the syncing state if EOSE never arrives.")

(defvar nostr-relay-sync-finished-hook nil
  "Hook run with no arguments when the initial-sync EOSE burst completes.")

(defvar nostr-relay--verified-event-ids (make-hash-table :test #'equal)
  "Ids of events already verified and stored this session.
Used to skip redundant verification subprocesses and re-store work when the
same event id arrives from multiple overlapping relay subscriptions.")

(defun nostr-relay--reset-seen-events ()
  "Clear the set of already-verified event ids.
Call after the cache database is reset so stale ids do not suppress
re-storage of events."
  (clrhash nostr-relay--verified-event-ids))

(defvar nostr-relay--verify-inflight 0
  "Number of event-signature verifications currently in flight.")

(defvar nostr-relay--verify-queue nil
  "FIFO queue of pending (URL . EVENT) verification requests.")

(defvar nostr-relay--verify-drain-timer nil
  "Pending timer that drains queued event verifications.")

(defvar nostr-relay--ingestion-queue nil
  "FIFO queue of verified (URL . EVENT) pairs awaiting background ingestion.
Verification completes asynchronously; instead of storing each event
synchronously in the completion callback (which would pile work on the main
thread during the initial-sync burst), verified events are queued here and
drained in bounded batches by `nostr-relay--drain-ingestion-queue'.")

(defvar nostr-relay--ingestion-drain-timer nil
  "Pending timer that drains `nostr-relay--ingestion-queue' in batches.")

(defvar nostr-relay--profile-requests (make-hash-table :test #'equal)
  "Pubkeys with in-flight profile metadata requests.")

(defvar nostr-relay--profile-batch-requests (make-hash-table :test #'equal)
  "Pubkey to last batched profile-fetch time, TTL-suppressed.
Tracks eager avatar fetches separately from `nostr-relay--profile-requests' so a
batch does not feed the mode-line pending-profile spinner.")

(defvar nostr-relay--profile-request-counts (make-hash-table :test #'equal)
  "Pubkeys to remaining relay EOSE responses for profile metadata requests.")

(defvar nostr-relay--profile-request-subscriptions (make-hash-table :test #'equal)
  "Profile metadata subscription ids to requested pubkeys.")

(defvar nostr-relay--event-metadata-requests (make-hash-table :test #'equal)
  "Event ids recently requested for interaction metadata.")

(defvar nostr-relay--visible-reaction-sub-id nil
  "Active subscription id for visible-card reaction events.")

(defvar nostr-relay--visible-reaction-event-ids nil
  "Event ids currently tracked by the visible-card reaction subscription.")

(defvar nostr-relay--visible-reaction-relays (make-hash-table :test #'equal)
  "Relay URLs that received the active visible-card reaction subscription.")

(defvar nostr-relay--event-id-requests (make-hash-table :test #'equal)
  "Event ids recently requested directly by id.")

(defvar nostr-relay--addressable-event-requests (make-hash-table :test #'equal)
  "Addressable event coordinates recently requested directly.")

(defvar nostr-relay--search-profile-queries (make-hash-table :test #'equal)
  "Search subscription ids to profile query strings.")

(defvar nostr-relay--search-profile-author-requests (make-hash-table :test #'equal)
  "Profile-search author fetches already requested by query and pubkey.")

(defvar nostr-relay--pending-subscriptions (make-hash-table :test #'equal)
  "Relay URL to pending subscriptions as (SUB-ID FILTERS) lists.")

(defvar nostr-relay--search-request-counts (make-hash-table :test #'equal)
  "Search subscription ids to pending relay response counts.")

(defvar nostr-relay--search-author-request-counts (make-hash-table :test #'equal)
  "Author subscription ids started by profile search to pending relay counts.")

(defvar nostr-relay--search-timeout-timers (make-hash-table :test #'equal)
  "Search subscription ids to timeout timers.")

(defvar nostr-relay--global-last-request-time nil
  "Last `float-time' when Global requested relay events.")

(defvar nostr-relay-event-hook nil
  "Hook run with normalized events after they are stored.")

(defvar nostr-relay--ingested-event-count 0
  "Number of recently stored relay events shown in the mode line.")

(defvar nostr-relay--activity-timer nil
  "Timer that clears recent relay ingestion activity.")

(defvar nostr-relay--mode-line-string nil
  "Mode-line text describing current Nostr relay activity.")

(defvar nostr-relay--mode-line-timer nil
  "Pending timer for coalesced Nostr mode-line redisplay.")

(defconst nostr-relay--mode-line-spinner-frames
  ["⠋" "⠙" "⠹" "⢸" "⣰" "⣠" "⣄" "⣆" "⡇" "⠏"]
  "Four-row braille spinner frames for the mode line.")

(defvar nostr-relay--mode-line-spinner-index 0
  "Current index into `nostr-relay--mode-line-spinner-frames'.")

(defvar nostr-current-pubkey)
(defvar nostr-db-path)

(defun nostr-relay--ensure-mode-line ()
  "Install the Nostr relay activity segment in `global-mode-string'."
  (add-to-list 'global-mode-string
               '(:eval nostr-relay--mode-line-string)
               t
               #'equal))

(defun nostr-relay--pending-profile-count ()
  "Return number of profile metadata requests still waiting for relay results."
  (hash-table-count nostr-relay--profile-requests))

(defun nostr-relay--pending-search-count ()
  "Return number of relay-backed search requests still waiting for EOSE/CLOSED."
  (+ (hash-table-count nostr-relay--search-request-counts)
     (hash-table-count nostr-relay--search-author-request-counts)))

(defun nostr-relay--mode-line-active-p ()
  "Return non-nil when relay activity should show in the mode line."
  (or (> (nostr-relay--pending-search-count) 0)
      (> nostr-relay--ingested-event-count 0)
      (> (nostr-relay--pending-profile-count) 0)
      (> (length nostr-relay--connect-queue) 0)))

(defun nostr-relay--unread-notification-count ()
  "Return unread notification count, or 0 when the DB is unavailable."
  (if (and (boundp 'nostr-db--connection) nostr-db--connection)
      (nostr-db-unread-notification-count)
    0))

(defun nostr-relay--mode-line-string ()
  "Return compact mode-line text for Nostr activity and notifications."
  (let ((parts nil)
        (unread (nostr-relay--unread-notification-count)))
    (when (nostr-relay--mode-line-active-p)
      (push (aref nostr-relay--mode-line-spinner-frames
                  nostr-relay--mode-line-spinner-index)
            parts))
    (when (> unread 0)
      (push (format "%s%d" nostr-notifications-mode-line-icon unread) parts))
    (when parts
      (format " Nostr %s" (string-join (nreverse parts) " ")))))

(defun nostr-relay--schedule-mode-line-refresh ()
  "Schedule a coalesced mode-line refresh and spinner tick."
  (unless (timerp nostr-relay--mode-line-timer)
    (setq nostr-relay--mode-line-timer
          (run-at-time
           0.2 nil
           (lambda ()
             (setq nostr-relay--mode-line-timer nil)
             (when nostr-relay--mode-line-string
               (setq nostr-relay--mode-line-spinner-index
                     (mod (1+ nostr-relay--mode-line-spinner-index)
                          (length nostr-relay--mode-line-spinner-frames)))
               (setq nostr-relay--mode-line-string
                     (nostr-relay--mode-line-string)))
             (force-mode-line-update)
             (when (nostr-relay--mode-line-active-p)
               (nostr-relay--schedule-mode-line-refresh)))))))

(defun nostr-relay--update-mode-line ()
  "Refresh the Nostr relay activity mode-line segment state."
  (nostr-relay--ensure-mode-line)
  (setq nostr-relay--mode-line-string
        (nostr-relay--mode-line-string))
  ;; Relay process filters can run while redisplay is evaluating third-party
  ;; mode-line forms.  Do not force an immediate all-frame mode-line update
  ;; from that path; coalesce the visual invalidation onto the timer queue.
  (nostr-relay--schedule-mode-line-refresh))

(defun nostr-relay--clear-recent-activity ()
  "Clear recent relay ingestion activity from the mode line."
  (setq nostr-relay--activity-timer nil
        nostr-relay--ingested-event-count 0)
  (nostr-relay--update-mode-line))

(defun nostr-relay--note-ingested-event ()
  "Record that one relay event was stored in the local database."
  (cl-incf nostr-relay--ingested-event-count)
  (when (timerp nostr-relay--activity-timer)
    (cancel-timer nostr-relay--activity-timer))
  (setq nostr-relay--activity-timer
        (run-at-time nostr-relay-activity-idle-delay
                     nil
                     #'nostr-relay--clear-recent-activity))
  (nostr-relay--update-mode-line))

(defun nostr-relay--track-profile-request (pubkey sub-id sent)
  "Track PUBKEY profile request SUB-ID sent to SENT relays."
  (when (> sent 0)
    (puthash pubkey t nostr-relay--profile-requests)
    (puthash pubkey sent nostr-relay--profile-request-counts)
    (puthash sub-id pubkey nostr-relay--profile-request-subscriptions)
    (nostr-relay--update-mode-line)))

(defun nostr-relay--complete-profile-request (pubkey)
  "Mark PUBKEY profile metadata as no longer pending."
  (when pubkey
    (remhash pubkey nostr-relay--profile-requests)
    (remhash pubkey nostr-relay--profile-request-counts)
    (remhash (nostr-relay--profile-sub-id pubkey)
             nostr-relay--profile-request-subscriptions)
    (nostr-relay--update-mode-line)))

(defun nostr-relay--note-profile-eose (sub-id)
  "Record an EOSE for profile metadata request SUB-ID."
  (when-let* ((pubkey (gethash sub-id nostr-relay--profile-request-subscriptions)))
    (let ((remaining (1- (or (gethash pubkey nostr-relay--profile-request-counts) 1))))
      (if (> remaining 0)
          (puthash pubkey remaining nostr-relay--profile-request-counts)
        (nostr-relay--complete-profile-request pubkey)))))

(defun nostr-relay--track-search-request (sub-id sent)
  "Track search SUB-ID sent to SENT relays."
  (when (> sent 0)
    (puthash sub-id sent nostr-relay--search-request-counts)
    (nostr-relay--arm-search-timeout sub-id)
    (nostr-relay--update-mode-line)))

(defun nostr-relay--track-search-author-request (sub-id sent)
  "Track search-triggered author SUB-ID sent to SENT relays."
  (when (> sent 0)
    (puthash sub-id sent nostr-relay--search-author-request-counts)
    (nostr-relay--arm-search-timeout sub-id)
    (nostr-relay--update-mode-line)))

(defun nostr-relay--clear-search-progress (sub-id)
  "Clear tracked search progress for SUB-ID."
  (remhash sub-id nostr-relay--search-request-counts)
  (remhash sub-id nostr-relay--search-author-request-counts)
  (when-let* ((timer (gethash sub-id nostr-relay--search-timeout-timers)))
    (when (timerp timer)
      (cancel-timer timer))
    (remhash sub-id nostr-relay--search-timeout-timers))
  (nostr-relay--update-mode-line))

(defun nostr-relay--arm-search-timeout (sub-id)
  "Clear search progress for SUB-ID if a relay never sends EOSE/CLOSED."
  (when-let* ((timer (gethash sub-id nostr-relay--search-timeout-timers)))
    (when (timerp timer)
      (cancel-timer timer)))
  (puthash sub-id
           (run-at-time nostr-relay-search-timeout-seconds
                        nil
                        #'nostr-relay--clear-search-progress
                        sub-id)
           nostr-relay--search-timeout-timers))

(defun nostr-relay--note-counted-eose (sub-id table)
  "Decrement SUB-ID in TABLE and return non-nil when it was tracked."
  (when-let* ((remaining (gethash sub-id table)))
    (if (> remaining 1)
        (puthash sub-id (1- remaining) table)
      (progn
        (remhash sub-id table)
        (unless (or (gethash sub-id nostr-relay--search-request-counts)
                    (gethash sub-id nostr-relay--search-author-request-counts))
          (when-let* ((timer (gethash sub-id nostr-relay--search-timeout-timers)))
            (when (timerp timer)
              (cancel-timer timer))
            (remhash sub-id nostr-relay--search-timeout-timers)))))
    (nostr-relay--update-mode-line)
    t))

(defun nostr-relay--note-search-eose (sub-id)
  "Record an EOSE/CLOSED for search SUB-ID."
  (or (nostr-relay--note-counted-eose sub-id nostr-relay--search-request-counts)
      (nostr-relay--note-counted-eose sub-id nostr-relay--search-author-request-counts)))

(defun nostr-relay--preference-urls (pubkey capability)
  "Return PUBKEY relay preference URLs matching CAPABILITY.
CAPABILITY is nil, `read', or `write'."
  (when (and nostr-db--connection pubkey)
    (delq nil
          (mapcar (lambda (relay)
                    (when (cond
                           ((eq capability 'read) (alist-get 'read relay))
                           ((eq capability 'write) (alist-get 'write relay))
                           (t t))
                      (alist-get 'url relay)))
                  (nostr-db-select-relay-list pubkey)))))

(defun nostr-relay-urls-for-pubkey (pubkey &optional capability)
  "Return candidate relay URLs for PUBKEY.
CAPABILITY may be nil, `read', or `write'.  Static configured relays remain
the fallback so a fresh account can bootstrap before NIP-65 metadata arrives."
  (let ((urls nil))
    (dolist (url (append nostr-relay-urls
                         (nostr-relay--preference-urls pubkey capability)))
      (when (and (stringp url) (not (string-empty-p url)))
        (cl-pushnew url urls :test #'equal)))
    (nreverse urls)))

(defun nostr-relay--sub-id (prefix &rest parts)
  "Return stable subscription id from PREFIX and PARTS."
  (format "%s-%s" prefix (substring (md5 (prin1-to-string parts)) 0 12)))

(defun nostr-relay--profile-sub-id (pubkey)
  "Return the profile metadata subscription id for PUBKEY."
  (format "profile-%s" (substring (md5 pubkey) 0 12)))

(defun nostr-relay--send (url message)
  "Route relay MESSAGE for URL through the daemon.
MESSAGE is the client frame vector Emacs used to send over the websocket: a
`REQ' becomes a relay-targeted daemon subscribe, a `CLOSE' a daemon close.  The
daemon owns the socket, so this only translates intent."
  (pcase (append message nil)
    (`("REQ" ,sub-id . ,filters)
     (nostr-daemon-subscribe sub-id filters (list url)))
    (`("CLOSE" ,sub-id)
     (nostr-daemon-close sub-id))
    (_ nil)))

(defun nostr-relay--subscription-key (url sub-id)
  "Return the relay-local subscription key for URL and SUB-ID."
  (concat url "\0" sub-id))

(defun nostr-relay--subscription-active-p (url sub-id)
  "Return non-nil when SUB-ID is active on URL."
  (gethash (nostr-relay--subscription-key url sub-id)
           nostr-relay--subscriptions))

(defun nostr-relay--remove-subscription-all (sub-id)
  "Forget SUB-ID across every relay."
  (let (keys)
    (maphash (lambda (key _value)
               (when (string-suffix-p (concat "\0" sub-id) key)
                 (push key keys)))
             nostr-relay--subscriptions)
    (dolist (key keys)
      (remhash key nostr-relay--subscriptions))))

(defun nostr-relay-subscribe (url sub-id filters &optional close-on-eose)
  "Subscribe to URL with SUB-ID and FILTERS.
With CLOSE-ON-EOSE, the relay subscription auto-closes after EOSE.  Use it for
one-shot fetches (profiles, event metadata, missing events) so they free the
relay's subscription slot instead of staying open and exhausting the relay's
concurrent-subscription limit."
  (puthash (nostr-relay--subscription-key url sub-id)
           t
           nostr-relay--subscriptions)
  (when (nostr-daemon-running-p)
    (nostr-daemon-subscribe sub-id filters (list url) close-on-eose)))

(defun nostr-relay--connection-open-p (url)
  "Return non-nil when URL is registered as connected with the daemon."
  (and (gethash url nostr-relay--connections) t))

(defun nostr-relay--queue-subscription (url sub-id filters)
  "Queue SUB-ID with FILTERS until URL opens."
  (let ((pending (gethash url nostr-relay--pending-subscriptions)))
    (puthash url (cons (list sub-id filters) pending)
             nostr-relay--pending-subscriptions)))

(defun nostr-relay--drain-pending-subscriptions (url)
  "Send pending subscriptions for URL."
  (when-let* ((pending (gethash url nostr-relay--pending-subscriptions)))
    (remhash url nostr-relay--pending-subscriptions)
    (dolist (request (nreverse pending))
      (pcase-let ((`(,sub-id ,filters) request))
        (nostr-relay-subscribe url sub-id filters)))))

(defun nostr-relay-close-subscription (url sub-id)
  "Close SUB-ID on relay URL."
  (nostr-relay--send url `["CLOSE" ,sub-id])
  (remhash (nostr-relay--subscription-key url sub-id)
           nostr-relay--subscriptions))

(defun nostr-relay-close-subscription-all (sub-id)
  "Close SUB-ID on every connected relay."
  (maphash (lambda (url _ws)
             (when (nostr-relay--subscription-active-p url sub-id)
               (nostr-relay--send url `["CLOSE" ,sub-id])))
           nostr-relay--connections)
  (nostr-relay--remove-subscription-all sub-id))

;; The daemon verifies signatures, writes the cache, and derives notifications.
;; Emacs no longer parses EVENT frames or runs a verify/ingest pipeline; it
;; reacts to the daemon's stdout stream below.

(defun nostr-relay--on-stored-event (sub-id event)
  "Run cache follow-up workflows for a daemon-stored EVENT on SUB-ID.
EVENT is the normalized alist re-read from the cache; the daemon already stored
it and derived any notifications, so this only schedules the dependent fetches
and UI count refreshes the old ingestion step performed."
  (nostr-relay--maybe-fetch-profile-search-author sub-id event)
  (nostr-relay--note-ingested-event)
  (let ((kind (alist-get 'kind event))
        (pubkey (alist-get 'pubkey event)))
    (when (equal kind nostr-kind-repost)
      ;; Repost feed items render the original note, so fetch missing targets.
      (when-let* ((target (nostr-event-repost-event-id event)))
        (nostr-relay-fetch-events-by-id (list target))))
    (when (and (equal kind nostr-kind-reaction)
               (fboundp 'nostr-ui-refresh-note-counts))
      (when-let* ((target (nostr-event-reaction-event-id event)))
        (nostr-ui-refresh-note-counts target)))
    (when (equal kind nostr-kind-metadata)
      (nostr-relay--complete-profile-request pubkey))
    ;; Author profiles are NOT fetched per stored event: that fired a profile
    ;; subscription for every event in a sync burst, flooding relays'
    ;; subscription slots (which then dropped the feed and stalled avatars).
    ;; Followed-author metadata arrives via the follows-feed metadata filter, and
    ;; the timeline render backfills missing profiles only for visible notes,
    ;; bounded and deferred until the sync settles.
    (when (and (equal kind nostr-kind-relay-list)
               (boundp 'nostr-current-pubkey)
               (equal pubkey nostr-current-pubkey))
      (nostr-relay-connect-recommended-deferred nostr-current-pubkey))
    ;; Once the account's own contact list lands, the follow set is known, so
    ;; discover and connect the relays those follows publish to (NIP-65).
    (when (and (equal kind nostr-kind-contacts)
               (boundp 'nostr-current-pubkey)
               (equal pubkey nostr-current-pubkey))
      (nostr-relay-discover-and-connect nostr-current-pubkey))
    (run-hook-with-args 'nostr-relay-event-hook event)
    event))

(defun nostr-relay--on-daemon-notification (notification)
  "Bridge a daemon stdout NOTIFICATION into relay sync/cache bookkeeping.
`stored' runs the per-event follow-ups, while `eose'/`closed' drive the same
profile/search/feed EOSE tracking the websocket frame handler used to."
  (pcase (alist-get 'event notification)
    ("stored"
     (when-let* ((event (nostr-db-select-event (alist-get 'id notification))))
       (nostr-relay--on-stored-event (alist-get 'sub notification) event)))
    ("eose"
     (let ((sub-id (alist-get 'sub notification))
           (relay (alist-get 'relay notification)))
       (nostr-relay--note-profile-eose sub-id)
       (nostr-relay--note-search-eose sub-id)
       (nostr-relay--handle-eose relay sub-id)))
    ("closed"
     (let ((sub-id (alist-get 'sub notification))
           (relay (alist-get 'relay notification)))
       (remhash (nostr-relay--subscription-key relay sub-id)
                nostr-relay--subscriptions)
       (nostr-relay--note-profile-eose sub-id)
       (nostr-relay--note-search-eose sub-id)
       (remhash sub-id nostr-relay--search-profile-queries)))))

(add-hook 'nostr-daemon-event-hook #'nostr-relay--on-daemon-notification)

(defun nostr-relay--search-profile-query-key (query)
  "Return normalized key for profile search QUERY."
  (downcase (string-remove-prefix "@" (string-trim (or query "")))))

(defun nostr-relay--search-profile-content-match-p (query content)
  "Return non-nil when profile CONTENT matches profile search QUERY."
  (let ((needle (nostr-relay--search-profile-query-key query)))
    (and (not (string-empty-p needle))
         (stringp content)
         (ignore-errors
           (let* ((profile (json-parse-string content
                                              :object-type 'alist
                                              :array-type 'list
                                              :false-object nil))
                  (fields (list (alist-get 'name profile)
                                (alist-get 'display_name profile)
                                (alist-get 'displayName profile)
                                (alist-get 'username profile)
                                (alist-get 'nip05 profile))))
             (seq-some
              (lambda (value)
                (and (stringp value)
                     (string-match-p (regexp-quote needle) (downcase value))))
              fields))))))

(defun nostr-relay--maybe-fetch-profile-search-author (sub-id event)
  "Fetch author activity when profile-search EVENT matches SUB-ID's query."
  (when-let* ((query (gethash sub-id nostr-relay--search-profile-queries)))
    (when (and (equal (alist-get 'kind event) nostr-kind-metadata)
               (nostr-relay--search-profile-content-match-p
                query
                (alist-get 'content event)))
      (when-let* ((pubkey (alist-get 'pubkey event)))
        (let ((request-key (concat (nostr-relay--search-profile-query-key query)
                                   "\0"
                                   pubkey)))
          (unless (gethash request-key nostr-relay--search-profile-author-requests)
            (puthash request-key t nostr-relay--search-profile-author-requests)
            (nostr-relay-fetch-author pubkey nostr-default-feed-limit)
            (nostr-relay-fetch-author-from-urls
             pubkey nostr-default-feed-limit nostr-relay-search-author-urls t)))))))

(defun nostr-relay--since-for-pubkeys (pubkeys)
  "Return a since timestamp for PUBKEYS with overlap."
  (if-let* ((latest (nostr-db-oldest-latest-event-time pubkeys)))
      (max 0 (- latest nostr-relay-since-overlap-seconds))
    (max 0 (- (floor (float-time)) nostr-relay-startup-window-seconds))))

(defun nostr-relay--since-for-pubkey (pubkey)
  "Return a conservative since timestamp for PUBKEY."
  (nostr-relay--since-for-pubkeys (list pubkey)))

(defun nostr-relay--personal-filters (pubkey)
  "Return personal activity filters for PUBKEY.
Nostr filter fields are ANDed, so authored events and mentions must be separate
filters or kind-0 metadata will never match."
  `((("kinds" . (,nostr-kind-metadata
                 ,nostr-kind-contacts
                 ,nostr-kind-mute-list
                 ,nostr-kind-text-note
                 ,nostr-kind-repost
                 ,nostr-kind-relay-list))
     ("authors" . (,pubkey))
     ("since" . ,(nostr-relay--since-for-pubkey pubkey))
     ("limit" . ,nostr-relay-personal-limit))
    (("kinds" . (,nostr-kind-text-note
                 ,nostr-kind-repost
                 ,nostr-kind-reaction
                 ,nostr-kind-zap-receipt))
     ("#p" . (,pubkey))
     ("since" . ,(nostr-relay--since-for-pubkey pubkey))
     ("limit" . ,nostr-relay-mentions-limit))))

(defun nostr-relay--contacts-filter (pubkey)
  "Return contact-list filter for PUBKEY."
  `(("kinds" . (,nostr-kind-contacts ,nostr-kind-mute-list ,nostr-kind-relay-list))
    ("authors" . (,pubkey))
    ("limit" . 10)))

(defun nostr-relay--feed-filter (pubkeys &optional until)
  "Return follows feed filter for PUBKEYS.
With UNTIL, page back through history: drop the incremental `since' bound and
fetch events older than UNTIL instead."
  (delq nil
        `(("kinds" . (,nostr-kind-text-note
                      ,nostr-kind-repost
                      ,nostr-kind-reaction))
          ("authors" . ,pubkeys)
          ,(if until
               `("until" . ,until)
             (when-let* ((since (nostr-relay--since-for-pubkeys pubkeys)))
               `("since" . ,since)))
          ("limit" . ,nostr-default-feed-limit))))

(defun nostr-relay--follow-metadata-filter (pubkeys)
  "Return followed-account metadata filter for PUBKEYS.
Kind-0 is a replaceable event: the current profile may be months old, so this
must NOT carry a `since' bound or most avatars never load.  The limit covers one
profile per followed author."
  `(("kinds" . (,nostr-kind-metadata))
    ("authors" . ,pubkeys)
    ("limit" . ,(max nostr-relay-follow-metadata-limit (length pubkeys)))))

(defun nostr-relay--global-sub-id ()
  "Return the stable Global timeline subscription id."
  "global-recent")

(defun nostr-relay--global-filter (&optional until)
  "Return the conservative Global timeline relay filter.
With UNTIL, page back: fetch events older than UNTIL instead of the recent
window."
  (delq nil
        `(("kinds" . (,nostr-kind-text-note))
          ,(if until
               `("until" . ,until)
             `("since" . ,(max 0 (- (floor (float-time))
                                    nostr-relay-global-window-seconds))))
          ("limit" . ,nostr-relay-global-limit))))

(defun nostr-relay-subscribe-global (&optional force)
  "Request recent Global timeline events from connected relays.
When FORCE is nil, suppress repeated requests within
`nostr-relay-global-refresh-interval'."
  (let ((now (float-time)))
    (when (or force
              (not nostr-relay--global-last-request-time)
              (>= (- now nostr-relay--global-last-request-time)
                  nostr-relay-global-refresh-interval))
      (setq nostr-relay--global-last-request-time now)
      (let ((sub-id (nostr-relay--global-sub-id))
            (filter (nostr-relay--global-filter))
            (sent 0))
        (maphash (lambda (url _ws)
                   (nostr-relay-subscribe url sub-id (list filter))
                   (setq sent (1+ sent)))
                 nostr-relay--connections)
        sent))))

(defun nostr-relay-close-global ()
  "Close any active Global timeline relay subscription."
  (nostr-relay-close-subscription-all (nostr-relay--global-sub-id)))

(defun nostr-relay-syncing-p ()
  "Return non-nil while the initial-sync feed subscriptions await EOSE."
  (> (hash-table-count nostr-relay--syncing-subs) 0))

(defun nostr-relay--feed-sub-p (sub-id)
  "Return non-nil when SUB-ID is an initial-sync feed subscription."
  (and (stringp sub-id)
       (or (string-prefix-p "personal-" sub-id)
           (string-prefix-p "follows-" sub-id))))

(defun nostr-relay--clear-syncing ()
  "Force the syncing state to finished and run the finished hook.
Runs `nostr-relay-sync-finished-hook' when a sync session was in progress (the
last EOSE empties the subscription set but leaves the safety timer set, so the
timer also marks an in-progress session)."
  (let ((was-syncing (or (> (hash-table-count nostr-relay--syncing-subs) 0)
                         (timerp nostr-relay--sync-timeout-timer))))
    (when (timerp nostr-relay--sync-timeout-timer)
      (cancel-timer nostr-relay--sync-timeout-timer))
    (setq nostr-relay--sync-timeout-timer nil)
    (clrhash nostr-relay--syncing-subs)
    (when was-syncing
      ;; The daemon stores events as they arrive, so by EOSE the cache already
      ;; reflects them; just trigger the final refresh.
      (run-hooks 'nostr-relay-sync-finished-hook))))

(defun nostr-relay--note-feed-subscription (url sub-id)
  "Record that feed SUB-ID was sent to URL and is awaiting EOSE."
  (when (nostr-relay--feed-sub-p sub-id)
    (puthash (concat url "\0" sub-id) t nostr-relay--syncing-subs)
    (unless (timerp nostr-relay--sync-timeout-timer)
      (setq nostr-relay--sync-timeout-timer
            (run-at-time nostr-relay-sync-timeout-seconds nil
                         #'nostr-relay--clear-syncing)))))

(defun nostr-relay--note-feed-eose (url sub-id)
  "Record EOSE for feed SUB-ID from URL, clearing syncing when none remain."
  (when (nostr-relay--feed-sub-p sub-id)
    (remhash (concat url "\0" sub-id) nostr-relay--syncing-subs)
    (unless (nostr-relay-syncing-p)
      (nostr-relay--clear-syncing))))

(defun nostr-relay-subscribe-personal (url pubkey)
  "Subscribe URL to personal events for PUBKEY."
  (let ((sub-id (nostr-relay--sub-id "personal" pubkey)))
    (nostr-relay--note-feed-subscription url sub-id)
    (nostr-relay-subscribe
     url
     sub-id
     (append (nostr-relay--personal-filters pubkey)
             (list (nostr-relay--contacts-filter pubkey))))))

(defun nostr-relay-subscribe-follows-feed (url pubkey)
  "Subscribe URL to cached follows feed for PUBKEY."
  (let ((follows (nostr-db-select-follows pubkey)))
    (when follows
      (let ((sub-id (nostr-relay--sub-id "follows" pubkey follows)))
        (nostr-relay--note-feed-subscription url sub-id)
        (nostr-relay-subscribe
         url
         sub-id
         (list (nostr-relay--feed-filter follows)
               (nostr-relay--follow-metadata-filter follows)))))))

(defun nostr-relay--handle-eose (url sub-id)
  "Handle EOSE for SUB-ID on URL."
  (when (and (boundp 'nostr-current-pubkey)
             nostr-current-pubkey
             (string-prefix-p "personal-" sub-id))
    (nostr-relay-subscribe-follows-feed url nostr-current-pubkey))
  (nostr-relay--note-feed-eose url sub-id))

(defun nostr-relay--ensure-daemon (pubkey)
  "Start the relay daemon for PUBKEY if it is not already running.
The daemon connects to relays, verifies, and writes the cache; Emacs only drives
it and reads the database back."
  (unless (nostr-daemon-running-p)
    (nostr-daemon-start nostr-db-path
                        (nostr-relay-urls-for-pubkey pubkey)
                        pubkey))
  (nostr-daemon-set-pubkey pubkey))

(defun nostr-relay-open (url pubkey)
  "Connect relay URL for PUBKEY through the daemon and prime its subscriptions.
The daemon owns the websocket and connects in the background, so this never
blocks: it registers URL with the daemon, marks it connected for Emacs's own
bookkeeping, flushes any subscriptions queued before the relay existed, and
issues the personal/follows/visible-reaction subscriptions the websocket
`on-open' handler used to send."
  (nostr-relay--ensure-daemon pubkey)
  (unless (gethash url nostr-relay--connections)
    (nostr-daemon-add-relay url)
    ;; `t' is the connected sentinel; the daemon holds the real socket.
    (puthash url t nostr-relay--connections)
    (nostr-relay--drain-pending-subscriptions url)
    (when pubkey
      (nostr-relay-subscribe-personal url pubkey)
      (nostr-relay-subscribe-follows-feed url pubkey))
    (when (fboundp 'nostr-visible-note-ids)
      (nostr-relay-subscribe-visible-reactions
       (nostr-visible-note-ids) nil (list url)))
    (nostr-relay--update-mode-line))
  t)

(defun nostr-relay-connect-all (pubkey)
  "Connect all configured relays for PUBKEY."
  (dolist (url (nostr-relay-urls-for-pubkey pubkey))
    (unless (gethash url nostr-relay--connections)
      (nostr-relay-open url pubkey))))

(defun nostr-relay--enqueue-connections (urls pubkey)
  "Queue URLS for deferred relay connection as PUBKEY."
  (dolist (url urls)
    (when (and (stringp url)
               (not (gethash url nostr-relay--connections))
               (not (cl-find url nostr-relay--connect-queue
                             :key #'car
                             :test #'equal)))
      (setq nostr-relay--connect-queue
            (append nostr-relay--connect-queue (list (cons url pubkey))))))
  (nostr-relay--update-mode-line))

(defun nostr-relay--drain-connect-queue ()
  "Open one queued relay connection and reschedule remaining work."
  (setq nostr-relay--connect-timer nil)
  (when-let* ((request (pop nostr-relay--connect-queue)))
    (let ((url (car request))
          (pubkey (cdr request)))
      (unless (gethash url nostr-relay--connections)
        (nostr-relay-open url pubkey))))
  (if nostr-relay--connect-queue
      (setq nostr-relay--connect-timer
            (run-at-time nostr-relay-connect-interval
                         nil
                         #'nostr-relay--drain-connect-queue))
    (nostr-relay--update-mode-line)))

(defun nostr-relay--ensure-connect-timer ()
  "Ensure deferred relay connection work is scheduled."
  (unless (timerp nostr-relay--connect-timer)
    (setq nostr-relay--connect-timer
          (run-at-time 0 nil #'nostr-relay--drain-connect-queue))))

(defun nostr-relay-connect-all-deferred (pubkey)
  "Queue all configured relays for PUBKEY without blocking the caller.
Also kicks off NIP-65 discovery so the account's own relays (and any followed
relays already cached) get connected even when absent from `nostr-relay-urls'."
  (nostr-relay--enqueue-connections (nostr-relay-urls-for-pubkey pubkey) pubkey)
  (nostr-relay--ensure-connect-timer)
  (nostr-relay-discover-and-connect pubkey))

(defun nostr-relay-connect-recommended (pubkey)
  "Connect missing cached NIP-65 relays for PUBKEY."
  (dolist (url (nostr-relay--preference-urls pubkey nil))
    (unless (gethash url nostr-relay--connections)
      (nostr-relay-open url pubkey))))

(defun nostr-relay-connect-recommended-deferred (pubkey)
  "Queue missing cached NIP-65 relays for PUBKEY without blocking."
  (nostr-relay--enqueue-connections (nostr-relay--preference-urls pubkey nil) pubkey)
  (nostr-relay--ensure-connect-timer))

(defun nostr-relay--relay-list-filter (pubkeys)
  "Return a NIP-65 relay-list (kind 10002) filter for PUBKEYS."
  `(("kinds" . (,nostr-kind-relay-list))
    ("authors" . ,pubkeys)))

(defun nostr-relay-discover-relays (pubkeys)
  "Discover NIP-65 relay lists for PUBKEYS via the indexer relays.
Connects `nostr-relay-indexer-urls' if needed, then issues a windowed kind-10002
fetch so the daemon caches PUBKEYS' relay preferences.  Returns the relays the
query was sent to."
  (let ((pubkeys (delete-dups (seq-filter #'stringp (copy-sequence pubkeys)))))
    (when pubkeys
      (dolist (url nostr-relay-indexer-urls)
        (unless (gethash url nostr-relay--connections)
          (nostr-relay-open url (and (boundp 'nostr-current-pubkey)
                                     nostr-current-pubkey))))
      (nostr-relay--fetch (nostr-relay--sub-id "relay-lists" pubkeys)
                          (list (nostr-relay--relay-list-filter pubkeys))))))

(defun nostr-relay-connect-discovered ()
  "Connect the most-advertised NIP-65 write relays, bounded and deduped.
Lets followed accounts' notes load from the relays where they actually publish,
even when those relays are not configured."
  (dolist (url (nostr-db-select-popular-write-relays
                nostr-relay-max-discovered-relays))
    (when (and (stringp url) (not (string-empty-p url))
               (not (gethash url nostr-relay--connections)))
      (nostr-relay-open url (and (boundp 'nostr-current-pubkey)
                                 nostr-current-pubkey)))))

(defun nostr-relay-discover-and-connect (pubkey)
  "Discover relay lists for PUBKEY and its follows, then connect their relays.
The discovery query runs first; connecting the discovered write relays is
deferred until the fetch window has had time to populate relay preferences."
  (when pubkey
    (let ((pubkeys (cons pubkey (nostr-db-select-follows pubkey))))
      (when (nostr-relay-discover-relays pubkeys)
        (run-at-time (1+ nostr-relay-fetch-window-seconds) nil
                     #'nostr-relay-connect-discovered)))))

(defun nostr-relay-disconnect-all ()
  "Disconnect all active relays."
  (when (timerp nostr-relay--connect-timer)
    (cancel-timer nostr-relay--connect-timer))
  (setq nostr-relay--connect-timer nil
        nostr-relay--connect-queue nil)
  ;; The daemon owns every relay socket; stopping it tears them all down.
  (nostr-daemon-stop)
  (clrhash nostr-relay--connecting)
  ;; Reset syncing state without running the finished hook (this is a teardown,
  ;; not a completed sync).
  (when (timerp nostr-relay--sync-timeout-timer)
    (cancel-timer nostr-relay--sync-timeout-timer))
  (setq nostr-relay--sync-timeout-timer nil)
  (clrhash nostr-relay--syncing-subs)
	  (clrhash nostr-relay--connections)
	  (clrhash nostr-relay--subscriptions)
	  (clrhash nostr-relay--profile-requests)
	  (clrhash nostr-relay--profile-batch-requests)
	  (clrhash nostr-relay--profile-request-counts)
	  (clrhash nostr-relay--profile-request-subscriptions)
	  (clrhash nostr-relay--event-metadata-requests)
	  (clrhash nostr-relay--event-id-requests)
	  (clrhash nostr-relay--search-profile-queries)
	  (clrhash nostr-relay--search-profile-author-requests)
	  (clrhash nostr-relay--pending-subscriptions)
	  (clrhash nostr-relay--search-request-counts)
	  (clrhash nostr-relay--search-author-request-counts)
  (maphash (lambda (_sub-id timer)
             (when (timerp timer)
               (cancel-timer timer)))
           nostr-relay--search-timeout-timers)
	  (clrhash nostr-relay--search-timeout-timers)
  (when (timerp nostr-relay--activity-timer)
    (cancel-timer nostr-relay--activity-timer))
  (when (timerp nostr-relay--mode-line-timer)
    (cancel-timer nostr-relay--mode-line-timer))
  (when (timerp nostr-relay--verify-drain-timer)
    (cancel-timer nostr-relay--verify-drain-timer))
  (setq nostr-relay--activity-timer nil
        nostr-relay--mode-line-timer nil
        nostr-relay--global-last-request-time nil
        nostr-relay--ingested-event-count 0
        nostr-relay--verify-inflight 0
        nostr-relay--verify-drain-timer nil
        nostr-relay--verify-queue nil
        nostr-relay--visible-reaction-sub-id nil
        nostr-relay--visible-reaction-event-ids nil)
  (clrhash nostr-relay--visible-reaction-relays)
  (nostr-relay--update-mode-line))

(defun nostr-relay-publish-target-urls ()
  "Return open relay URLs eligible for publishing.
When the current account has cached NIP-65 write relays, return only those
open relays.  Otherwise return every open relay."
  (let ((write-urls (and (boundp 'nostr-current-pubkey)
                         nostr-current-pubkey
                         (nostr-relay--preference-urls nostr-current-pubkey 'write)))
        urls)
    (maphash (lambda (url _connected)
               (when (and (nostr-relay--connection-open-p url)
                          (or (not write-urls) (member url write-urls)))
                 (push url urls)))
             nostr-relay--connections)
    (nreverse urls)))

(defun nostr-relay--client-message-event (client-message)
  "Return the event alist embedded in a relay-ready CLIENT-MESSAGE string."
  (pcase (ignore-errors
           (json-parse-string client-message
                              :object-type 'alist
                              :array-type 'list
                              :false-object nil))
    (`("EVENT" ,event) event)
    (_ nil)))

(defun nostr-relay-send-client-message-to-urls (client-message urls)
  "Publish CLIENT-MESSAGE's signed event to relay URLS through the daemon.
Records a `pending' receipt per relay immediately (the daemon updates it to
`sent'/`failed' as relays answer) and returns the number of targeted relays."
  (let ((event (nostr-relay--client-message-event client-message)))
    (if (and event urls)
        (let ((event-id (alist-get 'id event)))
          (when event-id
            (dolist (url urls)
              (nostr-db-store-publish-receipt event-id url "pending")))
          (nostr-daemon-publish event urls)
          (length urls))
      0)))

(defun nostr-relay-send-client-message (client-message)
  "Broadcast relay-ready CLIENT-MESSAGE string to selected open relays.
When the current account has cached NIP-65 write relays, publish only to those
open relays.  Otherwise publish to every open relay."
  (nostr-relay-send-client-message-to-urls
   client-message
   (nostr-relay-publish-target-urls)))

(defun nostr-relay--event-envelope (event)
  "Return a relay EVENT envelope for cached EVENT."
  (json-encode
   (vector
    "EVENT"
    (list
     (cons 'id (alist-get 'id event))
     (cons 'pubkey (alist-get 'pubkey event))
     (cons 'created_at (or (alist-get 'created_at event)
                           (alist-get 'created-at event)))
     (cons 'kind (alist-get 'kind event))
     (cons 'tags (or (alist-get 'tags event) []))
     (cons 'content (or (alist-get 'content event) ""))
     (cons 'sig (alist-get 'sig event))))))

(defun nostr-relay--retryable-publish-urls (event-id)
  "Return connected publish target URLs that should retry EVENT-ID."
  (let* ((now (truncate (float-time)))
         (receipts (nostr-db-select-publish-receipts event-id))
         (targets (nostr-relay-publish-target-urls))
         urls)
    (dolist (url targets)
      (let* ((receipt (seq-find (lambda (item)
                                  (equal (alist-get 'url item) url))
                                receipts))
             (state (alist-get 'state receipt))
             (updated-at (or (alist-get 'updated-at receipt) 0)))
        (when (or (not receipt)
                  (equal state "rejected")
                  (and (equal state "pending")
                       (> (- now updated-at)
                          nostr-relay-publish-pending-stale-seconds)))
          (push url urls))))
    (nreverse urls)))

(defun nostr-relay-retry-publish (event)
  "Retry publishing cached signed EVENT to failed or stale relay targets."
  (let* ((event-id (alist-get 'id event))
         (urls (and event-id (nostr-relay--retryable-publish-urls event-id))))
    (unless event-id
      (user-error "Selected note has no event id"))
    (unless (equal (alist-get 'pubkey event) nostr-current-pubkey)
      (user-error "Only your own notes can be re-published"))
    (unless urls
      (user-error "No failed or stale publish targets to retry"))
    (nostr-relay-send-client-message-to-urls
     (nostr-relay--event-envelope event)
     urls)))

(defun nostr-relay--search-target-urls ()
  "Return connected and configured search relay URLs."
  (let (urls)
    (maphash (lambda (url _ws)
               (cl-pushnew url urls :test #'equal))
             nostr-relay--connections)
    (dolist (url nostr-relay-search-urls)
      (when (and (stringp url) (not (string-empty-p url)))
        (cl-pushnew url urls :test #'equal)))
    (nreverse urls)))

(defun nostr-relay--primal-cache-url-p (url)
  "Return non-nil when URL is a Primal cache websocket endpoint."
  (string-match-p "\\`wss://cache[0-9]*[.]primal[.]net/" url))

(defun nostr-relay--nip50-search-url-p (url)
  "Return non-nil when URL should receive standard NIP-50 filters."
  (not (string-match-p "\\`wss://relay[.]primal[.]net\\'" url)))

(defun nostr-relay--search-filters-for-url (url query profile-query limit)
  "Return search filters for URL using QUERY and PROFILE-QUERY."
  (cond
   ((nostr-relay--primal-cache-url-p url)
    `((("cache" . ("user_search" (("query" . ,profile-query)
                                  ("limit" . ,limit)))))))
   ((nostr-relay--nip50-search-url-p url)
    `((("kinds" . (,nostr-kind-text-note))
       ("search" . ,query)
       ("limit" . ,limit))
      (("kinds" . (,nostr-kind-metadata))
       ("search" . ,profile-query)
       ("limit" . ,limit))))
   (t nil)))

(defun nostr-relay-search (query &optional limit)
  "Request relay-backed note and profile search QUERY."
  (let* ((profile-query (nostr-relay--search-profile-query-key query))
         (sub-id (format "search-%s" (md5 query)))
         (limit (or limit 50))
         (sent 0))
    (puthash sub-id query nostr-relay--search-profile-queries)
    (dolist (url (nostr-relay--search-target-urls))
      (when-let* ((filters (nostr-relay--search-filters-for-url
                            url query profile-query limit)))
        (cl-incf sent)
        (if (nostr-relay--connection-open-p url)
            (nostr-relay-subscribe url sub-id filters)
          (progn
            (nostr-relay--queue-subscription url sub-id filters)
            (nostr-relay-open url nil)))))
    (when (> sent 0)
      (run-at-time nostr-relay-fetch-window-seconds nil
                   #'nostr-relay-close-subscription-all sub-id))
    (nostr-relay--track-search-request sub-id sent)
    sub-id))

(defun nostr-relay--author-filters (pubkey limit)
  "Return filters for public activity by PUBKEY."
  `((("kinds" . (,nostr-kind-metadata
                 ,nostr-kind-contacts
                 ,nostr-kind-text-note
                 ,nostr-kind-repost
                 ,nostr-kind-reaction
                 ,nostr-kind-zap-receipt
                 ,nostr-kind-relay-list))
     ("authors" . (,pubkey))
     ("limit" . ,limit))))

(defun nostr-relay-fetch-author (pubkey &optional limit)
  "Request cached profile and public activity for author PUBKEY."
  (let ((sub-id (format "author-%s" (substring (md5 pubkey) 0 12)))
        (filters (nostr-relay--author-filters pubkey (or limit 50))))
    (nostr-relay--fetch sub-id filters)
    sub-id))

(defun nostr-relay--my-posts-filter (pubkey &optional until)
  "Return a relay filter for PUBKEY's own posts and reposts.
With UNTIL, restrict to events older than that timestamp for history paging.
There is no `since': My Posts backfills the full authored history."
  (delq nil
        `(("kinds" . (,nostr-kind-text-note ,nostr-kind-repost))
          ("authors" . (,pubkey))
          ,(when until `("until" . ,until))
          ("limit" . ,nostr-default-feed-limit))))

(defun nostr-relay-fetch-my-posts (pubkey &optional until)
  "Backfill PUBKEY's own posts via one auto-closing subscription per relay.
With UNTIL, fetch posts older than that timestamp so the My Posts view can page
back through the full authored history.  Return the subscriptions started."
  (let ((sub-id (nostr-relay--sub-id "my-posts" pubkey until))
        (filter (nostr-relay--my-posts-filter pubkey until)))
    (if (stringp pubkey)
        (nostr-relay--fetch sub-id (list filter))
      0)))

(defun nostr-relay-fetch-author-from-urls (pubkey &optional limit urls track-progress)
  "Request public activity for author PUBKEY from URLS.
Missing URLs are opened lazily and the author subscription is queued.
When TRACK-PROGRESS is non-nil, show the request as search progress."
  (let ((sub-id (format "author-%s" (substring (md5 pubkey) 0 12)))
        (filters (nostr-relay--author-filters pubkey (or limit 50)))
        (sent 0))
    (dolist (url urls)
      (when (and (stringp url) (not (string-empty-p url)))
        (cl-incf sent)
        (if (nostr-relay--connection-open-p url)
            (nostr-relay-subscribe url sub-id filters)
          (progn
            (nostr-relay--queue-subscription url sub-id filters)
            (nostr-relay-open url nil)))))
    (when (> sent 0)
      (run-at-time nostr-relay-fetch-window-seconds nil
                   #'nostr-relay-close-subscription-all sub-id))
    (when track-progress
      (nostr-relay--track-search-author-request sub-id sent))
    sub-id))

(defun nostr-relay--fetch (sub-id filters)
  "Open a one-shot fetch SUB-ID with FILTERS on every connected relay.
The subscription stays open (no relay-side auto-close, which would truncate slow
relays) and is closed after `nostr-relay-fetch-window-seconds' so it does not
linger and exhaust relay subscription slots.  Return the relays subscribed."
  (let ((sent 0))
    (maphash (lambda (url _ws)
               (nostr-relay-subscribe url sub-id filters)
               (setq sent (1+ sent)))
             nostr-relay--connections)
    (when (> sent 0)
      (run-at-time nostr-relay-fetch-window-seconds nil
                   #'nostr-relay-close-subscription-all sub-id))
    sent))

(defun nostr-relay-fetch-profile (pubkey &optional url)
  "Request kind-0 metadata for PUBKEY from URL or all connected relays.
Return the number of relay subscriptions started."
  (if (or (not pubkey)
          (gethash pubkey nostr-relay--profile-requests)
          (nostr-db-select-profile pubkey))
      0
    (let ((sub-id (nostr-relay--profile-sub-id pubkey))
          (filter `(("kinds" . (,nostr-kind-metadata))
                    ("authors" . (,pubkey))
                    ("limit" . 1)))
          (sent 0))
      (setq sent
            (if url
                (if (gethash url nostr-relay--connections)
                    (progn
                      (nostr-relay-subscribe url sub-id (list filter))
                      (run-at-time nostr-relay-fetch-window-seconds nil
                                   #'nostr-relay-close-subscription-all sub-id)
                      1)
                  0)
              (nostr-relay--fetch sub-id (list filter))))
      (nostr-relay--track-profile-request pubkey sub-id sent)
      sent)))

(defun nostr-relay-fetch-profiles-batch (pubkeys)
  "Fetch kind-0 metadata for all uncached PUBKEYS in one subscription.
Eager avatar loading: collect the authors that have no cached profile and were
not fetched within `nostr-relay-event-metadata-request-ttl', then issue a single
windowed kind-0 subscription for the whole batch.  Profiles are replaceable and
frequently old, so the filter carries no `since'.  One subscription for many
authors is far cheaper than `nostr-relay-fetch-profile' per author.  Return the
number of relay subscriptions started."
  (let* ((now (float-time))
         (missing (seq-filter
                   (lambda (pk)
                     (and (not (nostr-db-select-profile pk))
                          (let ((last (gethash pk nostr-relay--profile-batch-requests)))
                            (or (not last)
                                (> (- now last)
                                   nostr-relay-event-metadata-request-ttl)))))
                   (delete-dups (seq-filter #'stringp (copy-sequence pubkeys)))))
         (sent 0))
    (when missing
      (let ((sub-id (nostr-relay--sub-id "profiles" missing))
            (filter `(("kinds" . (,nostr-kind-metadata))
                      ("authors" . ,missing)
                      ("limit" . ,(length missing)))))
        (setq sent (nostr-relay--fetch sub-id (list filter)))
        (when (> sent 0)
          (dolist (pk missing)
            (puthash pk now nostr-relay--profile-batch-requests)))))
    sent))

(defun nostr-relay--fresh-event-metadata-ids (event-ids now)
  "Return EVENT-IDS that have not been requested recently as of NOW."
  (let (fresh)
    (dolist (event-id (delete-dups (seq-filter #'stringp (copy-sequence event-ids))))
      (let ((last-requested (gethash event-id nostr-relay--event-metadata-requests)))
        (when (or (not last-requested)
                  (> (- now last-requested) nostr-relay-event-metadata-request-ttl))
          (push event-id fresh))))
    (nreverse fresh)))

(defun nostr-relay-fetch-event-metadata (event-ids &optional limit)
  "Fetch interaction metadata for visible EVENT-IDS.
The request asks connected relays for replies, reposts, reactions, and zap
receipts that tag any of EVENT-IDS with `#e'.  Recently requested ids are
suppressed so frequent UI refreshes do not spam relays."
  (let* ((now (float-time))
         (ids (nostr-relay--fresh-event-metadata-ids event-ids now))
         (sent 0))
    (when ids
      ;; Keep interaction kinds in separate requests.  A single mixed filter
      ;; with a shared limit can be filled by replies before reactions arrive,
      ;; which leaves timeline reaction counts stale while Discover provider
      ;; stats appear correct.
      (dolist (kind (list nostr-kind-text-note
                          nostr-kind-repost
                          nostr-kind-reaction
                          nostr-kind-zap-receipt))
        (let ((sub-id (nostr-relay--sub-id (format "event-meta-%s" kind) ids))
              (filter `(("kinds" . (,kind))
                        ("#e" . ,ids)
                        ("limit" . ,(or limit (* 4 (length ids)))))))
          (setq sent (+ sent (nostr-relay--fetch sub-id (list filter))))))
        (when (> sent 0)
          (dolist (event-id ids)
            (puthash event-id now nostr-relay--event-metadata-requests))))
    sent))

(defun nostr-relay--visible-reaction-filter (event-ids &optional limit)
  "Return a NIP-25 reaction filter for visible EVENT-IDS.
NIP-01 filter fields are ANDed, so this intentionally does not add `authors'
or `#p': visible note counts need every kind-7 event tagging these note ids."
  (append `(("kinds" . (,nostr-kind-reaction))
            ("#e" . ,event-ids))
          (when nostr-relay-visible-reaction-window-seconds
            `(("since" . ,(max 0 (- (floor (float-time))
                                    nostr-relay-visible-reaction-window-seconds)))))
          (when limit
            `(("limit" . ,limit)))))

(defun nostr-relay--connected-urls ()
  "Return connected relay URLs."
  (let (urls)
    (maphash (lambda (url _ws) (push url urls)) nostr-relay--connections)
    (nreverse urls)))

(defun nostr-relay-subscribe-visible-reactions (event-ids &optional limit urls)
  "Keep an open reaction subscription for visible EVENT-IDS.
Unlike `nostr-relay-fetch-event-metadata', this is not TTL-suppressed: it is
the live path that keeps note-card reaction counts current while cards are
visible.  When URLS is non-nil, subscribe only those relays; this lets a relay
that opens after the UI render catch up without disturbing existing relays."
  (let* ((ids (delete-dups (seq-filter #'stringp (copy-sequence event-ids))))
         (ids-key (sort (copy-sequence ids) #'string<))
         (sent 0))
    (unless (equal ids-key nostr-relay--visible-reaction-event-ids)
      (when nostr-relay--visible-reaction-sub-id
        (nostr-relay-close-subscription-all nostr-relay--visible-reaction-sub-id))
      (setq nostr-relay--visible-reaction-sub-id nil
            nostr-relay--visible-reaction-event-ids nil)
      (clrhash nostr-relay--visible-reaction-relays))
    (if (not ids)
        (setq nostr-relay--visible-reaction-event-ids nil)
      (let* ((sub-id (or nostr-relay--visible-reaction-sub-id
                         (nostr-relay--sub-id "visible-reactions" ids-key)))
             (filter (nostr-relay--visible-reaction-filter ids limit))
             (target-urls (or urls (nostr-relay--connected-urls))))
        (dolist (url target-urls)
          (when (and (gethash url nostr-relay--connections)
                     (not (gethash url nostr-relay--visible-reaction-relays)))
            (nostr-relay-subscribe url sub-id (list filter))
            (puthash url t nostr-relay--visible-reaction-relays)
            (setq sent (1+ sent))))
        (when (or (> sent 0) nostr-relay--visible-reaction-sub-id)
          (setq nostr-relay--visible-reaction-sub-id sub-id
                nostr-relay--visible-reaction-event-ids ids-key))))
    sent))

(defun nostr-relay--fresh-missing-event-ids (event-ids now)
  "Return EVENT-IDS that are missing locally and not recently requested."
  (let (fresh)
    (dolist (event-id (delete-dups (seq-filter #'stringp (copy-sequence event-ids))))
      (let ((last-requested (gethash event-id nostr-relay--event-id-requests)))
        (when (and (not (nostr-db-event-pubkey event-id))
                   (or (not last-requested)
                       (> (- now last-requested) nostr-relay-event-metadata-request-ttl)))
          (push event-id fresh))))
    (nreverse fresh)))

(defun nostr-relay-fetch-events-by-id (event-ids &optional limit)
  "Request missing events matching EVENT-IDS from connected relays."
  (let* ((now (float-time))
         (ids (nostr-relay--fresh-missing-event-ids event-ids now))
         (sent 0))
    (when ids
      (let ((sub-id (nostr-relay--sub-id "event-ids" ids))
            (filter `(("ids" . ,ids)
                      ("limit" . ,(or limit (length ids))))))
        (setq sent (nostr-relay--fetch sub-id (list filter)))
        (when (> sent 0)
          (dolist (event-id ids)
            (puthash event-id now nostr-relay--event-id-requests)))))
    sent))

(defun nostr-relay--addressable-request-key (kind pubkey identifier)
  "Return a cache key for an addressable event request."
  (format "%s:%s:%s" kind pubkey (or identifier "")))

(defun nostr-relay-fetch-addressable-event (kind pubkey identifier &optional relays)
  "Request addressable event KIND by PUBKEY and IDENTIFIER.
RELAYS, when non-nil, are preferred over the currently connected relay set."
  (let* ((key (nostr-relay--addressable-request-key kind pubkey identifier))
         (now (float-time))
         (last-requested (gethash key nostr-relay--addressable-event-requests))
         (urls (delete-dups
                (seq-filter #'stringp
                            (or (copy-sequence relays)
                                (let (connected)
                                  (maphash (lambda (url _ws) (push url connected))
                                           nostr-relay--connections)
                                  connected)))))
         (filter (delq nil
                       `(("kinds" . (,kind))
                         ("authors" . (,pubkey))
                         ,(when (stringp identifier)
                            `("#d" . (,identifier)))
                         ("limit" . 1))))
         (sub-id (nostr-relay--sub-id "naddr" (list key)))
         (sent 0))
    (when (and urls
               (or (not last-requested)
                   (> (- now last-requested) nostr-relay-event-metadata-request-ttl)))
      (dolist (url urls)
        (if (gethash url nostr-relay--connections)
            (progn
              (nostr-relay-subscribe url sub-id (list filter))
              (setq sent (1+ sent)))
          (nostr-relay--queue-subscription url sub-id (list filter))
          (nostr-relay-open url (and (boundp 'nostr-current-pubkey)
                                     nostr-current-pubkey))))
      (when (> sent 0)
        (puthash key now nostr-relay--addressable-event-requests)
        (run-at-time nostr-relay-fetch-window-seconds nil
                     #'nostr-relay-close-subscription-all sub-id)))
    sent))

(provide 'nostr-relay)
;;; nostr-relay.el ends here
