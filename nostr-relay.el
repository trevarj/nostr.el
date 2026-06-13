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
(require 'websocket)
(require 'nostr-backend)
(require 'nostr-db)
(require 'nostr-event)

(defcustom nostr-relay-urls '("wss://relay.primal.net"
                              "wss://relay.damus.io")
  "Relay URLs used by the Nostr client."
  :type '(repeat string)
  :group 'nostr)

(defcustom nostr-default-feed-limit 100
  "Default relay limit for feed subscriptions."
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

(defcustom nostr-relay-max-concurrent-verifications 8
  "Maximum number of concurrent event-signature verification subprocesses.
Incoming relay events beyond this limit are queued and verified as
in-flight verifications finish, so a busy feed cannot fork an unbounded
number of `nostr-el-backend verify-event' subprocesses."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-connect-interval 0.05
  "Seconds between deferred relay connection attempts."
  :type 'number
  :group 'nostr)

(defcustom nostr-relay-event-metadata-request-ttl 60
  "Seconds to suppress repeated visible-note metadata requests."
  :type 'integer
  :group 'nostr)

(defcustom nostr-relay-activity-idle-delay 1.5
  "Seconds to keep the mode-line ingestion indicator visible after events arrive."
  :type 'number
  :group 'nostr)

(defvar nostr-relay--connections (make-hash-table :test #'equal)
  "Relay URL to websocket connection map.")

(defvar nostr-relay--connect-queue nil
  "Pending deferred relay connection requests as (URL . PUBKEY).")

(defvar nostr-relay--connect-timer nil
  "Timer used to drain `nostr-relay--connect-queue'.")

(defvar nostr-relay--subscriptions (make-hash-table :test #'equal)
  "Active subscription IDs.")

(defvar nostr-relay--verify-inflight 0
  "Number of event-signature verifications currently in flight.")

(defvar nostr-relay--verify-queue nil
  "FIFO queue of pending (URL . EVENT) verification requests.")

(defvar nostr-relay--profile-requests (make-hash-table :test #'equal)
  "Pubkeys with in-flight profile metadata requests.")

(defvar nostr-relay--profile-request-counts (make-hash-table :test #'equal)
  "Pubkeys to remaining relay EOSE responses for profile metadata requests.")

(defvar nostr-relay--profile-request-subscriptions (make-hash-table :test #'equal)
  "Profile metadata subscription ids to requested pubkeys.")

(defvar nostr-relay--event-metadata-requests (make-hash-table :test #'equal)
  "Event ids recently requested for interaction metadata.")

(defvar nostr-relay--event-id-requests (make-hash-table :test #'equal)
  "Event ids recently requested directly by id.")

(defvar nostr-relay-event-hook nil
  "Hook run with normalized events after they are stored.")

(defvar nostr-relay--ingested-event-count 0
  "Number of recently stored relay events shown in the mode line.")

(defvar nostr-relay--activity-timer nil
  "Timer that clears recent relay ingestion activity.")

(defvar nostr-relay--mode-line-string nil
  "Mode-line text describing current Nostr relay activity.")

(defvar nostr-current-pubkey)

(defun nostr-relay--ensure-mode-line ()
  "Install the Nostr relay activity segment in `global-mode-string'."
  (add-to-list 'global-mode-string
               '(:eval nostr-relay--mode-line-string)
               t
               #'equal))

(defun nostr-relay--pending-profile-count ()
  "Return number of profile metadata requests still waiting for relay results."
  (hash-table-count nostr-relay--profile-requests))

(defun nostr-relay--update-mode-line ()
  "Refresh the Nostr relay activity mode-line segment."
  (nostr-relay--ensure-mode-line)
  (let ((profiles (nostr-relay--pending-profile-count))
        (connecting (length nostr-relay--connect-queue)))
    (setq nostr-relay--mode-line-string
          (cond
           ((and (> nostr-relay--ingested-event-count 0)
                 (> profiles 0))
            (format " Nostr:loading %d events/%d profiles"
                    nostr-relay--ingested-event-count profiles))
           ((> nostr-relay--ingested-event-count 0)
            (format " Nostr:loading %d events" nostr-relay--ingested-event-count))
           ((> profiles 0)
            (format " Nostr:profiles %d" profiles))
           ((> connecting 0)
            (format " Nostr:connecting %d" connecting))
           (t nil))))
  (force-mode-line-update t))

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
  "Send MESSAGE JSON to relay URL."
  (when-let* ((ws (gethash url nostr-relay--connections)))
    (when (websocket-openp ws)
      (websocket-send-text ws (json-encode message)))))

(defun nostr-relay-subscribe (url sub-id filters)
  "Subscribe to URL with SUB-ID and FILTERS."
  (puthash sub-id t nostr-relay--subscriptions)
  (nostr-relay--send url (vconcat `["REQ" ,sub-id] filters)))

(defun nostr-relay-close-subscription (url sub-id)
  "Close SUB-ID on relay URL."
  (nostr-relay--send url `["CLOSE" ,sub-id])
  (remhash sub-id nostr-relay--subscriptions))

(defun nostr-relay--client-message-event-id (client-message)
  "Return event id from relay-ready CLIENT-MESSAGE, when available."
  (pcase (ignore-errors
           (json-parse-string client-message
                              :object-type 'alist
                              :array-type 'list
                              :false-object nil))
    (`("EVENT" ,event)
     (alist-get 'id event))
    (_ nil)))

(defun nostr-relay--invalid-event (url event message)
  "Store an invalid EVENT status from URL with MESSAGE."
  (nostr-db-store-relay-status
   url
   "invalid-event"
   (format "%s %s"
           (or (alist-get 'id event) "(unknown event)")
           (or message "signature verification failed"))))

(defun nostr-relay--store-verified-event (url event)
  "Store verified EVENT from URL and run follow-up cache workflows."
  (let ((normalized (nostr-event-normalize event url)))
    (nostr-db-store-event normalized)
    (nostr-relay--note-ingested-event)
    (nostr-relay--maybe-store-notification normalized)
    (when (equal (alist-get 'kind normalized) nostr-kind-metadata)
      (nostr-relay--complete-profile-request (alist-get 'pubkey normalized)))
    (when (memq (alist-get 'kind normalized) (list nostr-kind-text-note
                                                   nostr-kind-repost
                                                   nostr-kind-reaction
                                                   nostr-kind-zap-receipt))
      (nostr-relay-fetch-profile (alist-get 'pubkey normalized)))
    (when (and (equal (alist-get 'kind normalized) nostr-kind-relay-list)
               (boundp 'nostr-current-pubkey)
               (equal (alist-get 'pubkey normalized) nostr-current-pubkey))
      (nostr-relay-connect-recommended-deferred nostr-current-pubkey))
    (run-hook-with-args 'nostr-relay-event-hook normalized)
    normalized))

(defun nostr-relay--verify-finished ()
  "Record that one verification finished and dispatch the next queued event."
  (when (> nostr-relay--verify-inflight 0)
    (cl-decf nostr-relay--verify-inflight))
  (when-let* ((next (pop nostr-relay--verify-queue)))
    (nostr-relay--start-verification (car next) (cdr next))))

(defun nostr-relay--start-verification (url event)
  "Spawn a verify-event subprocess for EVENT from URL.
Increments the in-flight counter and decrements it (dispatching the next
queued event) when the verification resolves."
  (cl-incf nostr-relay--verify-inflight)
  (condition-case err
      (nostr-backend-call
       "verify-event"
       `((event . ,event))
       (lambda (response)
         (unwind-protect
             (if (alist-get 'valid response)
                 (nostr-relay--store-verified-event url event)
               (nostr-relay--invalid-event
                url event (alist-get 'reason response)))
           (nostr-relay--verify-finished)))
       (lambda (response stderr _status)
         (unwind-protect
             (let ((stderr-message (string-trim (or stderr ""))))
               (nostr-relay--invalid-event
                url event
                (or (alist-get 'message (alist-get 'error response))
                    (unless (string-empty-p stderr-message)
                      stderr-message)
                    "signature verification failed")))
           (nostr-relay--verify-finished))))
    (error
     (nostr-relay--invalid-event
      url event (error-message-string err))
     (nostr-relay--verify-finished)))
  'pending-verification)

(defun nostr-relay--handle-event (url _sub-id event)
  "Handle EVENT from URL without blocking relay IO.
When `nostr-relay-verify-events' is non-nil, verification subprocesses are
bounded by `nostr-relay-max-concurrent-verifications'; events arriving while
at the cap are queued (never dropped) and dispatched as verifications finish."
  (if (not nostr-relay-verify-events)
      (nostr-relay--store-verified-event url event)
    (if (>= nostr-relay--verify-inflight
            nostr-relay-max-concurrent-verifications)
        (progn
          (setq nostr-relay--verify-queue
                (nconc nostr-relay--verify-queue (list (cons url event))))
          'pending-verification)
      (nostr-relay--start-verification url event))))

(defun nostr-relay--maybe-store-notification (event)
  "Store notifications caused by EVENT for `nostr-current-pubkey'."
  (when (and (boundp 'nostr-current-pubkey) nostr-current-pubkey)
    (let ((event-id (alist-get 'id event))
          (pubkey (alist-get 'pubkey event))
          (created-at (or (alist-get 'created_at event)
                          (alist-get 'created-at event))))
      (pcase (alist-get 'kind event)
        (1
         (cond
          ((member nostr-current-pubkey (nostr-event-mentioned-pubkeys event))
           (nostr-db-store-notification
            (format "%s-mention" event-id) "mention" event-id pubkey nostr-current-pubkey created-at))
          ((and (alist-get 'reply-id event)
                (equal (nostr-db-event-pubkey (alist-get 'reply-id event)) nostr-current-pubkey))
           (nostr-db-store-notification
            (format "%s-reply" event-id) "reply" event-id pubkey nostr-current-pubkey created-at))))
        (7
         (when-let* ((target (nostr-event-reaction-event-id event)))
           (when (equal (nostr-db-event-pubkey target) nostr-current-pubkey)
             (nostr-db-store-notification
              (format "%s-reaction" event-id) "reaction" event-id pubkey nostr-current-pubkey created-at))))
        (6
         (when-let* ((target (nostr-event-repost-event-id event)))
           (when (equal (nostr-db-event-pubkey target) nostr-current-pubkey)
             (nostr-db-store-notification
              (format "%s-repost" event-id) "repost" event-id pubkey nostr-current-pubkey created-at))))
        (9735
         (when-let* ((target (nostr-event-zap-target-event-id event)))
           (when (equal (nostr-db-event-pubkey target) nostr-current-pubkey)
             (nostr-db-store-notification
              (format "%s-zap" event-id) "zap" event-id pubkey nostr-current-pubkey created-at))))
        (3
         (when (member nostr-current-pubkey (nostr-event-mentioned-pubkeys event))
           (nostr-db-store-notification
            (format "%s-follow" event-id) "follow" event-id pubkey nostr-current-pubkey created-at)))))))

(defun nostr-relay-handle-frame (url payload)
  "Handle relay frame PAYLOAD from URL."
  (pcase (json-parse-string payload :object-type 'alist :array-type 'list :false-object nil)
    (`("EVENT" ,sub-id ,event)
     (nostr-relay--handle-event url sub-id event))
    (`("EOSE" ,sub-id)
     (nostr-db-store-relay-status url "eose" sub-id)
     (nostr-relay--note-profile-eose sub-id)
     (nostr-relay--handle-eose url sub-id))
    (`("NOTICE" . ,rest)
     (nostr-db-store-relay-status url "notice" (car rest)))
    (`("CLOSED" ,sub-id . ,rest)
     (remhash sub-id nostr-relay--subscriptions)
     (nostr-relay--note-profile-eose sub-id)
     (nostr-db-store-relay-status url "closed" (car rest)))
    (`("OK" ,event-id ,accepted . ,rest)
     (let ((message (car rest)))
       (nostr-db-store-publish-receipt
        event-id
        url
        (if accepted "accepted" "rejected")
        message)
       (nostr-db-store-relay-status
        url
        (if accepted "ok" "rejected")
        (format "%s %s" event-id (or message "")))))
    (_ nil)))

(defun nostr-relay--since-for-pubkeys (pubkeys)
  "Return a since timestamp for PUBKEYS with overlap."
  (when-let* ((latest (nostr-db-latest-event-time pubkeys)))
    (max 0 (- latest nostr-relay-since-overlap-seconds))))

(defun nostr-relay--personal-filters (pubkey)
  "Return personal activity filters for PUBKEY.
Nostr filter fields are ANDed, so authored events and mentions must be separate
filters or kind-0 metadata will never match."
  `((("kinds" . (,nostr-kind-metadata
                 ,nostr-kind-contacts
                 ,nostr-kind-text-note
                 ,nostr-kind-repost
                 ,nostr-kind-reaction
                 ,nostr-kind-zap-receipt
                 ,nostr-kind-relay-list))
     ("authors" . (,pubkey))
     ("limit" . ,nostr-default-feed-limit))
    (("#p" . (,pubkey))
     ("limit" . ,nostr-default-feed-limit))))

(defun nostr-relay--contacts-filter (pubkey)
  "Return contact-list filter for PUBKEY."
  `(("kinds" . (,nostr-kind-contacts ,nostr-kind-relay-list))
    ("authors" . (,pubkey))
    ("limit" . 10)))

(defun nostr-relay--feed-filter (pubkeys)
  "Return follows feed filter for PUBKEYS."
	  (delq nil
	        `(("kinds" . (,nostr-kind-metadata
	                      ,nostr-kind-text-note
	                      ,nostr-kind-repost
	                      ,nostr-kind-reaction
	                      ,nostr-kind-zap-receipt))
          ("authors" . ,pubkeys)
          ,(when-let* ((since (nostr-relay--since-for-pubkeys pubkeys)))
             `("since" . ,since))
          ("limit" . ,nostr-default-feed-limit))))

(defun nostr-relay-subscribe-personal (url pubkey)
  "Subscribe URL to personal events for PUBKEY."
  (nostr-relay-subscribe
   url
   (nostr-relay--sub-id "personal" pubkey)
   (append (nostr-relay--personal-filters pubkey)
           (list (nostr-relay--contacts-filter pubkey)))))

(defun nostr-relay-subscribe-follows-feed (url pubkey)
  "Subscribe URL to cached follows feed for PUBKEY."
  (let ((follows (nostr-db-select-follows pubkey)))
    (when follows
      (nostr-relay-subscribe
       url
       (nostr-relay--sub-id "follows" pubkey follows)
       (list (nostr-relay--feed-filter follows))))))

(defun nostr-relay--handle-eose (url sub-id)
  "Handle EOSE for SUB-ID on URL."
  (when (and (boundp 'nostr-current-pubkey)
             nostr-current-pubkey
             (string-prefix-p "personal-" sub-id))
    (nostr-relay-subscribe-follows-feed url nostr-current-pubkey)))

(defun nostr-relay-open (url pubkey)
  "Open websocket to URL for PUBKEY.
Because the socket is opened with `:nowait t', a one-shot timer guards the
handshake: if the connection has not opened within
`nostr-relay-open-timeout-seconds', the socket is closed and a \"timeout\"
status is stored.  The timer is cancelled once the connection opens."
  (condition-case err
      (let (timeout-timer ws)
        (setq ws (websocket-open
                  url
                  :nowait t
                  :on-message (lambda (_ws frame)
                                (nostr-relay-handle-frame url (websocket-frame-payload frame)))
                  :on-open (lambda (_ws)
                             (when (timerp timeout-timer)
                               (cancel-timer timeout-timer))
                             (nostr-db-store-relay-status url "open")
                             (nostr-relay-subscribe-personal url pubkey)
                             (nostr-relay-subscribe-follows-feed url pubkey))
                  :on-close (lambda (_ws)
                              (when (timerp timeout-timer)
                                (cancel-timer timeout-timer))
                              (nostr-db-store-relay-status url "closed"))))
        (setq timeout-timer
              (run-at-time
               nostr-relay-open-timeout-seconds
               nil
               (lambda ()
                 (when (and (websocket-p ws) (not (websocket-openp ws)))
                   (ignore-errors (websocket-close ws))
                   (nostr-db-store-relay-status
                    url "timeout" "Connection attempt timed out")))))
        (puthash url ws nostr-relay--connections)
        ws)
    (error
     (nostr-db-store-relay-status url "error" (error-message-string err))
     nil)))

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
  "Queue all configured relays for PUBKEY without blocking the caller."
  (nostr-relay--enqueue-connections (nostr-relay-urls-for-pubkey pubkey) pubkey)
  (nostr-relay--ensure-connect-timer))

(defun nostr-relay-connect-recommended (pubkey)
  "Connect missing cached NIP-65 relays for PUBKEY."
  (dolist (url (nostr-relay--preference-urls pubkey nil))
    (unless (gethash url nostr-relay--connections)
      (nostr-relay-open url pubkey))))

(defun nostr-relay-connect-recommended-deferred (pubkey)
  "Queue missing cached NIP-65 relays for PUBKEY without blocking."
  (nostr-relay--enqueue-connections (nostr-relay--preference-urls pubkey nil) pubkey)
  (nostr-relay--ensure-connect-timer))

(defun nostr-relay-disconnect-all ()
  "Disconnect all active relays."
  (when (timerp nostr-relay--connect-timer)
    (cancel-timer nostr-relay--connect-timer))
  (setq nostr-relay--connect-timer nil
        nostr-relay--connect-queue nil)
  (maphash (lambda (_url ws)
             (when (websocket-openp ws)
               (websocket-close ws)))
           nostr-relay--connections)
	  (clrhash nostr-relay--connections)
	  (clrhash nostr-relay--subscriptions)
	  (clrhash nostr-relay--profile-requests)
	  (clrhash nostr-relay--profile-request-counts)
	  (clrhash nostr-relay--profile-request-subscriptions)
	  (clrhash nostr-relay--event-metadata-requests)
	  (clrhash nostr-relay--event-id-requests)
  (when (timerp nostr-relay--activity-timer)
    (cancel-timer nostr-relay--activity-timer))
  (setq nostr-relay--activity-timer nil
        nostr-relay--ingested-event-count 0
        nostr-relay--verify-inflight 0
        nostr-relay--verify-queue nil)
  (nostr-relay--update-mode-line))

(defun nostr-relay-send-client-message (client-message)
  "Broadcast relay-ready CLIENT-MESSAGE string to selected open relays.
When the current account has cached NIP-65 write relays, publish only to those
open relays.  Otherwise publish to every open relay."
  (let ((write-urls (and (boundp 'nostr-current-pubkey)
                         nostr-current-pubkey
                         (nostr-relay--preference-urls nostr-current-pubkey 'write)))
        (event-id (nostr-relay--client-message-event-id client-message))
        (sent 0))
    (maphash (lambda (url ws)
               (when (and (websocket-openp ws)
                          (or (not write-urls) (member url write-urls)))
                 (when event-id
                   (nostr-db-store-publish-receipt event-id url "pending"))
                 (websocket-send-text ws client-message)
                 (setq sent (1+ sent))))
             nostr-relay--connections)
    sent))

(defun nostr-relay-search (query &optional limit)
  "Request NIP-50 text search QUERY from connected relays."
  (let ((sub-id (format "search-%s" (md5 query)))
        (filter `(("kinds" . (,nostr-kind-text-note))
                  ("search" . ,query)
                  ("limit" . ,(or limit 50)))))
    (maphash (lambda (url _ws)
               (nostr-relay-subscribe url sub-id (list filter)))
             nostr-relay--connections)
    sub-id))

(defun nostr-relay-fetch-author (pubkey &optional limit)
  "Request cached profile and public activity for author PUBKEY."
  (let ((sub-id (format "author-%s" (substring (md5 pubkey) 0 12)))
        (filter `(("kinds" . (,nostr-kind-metadata
                              ,nostr-kind-contacts
                              ,nostr-kind-text-note
                              ,nostr-kind-repost
                              ,nostr-kind-reaction
                              ,nostr-kind-zap-receipt
                              ,nostr-kind-relay-list))
                  ("authors" . (,pubkey))
                  ("limit" . ,(or limit 50)))))
    (maphash (lambda (url _ws)
               (nostr-relay-subscribe url sub-id (list filter)))
             nostr-relay--connections)
    sub-id))

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
      (if url
          (when (gethash url nostr-relay--connections)
            (nostr-relay-subscribe url sub-id (list filter))
            (setq sent 1))
        (maphash (lambda (relay-url _ws)
                   (nostr-relay-subscribe relay-url sub-id (list filter))
                   (setq sent (1+ sent)))
                 nostr-relay--connections))
      (nostr-relay--track-profile-request pubkey sub-id sent)
      sent)))

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
      (let ((sub-id (nostr-relay--sub-id "event-meta" ids))
            (filter `(("kinds" . (,nostr-kind-text-note
                                  ,nostr-kind-repost
                                  ,nostr-kind-reaction
                                  ,nostr-kind-zap-receipt))
                      ("#e" . ,ids)
                      ("limit" . ,(or limit (* 4 (length ids)))))))
        (maphash (lambda (url _ws)
                   (nostr-relay-subscribe url sub-id (list filter))
                   (setq sent (1+ sent)))
                 nostr-relay--connections)
        (when (> sent 0)
          (dolist (event-id ids)
            (puthash event-id now nostr-relay--event-metadata-requests)))))
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
        (maphash (lambda (url _ws)
                   (nostr-relay-subscribe url sub-id (list filter))
                   (setq sent (1+ sent)))
                 nostr-relay--connections)
        (when (> sent 0)
          (dolist (event-id ids)
            (puthash event-id now nostr-relay--event-id-requests)))))
    sent))

(provide 'nostr-relay)
;;; nostr-relay.el ends here
