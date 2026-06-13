;;; nostr-discover.el --- Provider-backed Discover feeds -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Server-ranked public discovery feeds.  The first provider is Primal's cache
;; websocket, which supports ranked Explore requests that ordinary Nostr relay
;; filters cannot express.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'websocket)
(require 'nostr-db)
(require 'nostr-event)
(require 'nostr-relay)

(defcustom nostr-discover-provider 'primal
  "Provider used for the Discover feed."
  :type '(choice (const :tag "Primal cache" primal))
  :group 'nostr)

(defcustom nostr-discover-primal-cache-url "wss://cache2.primal.net/v1"
  "Primal cache websocket URL used by Discover."
  :type 'string
  :group 'nostr)

(defcustom nostr-discover-limit 20
  "Number of Discover notes requested per page."
  :type 'integer
  :group 'nostr)

(defcustom nostr-discover-scope "global"
  "Primal Discover scope."
  :type '(choice (const "global") (const "network") (const "follows") string)
  :group 'nostr)

(defcustom nostr-discover-timeframe "trending"
  "Primal Discover timeframe."
  :type '(choice (const "trending") (const "popular") (const "latest")
                 (const "mostzapped") string)
  :group 'nostr)

(defvar nostr-discover-finished-hook nil
  "Hook run after a Discover request reaches EOSE or fails.")

(defvar nostr-discover--loading nil
  "Non-nil while a Discover request is active.")

(defvar nostr-discover--last-status nil
  "Last Discover status plist.")

(defvar nostr-discover--active-websocket nil
  "Current Discover websocket, when one is active.")

(defun nostr-discover--provider-name ()
  "Return the current Discover provider name for DB keys."
  (symbol-name nostr-discover-provider))

(defun nostr-discover--feed-key ()
  "Return the current Discover feed key."
  (list (nostr-discover--provider-name)
        nostr-discover-scope
        nostr-discover-timeframe))

(defun nostr-discover-status-line ()
  "Return a compact human-readable Discover status line."
  (let* ((status nostr-discover--last-status)
         (state (plist-get status :state))
         (message (plist-get status :message))
         (refreshed-at (plist-get status :refreshed-at))
         (time-text (and refreshed-at
                         (format-time-string "%H:%M" (seconds-to-time refreshed-at)))))
    (string-join
     (delq nil
           (list
            (format "Primal %s" nostr-discover-timeframe)
            "24h"
            (when time-text (format "refreshed %s" time-text))
            (when nostr-discover--loading "loading")
            (when (and state (not (eq state 'ok)))
              (format "%s%s" state (if message (format ": %s" message) "")))))
     " · ")))

(defun nostr-discover--payload (&optional cursor)
  "Return a Primal cache Explore payload using optional CURSOR."
  (let ((payload `((timeframe . ,nostr-discover-timeframe)
                   (scope . ,nostr-discover-scope)
                   (limit . ,nostr-discover-limit)
                   (created_after . ,(- (truncate (float-time)) (* 60 60 24))))))
    (when (and (boundp 'nostr-current-pubkey) nostr-current-pubkey)
      (push `(user_pubkey . ,nostr-current-pubkey) payload))
    (when cursor
      (push `(until . ,cursor) payload))
    payload))

(defun nostr-discover--request-message (&optional cursor)
  "Return the Primal cache websocket request for optional CURSOR."
  (json-encode
   `["REQ" "nostr-discover"
     ((cache . ["explore" ,(nostr-discover--payload cursor)]))]))

(defun nostr-discover--parse-json-content (content)
  "Parse JSON CONTENT into an alist, returning nil on failure."
  (when (stringp content)
    (ignore-errors
      (json-parse-string content
                         :object-type 'alist
                         :array-type 'list
                         :false-object nil
                         :null-object nil))))

(defun nostr-discover--stats-cursor (event-id)
  "Return the pagination cursor for EVENT-ID from cached provider stats."
  (or (caar (emacsql nostr-db--connection
                     [:select [score24h]
                              :from discover_stats
                              :where (= event_id $s1)
                              :limit 1]
                     event-id))
      (caar (emacsql nostr-db--connection
                     [:select [created_at]
                              :from events
                              :where (= id $s1)
                              :limit 1]
                     event-id))))

(defun nostr-discover--store-page (events order append)
  "Persist Discover EVENTS and ORDER.
When APPEND is non-nil, ranks continue after the existing cached page."
  (pcase-let ((`(,provider ,scope ,timeframe) (nostr-discover--feed-key)))
    (unless append
      (nostr-db-clear-discover-results provider scope timeframe))
    (let* ((fetched-at (truncate (float-time)))
           (base-rank (if append
                          (nostr-db-discover-max-rank provider scope timeframe)
                        0))
           (ordered (delete-dups (seq-filter (lambda (id) (member id events))
                                             (or order events))))
           (rank base-rank))
      (dolist (event-id ordered)
        (setq rank (1+ rank))
        (nostr-db-store-discover-result
         provider scope timeframe event-id rank
         (nostr-discover--stats-cursor event-id)
         fetched-at)))))

(defun nostr-discover--finish (state &optional message)
  "Finish the active Discover request with STATE and optional MESSAGE."
  (setq nostr-discover--loading nil
        nostr-discover--last-status
        (list :state state
              :message message
              :refreshed-at (truncate (float-time))))
  (run-hooks 'nostr-discover-finished-hook))

(defun nostr-discover--handle-event (url event state)
  "Handle one Primal cache EVENT from URL, mutating STATE."
  (pcase (alist-get 'kind event)
    (1
     (push (alist-get 'id event) (plist-get state :events))
     (nostr-relay--handle-event url "nostr-discover" event))
    (0
     (nostr-relay--handle-event url "nostr-discover" event))
    (9735
     (nostr-relay--handle-event url "nostr-discover" event))
    (10000100
     (when-let* ((stats (nostr-discover--parse-json-content
                         (alist-get 'content event))))
       (nostr-db-store-discover-stats stats)))
    (10000113
     (when-let* ((paging (nostr-discover--parse-json-content
                          (alist-get 'content event)))
                 (elements (alist-get 'elements paging)))
       (plist-put state :order elements)))))

(defun nostr-discover--handle-frame (url payload state append)
  "Handle one Primal cache websocket PAYLOAD from URL.
STATE accumulates the page; APPEND controls final persistence behavior."
  (pcase (ignore-errors
           (json-parse-string payload
                              :object-type 'alist
                              :array-type 'list
                              :false-object nil
                              :null-object nil))
    (`("EVENT" ,_sub-id ,event)
     (nostr-discover--handle-event url event state))
    (`("EOSE" . ,_)
     (nostr-discover--store-page
      (nreverse (plist-get state :events))
      (plist-get state :order)
      append)
     (when (and (websocket-p nostr-discover--active-websocket)
                (websocket-openp nostr-discover--active-websocket))
       (websocket-close nostr-discover--active-websocket))
     (setq nostr-discover--active-websocket nil)
     (nostr-discover--finish 'ok))
    (`("NOTICE" . ,rest)
     (nostr-discover--finish 'error (string-join (mapcar (lambda (item)
                                                            (format "%s" item))
                                                          rest)
                                                 " ")))
    (_ nil)))

(defun nostr-discover-refresh (&optional append)
  "Refresh the Discover cache.
When APPEND is non-nil, request the next provider-ranked page."
  (interactive)
  (unless (eq nostr-discover-provider 'primal)
    (user-error "Unsupported Discover provider: %s" nostr-discover-provider))
  (let* ((url nostr-discover-primal-cache-url)
         (cursor (and append
                      (pcase-let ((`(,provider ,scope ,timeframe)
                                   (nostr-discover--feed-key)))
                        (nostr-db-discover-next-cursor provider scope timeframe))))
         (state (list :events nil :order nil)))
    (when (and append (not cursor))
      (user-error "No Discover page cursor available"))
    (setq nostr-discover--loading t
          nostr-discover--last-status
          (list :state 'loading
                :message nil
                :refreshed-at (plist-get nostr-discover--last-status :refreshed-at)))
    (when (and (websocket-p nostr-discover--active-websocket)
               (websocket-openp nostr-discover--active-websocket))
      (websocket-close nostr-discover--active-websocket))
    (condition-case err
        (setq nostr-discover--active-websocket
              (websocket-open
               url
               :nowait t
               :on-open (lambda (ws)
                          (websocket-send-text ws
                                               (nostr-discover--request-message cursor)))
               :on-message (lambda (_ws frame)
                             (nostr-discover--handle-frame
                              url (websocket-frame-payload frame) state append))
               :on-close (lambda (_ws)
                           (when nostr-discover--loading
                             (nostr-discover--finish 'closed)))))
      (error
       (nostr-discover--finish 'error (error-message-string err))))))

(provide 'nostr-discover)
;;; nostr-discover.el ends here
