;;; nostr.el --- A simple nostr client               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Fart

;;; Code:

(require 'json)
(require 'epg)
(require 'tabulated-list)
(require 'transient)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'websocket)

(defgroup nostr nil
  "A simple Nostr client in Emacs."
  :group 'applications)

(defcustom nostr-relay-urls '("wss://relay.damus.io")
  "The websocket URLs for select relays."
  :type '(repeat string)
  :group 'nostr)

(defcustom nostr-db-path (expand-file-name "nostr-db.sqlite" user-emacs-directory)
  "The path to the nostr sqlite database."
  :type 'string
  :group 'nostr)

(defcustom nostr-private-key-path "~/.nostr-private.gpg"
  "Path to GPG-encrypted private key file."
  :type 'string
  :group 'nostr)

(defcustom nostr-debug-logging t
  "Turns debug messages on or off."
  :type 'boolean
  :group 'nostr)

(defconst nostr-buffer-name "*Nostr*")
(defconst nostr--kind-metadata 0)
(defconst nostr--kind-text-note 1)
(defconst nostr--kind-contacts 3)
(defconst nostr--kind-dm 4)

(defvar nostr--db nil
  "Global database connection.")

(defvar nostr--relay-connections (make-hash-table :test 'equal)
  "Global map of relay URLs to their websocket connections.")

(defvar nostr--subscriptions (make-hash-table :test 'equal)
  "Global map of current subscriptions.")

(defun debug-message (fmt &rest args)
  "Wraps call to `(message FMT ARGS)' so output can be toggled on/off."
  (when nostr-debug-logging
    (apply 'message fmt args)))

;;; Keys / Account

(defun nostr-load-private-key ()
  "Decrypt and return your Nostr private key."
  (let ((ctx (epg-make-context 'OpenPGP)))
    (string-trim (epg-decrypt-file ctx nostr-private-key-path))))

;;;; Database

(defun nostr--open-db (db-path)
  "Set global db connection to db at DB-PATH."
  (unless nostr--db
    (unless (or (null db-path)
                (file-exists-p db-path))
      (with-temp-buffer (write-file db-path))
      (setq nostr--db (emacsql-sqlite-open db-path))
      (nostr--init-db))
    (unless nostr--db
      (setq nostr--db (emacsql-sqlite-open db-path)))))

(defun nostr--init-db ()
  "Initialize the database scheme on the database DB."
  (emacsql nostr--db
           [:create-table events
            ([(id :primary-key)
              pubkey
              (created_at integer)
              (kind integer)
              tags
              content
              sig
              relay])])
  (emacsql nostr--db [:create-index idx_events_created_at :on events ([created_at])])
  (emacsql nostr--db [:create-index idx_events_kind :on events ([kind])])
  (emacsql nostr--db [:create-index idx_events_pubkey :on events ([pubkey])])
  (emacsql nostr--db
           [:create-table users
            ([(pubkey :primary-key)
              name
              about
              picture])])
  (emacsql nostr--db
           [:create-table follows
            ([(pubkey :primary-key)])])
  (emacsql nostr--db
           [:create-table relays
            ([(url :primary-key)
              (last_connected_at integer)])]))

(defun nostr--store-event (relay event)
  "Store a single EVENT (alist) from RELAY into DB."
  (let-alist event
    (emacsql nostr--db
             [:insert :into events
              :values [$s1 $s2 $s3 $s4 $s5 $s6 $s7 $s8]
              :on-conflict ([id])
              :do :update
              :set [(= pubkey $i9)
                    (= created_at $i10)
                    (= kind $i11)
                    (= tags $i12)
                    (= content $i13)
                    (= sig $i14)
                    (= relay $i15)]
              :where (> $i10 $i16)]
             .id .pubkey .created_at .kind .tags .content .sig relay
             'excluded:pubkey 'excluded:created_at 'excluded:kind 'excluded:tags
             'excluded:content 'excluded:sig 'excluded:relay 'events:created_at)))

(defun nostr--fetch-user-posts (since)
  "Gets all user posts from the database SINCE given timestamp."
  (emacsql nostr--db
           [:select
            [users:name users:picture events:created_at events:content]
            :from events
            :inner-join users :on (= events:pubkey users:pubkey)
            :where (and (= events:kind 1)
                        (>= events:created_at $s1))
            :order-by [(desc events:created_at)]]
           (or since 0)))

;; TODO: fix this query and make it useful
;; Probably start with joining the tables and making a complete "post"
(defun nostr--fetch-events-from-db ()
  "Fetch events from the database."
  (emacsql nostr--db
           [:select [created_at pubkey content]
            :from events
            :order-by [(desc created_at)]]))

;;; Frame Handling

(defun nostr--handle-frame (relay frame)
  "Handle a FRAME sent from RELAY."
  (let ((data (json-parse-string frame :object-type 'alist :array-type 'list)))
    (pcase data
      (`("EVENT" ,sub-id ,event)        ; a regular nostr event
       (nostr--handle-event relay sub-id event)
       ;; todo: update ui or return some command?
       )
      (`("EOSE" ,sub-id)                ; end of stored events
       (nostr--handle-eose sub-id))
      (`("NOTICE" ,msg)                 ; human readable error message for client
       (nostr--handle-notice msg))
      (`("CLOSED" ,sub-id ,msg)         ; subscription closed server-side
       (nostr--handle-closed sub-id msg))
      (`("OK" ,event-id ,accepted ,msg) ; result of event being sent to relay
       (nostr--handle-ok event-id accepted msg))
      (_
       (cons 'unknown data))))
  t)

(defun nostr--handle-event (relay sub-id event)
  "Handles EVENT from SUB-ID on RELAY."
  (when (nostr--validate-event event)
    (let-alist event
      (pcase .kind
        ;; Kind 0 — Metadata (replaceable)
        (0
         (let-alist (ignore-errors (json-parse-string .content :object-type 'alist))
           (emacsql nostr--db
                    [:insert-or-replace :into users
                     :values [$s1 $s2 $s3 $s4]]
                    ..pubkey .name .about .picture)))

        ;; Kind 3 — Contact list (replaceable)
        (3
         ;; Just delete old contacts for this pubkey and re-insert
         (emacsql nostr--db
                  [:delete-from follows :where (= pubkey $s1)]
                  .pubkey)
         (dolist (tag .tags)
           (when (and (equal (car tag) "p")
                      (stringp (cadr tag)))
             (emacsql nostr--db
                      [:insert :into follows :values [$s1]]
                      (cadr tag)))))

        ;; Kind 1 — Text note
        (1
         (nostr--store-event relay event))

        ;; Default: ignore it, but log
        (_
         (debug-message "skipping event %s" event))))

    (debug-message "Handled event kind %s from %s" (alist-get 'kind event) relay)))

(defun nostr--validate-event (event)
  "Validates the signature on an EVENT.  Returns true if valid."
  ;; TODO: call out to a cli program to validate
  t)

(defun nostr--handle-eose (sub-id)
  "Handles an EOSE message for SUB-ID in `nostr--subscriptions'."
  (debug-message "EOSE for subscription %s" sub-id))

(defun nostr--handle-notice (msg)
  "Handles a notice MSG."
  ;; Just log out for now
  (debug-message "NOTICE %s" msg))

(defun nostr--handle-closed (sub-id msg)
  "Handles a closed SUB-ID and outputs debug MSG."
  (debug-message "CLOSED %s: %s" sub-id msg)
  (remhash sub-id nostr--subscriptions))

(defun nostr--handle-ok (event-id accepted msg)
  "Handles an ok MSG for an EVENT-ID and returns whether the event was ACCEPTED."
  ;; Just log out for now
  (if accepted
      (debug-message "%s OK %s" event-id msg)
    (debug-message "%s NOT ACCEPTED: %s" event-id msg)))

;;; Connections

(defun nostr--open-websocket (url)
  "Open a connection to URL and return the websocket."
  (debug-message "[nostr] Connecting to relay %s" url)
  (setq nostr-ws
        (websocket-open
         nostr-relay-url
         :on-message (lambda (_ws frame)
                       (let ((payload (websocket-frame-payload frame)))
                         (debug-message "Received frame: %s" payload)
                         (nostr--handle-frame payload)))
         :on-open (lambda (ws)
                    (debug-message "WebSocket opened: %s" url)
                    (nostr--subscribe url ws req))
         :on-close (lambda (_ws)
                     (debug-message "[nostr] Disconnected from relay")))))

(defun nostr--connect-to-all-relays ()
  "Connect to all relays in `nostr-relay-urls`."
  (dolist (url nostr-relay-urls)
    (unless (gethash url nostr--relay-connections)
      (let ((ws (nostr--open-websocket url)))
        (puthash url ws nostr--relay-connections)))))

(defun nostr--generate-sub-id ()
  "Generate a random subscription id to sent to the relay."
  ;; TODO: check how others do this
  (substring (md5 (format "%s" (current-time))) 0 10))

(defun nostr--subscribe (relay ws reqs)
  "Send a subscription REQ for KINDS over websocket WS connection to RELAY, limited by LIMIT."
  (dolist (req-fn reqs)
    (let ((json (json-encode (req-fn))))
      (puthash sub-id t nostr--subscriptions)
      (debug-message "Sending subscription %s to relay %s" json relay)
      (websocket-send-text ws json))))

(defun nostr--unsubscribe (ws sub-id)
  "Sends CLOSE message to unsubscribe from subscription SUB-ID on WS."
  (debug-message "Unsubscribing from subscription %s" sub-id)
  (websocket-send-text ws (json-encode `["CLOSE" ,sub-id])))

(defun nostr--send-event (ws event)
  "Sends EVENT object as json to WS."
  (debug-message "Sending event %s to relay %s" event ws)
  (websocket-send-text ws (json-encode `["EVENT" ,event])))

;;; Requests

(defun nostr--req-personal (pubkey)
  "Build a request for PUBKEY's metadata, contacts and personal feed."
  `["REQ" ,(nostr--generate-sub-id)
    (("kinds" . (,nostr--kind-metadata ,nostr--kind-contacts))
     ("authors" . (,pubkey)))])

(defun nostr--req-contacts-posts (pubkey)
  "Build a request for PUBKEY to get its contacts' posts."
  `["REQ" ,(nostr--generate-sub-id)
    (("kinds" . (,nostr--kind-text-note))
     ("authors" . (,pubkey)))])

(defun nostr--req-simple-global-feed (since &optional limit)
  "Build a request to fetch recent public notes, since unix timestamp SINCE with an optional LIMIT."
  `["REQ" ,(nostr--generate-sub-id)
    (("kinds" . (1))
     ("since" . ,since)
     ("limit" . ,(or limit 20)))])

;;;; UI

;;; Interactive

(defun nostr-open ()
  "Open the Nostr client buffer."
  (interactive)
  (unless nostr-db
    (nostr--open-db nostr-db-path))
  (let ((buf (get-buffer-create nostr-buffer-name)))
    (with-current-buffer buf
      (nostr-mode)
      (local-set-key (kbd "g") #'nostr--connect-to-all-relays)
      (local-set-key (kbd "?") nil)) ; TODO: popup menu
    (switch-to-buffer buf)
    (nostr-refresh)))

(provide 'nostr)
;;; nostr.el ends here
