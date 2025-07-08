;;; nostr.el --- A simple nostr client               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr
;; Version: 0.1
;; Package-Requires: ((emacs "30.1")
;;                    (bech32 "0.1")
;;                    (transient "0.3")
;;                    (emacsql "3.1.1")
;;                    (emacsql-sqlite "1.0.0")
;;                    (websocket "1.13"))

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

;;; Code:

(require 'bech32)
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

(defcustom nostr-relay-urls '("wss://relay.primal.net"
                              "wss://relay.damus.io")
  "The websocket URLs for select relays."
  :type '(repeat string)
  :group 'nostr)

(defcustom nostr-db-path (expand-file-name "nostr-db.sqlite" user-emacs-directory)
  "The path to the nostr sqlite database."
  :type 'string
  :group 'nostr)

(defcustom nostr-private-key-path "~/.nostr-private.gpg"
  "Path to GPG-encrypted private key file.
Which contains a hex or nsec private key."
  :type 'string
  :group 'nostr)

(defcustom nostr-debug-logging t
  "Turns debug messages on or off."
  :type 'boolean
  :group 'nostr)

(defcustom nostr-timestamp-format "%Y-%m-%d %H:%M"
  "Formatting for a notes timestamp."
  :type 'string
  :group 'nostr)

(defcustom nostr-backend 'nostril
  "The preferred nostr backend tool for signing and other things."
  :type '(choice (const nostril))
  :group 'nostr)

(defcustom nostr-since-timeframe 'one-week-ago
  "Timestamp or symbolic timeframe to use for the Nostr `since` filter."
  :type '(choice
          (number :tag "Exact UNIX timestamp")
          (const :tag "One hour ago" one-hour-ago)
          (const :tag "One day ago" one-day-ago)
          (const :tag "One week ago" one-week-ago)
          (const :tag "One month ago" one-month-ago))
  :group 'nostr)

(defconst nostr--buffer-name "*Nostr*")
(defconst nostr--thread-buffer-prefix "*Nostr Thread")
(defconst nostr--kind-metadata 0)
(defconst nostr--kind-text-note 1)
(defconst nostr--kind-contacts 3)
(defconst nostr--kind-dm 4)
(defconst nostr--kind-reaction 7)

(defvar nostr--db nil
  "Global database connection.")

(defvar nostr--relay-connections (make-hash-table :test 'equal)
  "Global map of relay URLs to their websocket connections.")

(defvar nostr--primary-relay (car nostr-relay-urls)
  "The first (primary) relay that will be used in relay recommendations.")

(defvar nostr--subscriptions (make-hash-table :test 'equal)
  "Global map of current subscriptions.")

(defvar nostr--current-pubkey nil
  "The currently loaded account pubkey.")

(defvar nostr--req-contacts-notes-id "contacts-notes")
(defvar nostr--req-contacts-metadata-id "contacts-metadata")
(defvar nostr--req-replies-id-prefix "replies")
(defvar nostr--req-metadata-id-prefix "metadata")
(defvar nostr--req-replies-id-prefix "replies")
(defvar nostr--req-personal-id "personal")

(defun nostr--since-timestamp-from-timeframe (timeframe)
  "Convert TIMEFRAME symbol to a UNIX timestamp."
  (let ((now (float-time)))
    (truncate
     (pcase timeframe
       ('one-hour-ago (- now (* 60 60)))
       ('one-day-ago (- now (* 60 60 24)))
       ('one-week-ago (- now (* 60 60 24 7)))
       ('one-month-ago (- now (* 60 60 24 7 4)))
       ((and (pred (numberp))
             (pred (<= now)))
        (- now timeframe))
       (_ (- now (* 60 60 24)))))))

(defun nostr-select-since-timeframe ()
  "Prompt the user to select a since timeframe."
  (interactive)
  (let* ((choice (intern
                  (completing-read
                   "Select Nostr timeframe: "
                   '("now" "one-hour-ago" "one-day-ago" "one-week-ago")))))
    (if (or (symbolp choice) (numberp choice))
        (setq nostr-since-timeframe choice)
      (message "Timeframe should be a symbol or a timestamp."))
    (message "Timeframe set to: %s" choice)))

(defun debug-message (fmt &rest args)
  "Wraps call to `(message FMT ARGS)' so output can be toggled on/off."
  (when nostr-debug-logging
    (apply #'message (concat "[nostr] " fmt) args)))

;;; Keys / Account

(defun nostr--load-private-key ()
  "Decrypt and return nostr private key hex string."
  (let* ((ctx (epg-make-context 'OpenPGP))
         (key (string-trim (epg-decrypt-file ctx (expand-file-name nostr-private-key-path) nil))))
    (if (string-prefix-p "nsec" key)
        (pcase (bech32-decode key)
          (`(,_ ,data t) (bech32-data-to-hex data)))
      key)))

(defun nostr--load-pubkey ()
  "Get public key from nostr backend."
  (setq nostr--current-pubkey
        (pcase nostr-backend
          ('nostril (nostr--nostril-get-pubkey)))))

(defun nostr--nostril-get-pubkey ()
  "Call to nostril and get pubkey from a random event."
  (let* ((cmd (list "nostril" "--sec" (nostr--load-private-key)))
         (result (with-temp-buffer
                   (let ((exit-code (apply #'call-process (car cmd) nil t nil (cdr cmd)))
                         (output (buffer-string)))
                     (cons exit-code output)))))
    (if (car result)
        (alist-get 'pubkey
                   (json-parse-string (cdr result) :object-type 'alist :array-type 'list))
      nil)))

;;;; Database

(defun nostr-delete-db ()
  "Wipe the DB."
  (interactive)
  (delete-file nostr-db-path)
  (setq nostr--db nil))

(defun nostr--open-db (db-path)
  "Set global db connection to db at DB-PATH."
  (if (file-exists-p db-path)
      (setq nostr--db (emacsql-sqlite-open db-path))
    (with-temp-buffer (write-file db-path))
    (setq nostr--db (emacsql-sqlite-open db-path))
    (nostr--init-db)))

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
                            relay
                            reply_id
                            (reply_count integer :default 0)
                            root_id])])
  (emacsql nostr--db
           [:create-trigger
            increment_reply_count_on_insert
            :after :insert :on events
            :when (and (= new:kind 1) (or (is-not new:reply_id nil)
                                          (is-not new:root_id nil)))
            :begin
            :update
            events
            :set (= reply_count (+ reply_count 1))
            :where (or (= id new:reply_id)
                       (= id new:root_id))
            :\;
            :end])
  (emacsql nostr--db [:create-index idx_events_created_at :on events ([created_at])])
  (emacsql nostr--db [:create-index idx_events_kind :on events ([kind])])
  (emacsql nostr--db [:create-index idx_events_pubkey :on events ([pubkey])])
  (emacsql nostr--db [:create-index idx_events_reply_id :on events ([reply_id])])
  (emacsql nostr--db [:create-index idx_events_root_id :on events ([root_id])])
  (emacsql nostr--db
           [:create-table users
                          ([(pubkey :primary-key)
                            name
                            about
                            picture])])
  (emacsql nostr--db [:create-table follows ([pubkey contact] (:unique [pubkey contact]))])
  (emacsql nostr--db [:create-index idx_follows_pubkey :on follows ([pubkey])])

  (emacsql nostr--db
           [:create-table reactions
                          ([(id :primary-key)
                            pubkey
                            (created_at integer)
                            event_id
                            content])])
  (emacsql nostr--db
           [:create-index idx_reactions_event_id :on reactions ([event_id])]))

(defun nostr--parse-e-tags-by-marker (tags)
  "Return a plist mapping tag markers to a list of event IDs.
TAGS is a list of e-tags: (\"e\" <id> <relay?> <marker?>)."
  (let (result)
    (dolist (tag tags)
      (when (and (string= (car tag) "e")
                 (nth 1 tag))              ; id exists
        (if-let* ((id (nth 1 tag))
                  (marker-str (nth 3 tag)) ; may not exist
                  (marker (intern (concat ":" (downcase marker-str)))))
            (setq result (plist-put result marker (cons id (plist-get result marker)))))))
    result))

(defun nostr--store-event (relay event)
  "Store a single EVENT (alist) from RELAY into DB."
  (debug-message "storing event in database")
  (let-alist event
    (let* ((parsed-tags (nostr--parse-e-tags-by-marker .tags))
           (root-id (car-safe (plist-get parsed-tags :root)))
           (reply-id (car-safe (plist-get parsed-tags :reply))))
      (emacsql nostr--db
               [:insert :into events
                        :values [$s1 $s2 $s3 $s4 $s5 $s6 $s7 $s8 $s9 $s10 $s11]
                        :on-conflict ([id])
                        :do :update
                        :set [(= pubkey $i12)
                              (= created_at $i13)
                              (= kind $i14)
                              (= tags $i15)
                              (= content $i16)
                              (= sig $i17)
                              (= relay $i18)
                              (= reply_id $i19)
                              (= reply_count $i20)
                              (= root_id $i21)]
                        :where (> $i13 $i22)]
               .id .pubkey .created_at .kind .tags .content .sig relay
               reply-id 0 root-id
               'excluded:pubkey 'excluded:created_at 'excluded:kind 'excluded:tags
               'excluded:content 'excluded:sig 'excluded:relay
               'events:reply_id 'events:reply_count 'events:root_id 'events:created_at))))

(defun nostr--store-reaction (event)
  "Store a Kind 7 reaction EVENT."
  (let-alist event
    (when-let* ((e-tag (seq-find (lambda (tag) (equal (car tag) "e")) .tags))
                (event-id (cadr e-tag)))
      (emacsql nostr--db
               [:insert :into reactions
                        :values [$s1 $s2 $s3 $s4 $s5]
                        :on-conflict ([id]) :do :nothing]
               .id .pubkey .created_at event-id .content))))

(defun nostr--store-follows (pubkey tags)
  "Parse TAGS and store PUBKEYs follows in database."
  ;; delete old follows for this pubkey and re-insert
  (emacsql-with-transaction nostr--db
    (emacsql nostr--db
             [:delete-from follows :where (= pubkey $s1)]
             pubkey)
    (dolist (tag tags)
      (when (and (equal (car tag) "p")
                 (stringp (cadr tag)))
        (emacsql nostr--db
                 [:insert-or-ignore :into follows :values [$s1 $s2]]
                 pubkey (cadr tag))))))

(defun nostr--store-metadata (pubkey content)
  "Parse CONTENT from metadata event for PUBKEY."
  (let-alist (ignore-errors (json-parse-string content :object-type 'alist))
    (emacsql nostr--db
             [:insert-or-replace :into users
                                 :values [$s1 $s2 $s3 $s4]]
             pubkey .name .about .picture)))

(defun nostr--fetch-follows-notes (me since limit root-only)
  "Gets all text notes from follows of ME pubkey SINCE given timestamp and LIMIT.
If ROOT-ONLY is t only root notes are returned."
  (emacsql nostr--db
           `[:select
             [events:id
              events:pubkey
              users:name
              users:picture
              events:created_at
              events:content
              events:tags
              events:reply_id
              events:reply_count
              events:root_id
              [:select [(funcall count *)]
                       :from reactions
                       :where (= event_id events:id)]]
             :from events
             :inner-join follows :on (= events:pubkey follows:contact)
             :left-join users :on (= events:pubkey users:pubkey)
             :where (and (= events:kind 1)
                         (>= events:created_at 2)
                         (= follows:pubkey $s3)
                         ,(if root-only
                              '(is events:root_id nil)
                            'true))
             :order-by [(desc events:created_at)]
             :limit $s1]
           (or limit 50) (or since 0) me))

(defun nostr--fetch-event-ids (limit)
  "Fetch latest LIMIT number of events."
  (seq-map #'car
           (emacsql nostr--db
                    [:select [events:id]
                             :from events
                             :order-by [(desc events:created_at)]
                             :limit $s1]
                    limit)))

(defun nostr--fetch-replies-from-db (note-id)
  "Fetching replies of a NOTE-ID."
  (emacsql nostr--db
           [:select
            [events:id
             events:pubkey
             users:name
             users:picture
             events:created_at
             events:content
             events:tags
             events:reply_id
             events:reply_count
             events:root_id
             [:select [(funcall count *)]
                      :from reactions
                      :where (= event_id events:id)]]
            :from events
            :left-join users :on (= events:pubkey users:pubkey)
            :where (or (and (= events:reply_id $s1)
                            (not (= events:root_id $s1)))
                       (and (= events:root_id $s1)
                            (is events:reply_id nil)))
            :order-by [(asc events:created_at)]]
           note-id))

(defun nostr--fetch-follows (pubkey)
  "Fetch follows for PUBKEY."
  (flatten-list
   (emacsql nostr--db
            [:select contact :from follows :where (= pubkey $s1)]
            pubkey)))

;;; Frame Handling

(defun nostr--handle-frame (relay frame)
  "Handle a FRAME sent from RELAY."
  (let ((data (json-parse-string frame :object-type 'alist :array-type 'list :false-object nil)))
    (pcase data
      (`("EVENT" ,sub-id ,event)        ; a regular nostr event
       (nostr--handle-event relay sub-id event))
      (`("EOSE" ,sub-id)                ; end of stored events
       (nostr--handle-eose relay sub-id))
      (`("NOTICE" ,msg)                 ; human readable error message for client
       (nostr--handle-notice msg))
      (`("CLOSED" ,sub-id ,msg)         ; subscription closed server-side
       (nostr--handle-closed sub-id msg))
      (`("OK" ,event-id ,accepted ,msg) ; result of event being sent to relay
       (nostr--handle-ok event-id accepted msg))
      (_
       (cons 'unknown data))))
  t)

(defun nostr--handle-event (relay _sub-id event)
  "Handles EVENT from SUB-ID on RELAY."
  (when (nostr--validate-event event)
    (let-alist event
      (pcase .kind
        ;; Kind 0 — Metadata (replaceable)
        (0 (nostr--store-metadata .pubkey .content))
        ;; Kind 3 — Contact list (replaceable)
        (3 (nostr--store-follows .pubkey .tags))
        ;; Kind 1 — Text note
        (1 (nostr--store-event relay event))
        ;; Kind 7 - Reactions
        (7 (nostr--store-reaction event))
        ;; Default: ignore it, but log
        (_
         (debug-message "skipping event %s" event))))))

(defun nostr--validate-event (_event)
  "Validates the signature on an EVENT.  Returns true if valid."
  ;; TODO: call out to a cli program to validate
  t)

(defun nostr--event-row-to-alist (row)
  "Convert event ROW to a alist or nil if it doesn't match."
  (pcase row
    (`(,id ,pubkey ,author ,pic ,created-at ,content ,tags
           ,reply-id ,replies ,root-id ,likes)
     `((id . ,id) (pubkey . ,pubkey) (author . ,author) (pic . ,pic)
       (created-at . ,created-at) (content . ,content) (tags . ,tags)
       (reply-id . ,reply-id) (replies . ,replies) (root-id . ,root-id)
       (likes . ,likes)))))

(defun nostr--handle-eose (relay sub-id)
  "Handles EOSE for SUB-ID on RELAY.
Sort of a chain reaction where some subscriptions start only after
others complete."
  (debug-message "EOSE for subscription %s" sub-id)
  (pcase sub-id
    ((pred (string= nostr--req-contacts-notes-id))
     ;; ready to fetch reactions and likes for recent notes
     (let ((ws (gethash relay nostr--relay-connections)))
       (when (websocket-openp ws)
         (debug-message "Opening subscription: %s" sub-id)
         (nostr--subscribe relay ws
                           (list
                            (nostr--req-replies-and-reactions
                             (nostr--fetch-event-ids 100)))))))
    ((pred (string-prefix-p nostr--req-replies-id-prefix))
     (nostr--unsubscribe relay sub-id)
     (nostr-refresh))
    ((pred (string-prefix-p nostr--req-metadata-id-prefix))
     (nostr--unsubscribe relay sub-id)
     )))

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
      (debug-message "OK %s event-id: %s" msg event-id)
    (debug-message "%s NOT ACCEPTED: %s" event-id msg)))

;;; Connections

(defun nostr--open-websocket (url)
  "Open a connection to URL and return the websocket."
  (debug-message "Connecting to relay %s" url)
  (websocket-open
   url
   :on-message (lambda (_ws frame)
                 (let ((payload (websocket-frame-payload frame)))
                   (debug-message "Received frame: %s" payload)
                   (nostr--handle-frame url payload)))
   :on-open (lambda (ws)
              (debug-message "WebSocket opened: %s" url)
              (nostr--subscribe
               url ws
               (let ((pubkey nostr--current-pubkey))
                 (list (nostr--req-personal pubkey)
                       (nostr--req-contacts-notes pubkey)
                       (nostr--req-contacts-metadata pubkey)))))
   :on-close (lambda (_ws)
               (debug-message "Disconnected from relay"))))

(defun nostr--connect-to-all-relays ()
  "Connect to all relays in `nostr-relay-urls`."
  (interactive)
  (dolist (url nostr-relay-urls)
    (unless (gethash url nostr--relay-connections)
      (let ((ws (nostr--open-websocket url)))
        (puthash url ws nostr--relay-connections)))))

(defun nostr--disconnect-all-relays ()
  "Close all connections in `nostr--relay-connections'."
  (interactive)
  (maphash (lambda (_url ws)
             (when (websocket-openp ws)
               (websocket-close ws)))
           nostr--relay-connections)
  (clrhash nostr--relay-connections)
  (debug-message "Disconnected from all relays."))

(defun nostr--subscribe (relay ws reqs)
  "Subscribe to each REQS over websocket WS connection to RELAY."
  (dolist (req reqs)
    (let* ((sub-id (elt req 1))
           (json (json-encode req)))
      (puthash sub-id t nostr--subscriptions)
      (debug-message "Sending subscription %s to relay %s" json relay)
      (websocket-send-text ws json))))

(defun nostr--unsubscribe (relay sub-id)
  "Unsubscribe from SUB-ID on RELAY."
  (let ((ws (gethash relay nostr--relay-connections)))
    (when (websocket-openp ws)
      (debug-message "Closing subscription: %s" sub-id)
      (websocket-send-text ws (json-encode `["CLOSE" ,sub-id]))
      (remhash sub-id nostr--subscriptions))))

(defun nostr--subscribe-to-replies (note-id)
  "Subscribe to events that reference NOTE-ID.
Usually the root note that the user will click to open."
  (let ((req (nostr--req-replies-and-reactions `(,note-id))))
    (maphash
     (lambda (relay ws)
       (nostr--subscribe relay ws (list req)))
     nostr--relay-connections)))

(defun nostr--subscribe-to-metadata (pubkey)
  "Subscribe to metadata for PUBKEY."
  (let ((req (nostr--req-metadata `(,pubkey))))
    (maphash
     (lambda (url ws)
       (nostr--subscribe url ws (list req)))
     nostr--relay-connections)))

(defun nostr--build-event (kind privkey tags content)
  "Delegate event creation to `nostr-backend'.
Using KIND, PRIVKEY, TAGS and CONTENT."
  (pcase nostr-backend
    ('nostril (nostr--nostril-build-event kind privkey tags content))))

(defun nostr--nostril-flatten-tags (tags)
  "Convert nested TAGS list into a flat list of --tagn args for nostril."
  (apply #'append
         (mapcar (lambda (tag)
                   (let ((n (length tag)))
                     (append (list "--tagn" (number-to-string n)) tag)))
                 tags)))

(defun nostr--nostril-build-event (kind privkey tags content)
  "Call to nostril to build and sign an event.
Using KIND, PRIVKEY, TAGS and CONTENT.  Returns nil if not succesful."
  (let* ((args (append (list "nostril"
                             "--envelope"
                             "--kind" (number-to-string kind)
                             "--content" content
                             "--sec" privkey)
                       (nostr--nostril-flatten-tags tags))))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process (car args) nil t nil (cdr args)))
            (output (buffer-string)))
        (if exit-code
            output
          (debug-message "couldn't create event using nostril"))))))

(defun nostr--send-event (ws event)
  "Sends EVENT object as json to WS."
  (debug-message "Sending event %s to relay %s" event ws)
  (websocket-send-text ws (json-encode `["EVENT" ,event])))

;;; Requests

(defun nostr--req-personal (pubkey)
  "Build a request for PUBKEY's metadata, follows and personal feed."
  `["REQ" ,nostr--req-personal-id
    (("kinds" . (,nostr--kind-metadata ,nostr--kind-contacts ,nostr--kind-text-note))
     ("authors" . (,pubkey))            ; pubkey's posts
     ("#p" . (,pubkey)))]) ; mentions of pubkey

(defun nostr--req-contacts-notes (pubkey)
  "Build a request for PUBKEY to get its contacts' notes.
Filter includes SINCE timestamp which should be the relay's last fetched
time."
  `["REQ" ,nostr--req-contacts-notes-id
    (("kinds" . (,nostr--kind-text-note))
     ("authors" . ,(nostr--fetch-follows pubkey))
     ("since" . ,(nostr--since-timestamp-from-timeframe nostr-since-timeframe))
     ("limit" . 100))])

(defun nostr--req-contacts-metadata (pubkey)
  "Build a request for PUBKEY to get its contacts' metadata."
  `["REQ" ,nostr--req-contacts-metadata-id
    (("kinds" . (,nostr--kind-metadata))
     ("authors" . ,(nostr--fetch-follows pubkey)))])

(defun nostr--req-metadata (pubkeys)
  "Build a request to fetch metadata for PUBKEYS."
  `["REQ" ,(format "%s-%s" nostr--req-metadata-id-prefix
                   (md5 (prin1-to-string pubkeys)))
    (("kinds" . (,nostr--kind-metadata))
     ("authors" . ,pubkeys))])

(defun nostr--req-replies-and-reactions (event-ids)
  "Build a subscription REQ to get replies and reactions to EVENT-IDS.
Filters SINCE given unix timestamp."
  `["REQ" ,(format "%s-%s" nostr--req-replies-id-prefix
                   (md5 (prin1-to-string event-ids)))
    (("kinds" . (,nostr--kind-text-note ,nostr--kind-reaction))
     ("#e" . ,event-ids)
     ("since" . ,(nostr--since-timestamp-from-timeframe nostr-since-timeframe))
     ("limit" . 100))])

(defvar nostr--req-metadata-id-prefix "metadata")
(defun nostr--req-pubkey-metadata (pubkey)
  "Build a request to get PUBKEY's metadata."
  `["REQ" ,(format "%s-%s" nostr--req-metadata-id-prefix pubkey)
    (("kinds" . (,nostr--kind-metadata))
     ("authors" . (,pubkey)))])

;;;; UI

(defvar nostr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-refresh)
    (define-key map (kbd "?") #'nostr-ui-popup-actions)
    (define-key map (kbd "r") #'nostr-reply-to-note)
    (define-key map (kbd "c") #'nostr-create-note)
    (define-key map (kbd "RET") #'nostr-open-thread)
    map)
  "Keymap for `nostr-mode'.")

(define-derived-mode nostr-mode tabulated-list-mode "Nostr"
  "Major mode for viewing Nostr notes in a list."
  (setq tabulated-list-format `[("Author" 20 t . (:right-align t))
                                ("⏲" ,(length (nostr--format-timestamp 0)) t . (:right-align t))
                                ("↩" 3 nil . (:right-align t))
                                ("❤" 3 nil . (:right-align t :pad-right 2))
                                ("Content" 0 nil)])
  (setq tabulated-list-padding 2)
  (hl-line-mode)
  (add-hook 'tabulated-list-revert-hook #'nostr-refresh nil t)
  (tabulated-list-init-header))

(defun nostr--format-timestamp (unix-time)
  "Convert UNIX-TIME seconds to formatted timestamp string."
  (format-time-string nostr-timestamp-format (seconds-to-time unix-time)))

(defun nostr--format-content (content)
  "Replace newlines with spaces in CONTENT for compact display."
  (replace-regexp-in-string "\n" " " content))

(defun nostr--refresh-list-view ()
  "Refresh main list view."
  (let ((inhibit-read-only t)
        (events (nostr--fetch-follows-notes nostr--current-pubkey nil 100 t)))
    (setq-local
     tabulated-list-entries
     (seq-map
      (lambda (e)
        (let ((event (nostr--event-row-to-alist e)))
          (let-alist event
            (list
             ;; the key which is also the entire row
             event
             ;; displayed content
             (vector (or .author .pubkey)
                     (nostr--format-timestamp .created-at)
                     (number-to-string .replies)
                     (number-to-string .likes)
                     (nostr--format-content .content))))))
      events))
    (tabulated-list-print t)))

(defun nostr-refresh ()
  "Refresh buffer depending on its type."
  (interactive)
  (cond
   ((eq major-mode 'nostr-mode)
    (nostr--refresh-list-view))
   ((eq major-mode 'nostr-thread-mode)
    (nostr--refresh-thread-view (current-buffer)))))

(transient-define-prefix nostr-ui-popup-actions ()
  "Actions for a selected Nostr note."
  [["Nostr Actions"
    ("r" "Reply to note" nostr-reply-to-note)
    ;; other actions can go here
    ]])

(defun nostr--get-selected-note ()
  "Return data for the currently selected note metadata as an alist."
  (tabulated-list-get-id))

(defvar nostr-note-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'nostr-send-current-note)
    map)
  "Keymap for `nostr-note-mode'.")

(define-derived-mode nostr-note-mode text-mode "Nostr-Note"
  "Major mode for composing a Nostr note or reply.")

(defvar-local nostr--reply-context nil
  "If non-nil, contains the event being replied to.")

(defun nostr--compose (&optional reply-context)
  "Open a new buffer to compose a note.
Optionally pass REPLY-CONTEXT (reply-to event) to
  reply to an event."
  (let ((buf (generate-new-buffer "*Nostr Compose*")))
    (with-current-buffer buf
      (erase-buffer)
      (nostr-note-mode)
      (when reply-context
        (setq nostr--reply-context reply-context)
        (insert (format ";; Replying to %s:\n\n;; %s\n\n"
                        (alist-get 'author reply-context)
                        (truncate-string-to-width
                         (alist-get 'content reply-context) 80 nil nil t))))
      (goto-char (point-max)))
    (switch-to-buffer buf)))

(defun nostr--build-note-tags (reply-to-note)
  "Build tags for a note using REPLY-TO-NOTE tags.
TODO: add t tag (hashtags)"
  ;; NIP-10
  (when reply-to-note
    (let-alist reply-to-note
      (let ((tags '()))
        (cond
         (.root-id
          ;; reply to a reply
          (push `("e" ,.root-id ,nostr--primary-relay "root") tags)
          (push `("e" ,.id ,nostr--primary-relay "reply") tags))
         (t
          ;; reply to root
          (push `("e" ,.id ,nostr--primary-relay "root") tags)))
        (push `("p" ,.pubkey) tags)
        (nreverse tags)))))

(defun nostr--get-note-buffer-content ()
  "Return note content excluding comment lines."
  (let ((lines
         (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))
    (string-trim
     (string-join
      (seq-remove (lambda (line) (string-prefix-p ";;" (string-trim-left line))) lines)
      "\n"))))

(defun nostr-send-current-note ()
  "Collect, sign, and send note in `nostr-note-mode` buffer."
  (interactive)
  (let ((content (nostr--get-note-buffer-content))
        (reply-to nostr--reply-context))
    (unless (string-empty-p content)
      (let* ((privkey (nostr--load-private-key))
             (tags (nostr--build-note-tags reply-to))
             (event (nostr--build-event 1 privkey tags content)))
        ;; try to send
        (when event
          (progn
            (maphash
             (lambda (url ws)
               (when (websocket-openp ws)
                 (websocket-send-text ws event)
                 (debug-message "note sent to relay %s" url)))
             nostr--relay-connections)
            (message "Note sent!")))
        (kill-buffer)))))

(defun nostr-reply-to-note ()
  "Reply to the note at point."
  (interactive)
  (let ((note
         (cond
          ((eq major-mode 'nostr-mode)
           (nostr--get-selected-note))
          ((eq major-mode 'nostr-thread-mode)
           (nostr--get-selected-thread-note)))))
    (when note
      (nostr--compose note))))

(defun nostr-create-note ()
  "Start composing a new note."
  (interactive)
  (nostr--compose))

(defvar nostr-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-refresh)
    (define-key map (kbd "?") #'nostr-ui-popup-actions)
    (define-key map (kbd "r") #'nostr-reply-to-note)
    (define-key map (kbd "C-n") #'nostr-thread-next-entry)
    (define-key map (kbd "C-p") #'nostr-thread-prev-entry)
    map)
  "Keymap for `nostr-thread-mode'.")

(defvar-local nostr--thread-root nil
  "The current thread's root note.")

(define-derived-mode nostr-thread-mode special-mode "Nostr-Thread"
  "Major mode for viewing a Nostr thread with multi-line entries."
  (setq-local truncate-lines nil
              nostr--thread-root nil
              nostr-thread-entries nil
              nostr-thread-highlight-overlay nil)
  (add-hook 'nostr-thread-mode-hook #'nostr--make-depth-faces))

(defvar nostr-thread-depth-faces nil
  "List of hl-line based faces for different thread depths.")

(defun nostr--make-depth-faces ()
  "Generate hl-line derived faces with varying background intensities."
  (let* ((base-bg (face-background 'default nil))
         (num-depths 5)
         (faces nil))
    (dotimes (i num-depths)
      (let* ((intensity (* 0.15 i))
             (color (if base-bg
                        (color-darken-hex base-bg intensity)
                      "#333333"))
             (face-name (intern (format "nostr-thread-depth-%d" i))))
        (make-face face-name)
        (set-face-attribute face-name nil :background color :extend t)
        (push face-name faces)))
    (setq nostr-thread-depth-faces (vconcat (nreverse faces)))))

(defvar-local nostr--thread-entries nil
  "List of entries in the current thread view.
Each entry is a plist: (:start <pos> :end <pos> :event <event>).")

(defun nostr--build-prefix (depth open-branches last-child last-prefix)
  "Return a threaded prefix string.
DEPTH is current depth, OPEN-BRANCHES is a list of t/nil indicating
whether each previous depth has more siblings.  LAST-CHILD determines if
this entry is the last at its depth.  LAST-PREFIX is the
pair of prefixes for the last item."
  (let ((prefix ""))
    (dotimes (i (1- depth))
      (setq prefix (concat prefix (if (nth i open-branches) "│  " "   "))))
    (when (> depth 0)
      (setq prefix (concat prefix (if last-child (car last-prefix) (cdr last-prefix)))))
    prefix))

(defun nostr--insert-threaded-note (note depth last-child open-branches)
  "Insert a threaded NOTE at DEPTH.
LAST-CHILD determines branching, OPEN-BRANCHES tracks vertical guides."
  (let-alist note
    (let* ((start (point))
           (timestamp (nostr--format-timestamp .created-at))
           (prefix (nostr--build-prefix depth open-branches last-child '("└─ " . "├─ ")))
           (face (if (zerop depth) 'bold 'default)))
      (insert (propertize (format "%s%s [%s] [❤ %s]\n"
                                  prefix (or .author .pubkey) timestamp .likes)
                          'face face))
      (let* ((blank-prefix (nostr--build-prefix depth open-branches last-child '("   " . "│  ")))
             (fill-width (- fill-column (length blank-prefix)))
             (wrapped-content
              (with-temp-buffer
                (insert .content)
                (let ((fill-column fill-width)
                      (fill-prefix "")
                      (truncate-lines nil))
                  (goto-char (point-min))
                  (while (not (eobp))
                    (fill-paragraph)
                    (forward-paragraph))
                  (split-string (buffer-string) "\n"))))
             (formatted-content
              (mapconcat (lambda (line)
                           (concat blank-prefix line))
                         wrapped-content
                         "\n")))
        (insert (propertize (format "%s\n%s\n" formatted-content blank-prefix)
                            'face 'default)))
      ;; Add overlay for depth-based background
      (let* ((end (point))
             (face (when (bound-and-true-p nostr-thread-depth-faces)
                     (aref nostr-thread-depth-faces
                           (mod depth (length nostr-thread-depth-faces)))))
             (ov (make-overlay start end)))
        (when face
          (overlay-put ov 'face face)
          (overlay-put ov 'nostr-thread-entry t)))
      (push `(:start ,start :end ,(point) :event ,note) nostr--thread-entries))))

(defvar-local nostr--thread-highlight-overlay nil
  "Overlay to highlight the current entry.")

(defun nostr--thread-highlight-entry (entry)
  "Highlight ENTRY plist in the buffer."
  (let ((start (plist-get entry :start))
        (end (plist-get entry :end)))
    (unless nostr--thread-highlight-overlay
      (setq nostr--thread-highlight-overlay (make-overlay start end)))
    (move-overlay nostr--thread-highlight-overlay start end)
    (overlay-put nostr--thread-highlight-overlay 'face 'highlight)))

(defun nostr--thread-entry-at-point ()
  "Return the entry plist at point."
  (cl-find-if (lambda (e)
                (let ((start (plist-get e :start))
                      (end (plist-get e :end)))
                  (and (>= (point) start) (< (point) end))))
              nostr--thread-entries))

(defun nostr-thread-next-entry ()
  "Move to next entry."
  (interactive)
  (let ((pos (point)))
    (cl-loop for e in (sort
                       nostr--thread-entries
                       (lambda (a b) (< (plist-get a :start) (plist-get b :start))))
             when (> (plist-get e :start) pos)
             return (progn
                      (goto-char (plist-get e :start))
                      (nostr--thread-highlight-entry e)))))

(defun nostr-thread-prev-entry ()
  "Move to previous entry."
  (interactive)
  (let ((pos (point)))
    (cl-loop for e in (sort
                       nostr--thread-entries
                       (lambda (a b) (< (plist-get a :start) (plist-get b :start))))
             when (< (plist-get e :start) pos)
             maximize (plist-get e :start) into best
             finally (when best
                       (goto-char best)
                       (let ((entry (cl-find-if (lambda (e)
                                                  (= (plist-get e :start) best))
                                                nostr--thread-entries)))
                         (nostr--thread-highlight-entry entry))))))

(defun nostr--get-selected-thread-note ()
  "Return the note/event for the currently highlighted entry."
  (let ((entry (nostr--thread-entry-at-point)))
    (plist-get entry :event)))

(defun nostr--build-thread-tree (parent-id &optional depth open-branches)
  "Recursively build a thread from PARENT-ID at DEPTH.
OPEN-BRANCHES is a list of booleans tracking which depths are still active."
  (let ((replies (nostr--fetch-replies-from-db parent-id))
        (depth (or depth 1)))
    (cl-loop
     for i from 0 below (length replies)
     for row = (nth i replies)
     for last = (= i (1- (length replies)))
     do (let* ((event (nostr--event-row-to-alist row))
               (event-id (alist-get 'id event))
               (child-open-branches (append open-branches (list (not last)))))
          (nostr--insert-threaded-note event depth last child-open-branches)
          (nostr--build-thread-tree event-id (1+ depth) child-open-branches)))))

(defun nostr--refresh-thread-view (buf)
  "Refresh the current thread view in BUF.
ROOT is the thread's root."
  (with-current-buffer buf
    (let* ((inhibit-read-only t)
           (root nostr--thread-root)
           (root-id (alist-get 'id root)))
      (setq-local nostr--thread-entries nil)
      (erase-buffer)
      (nostr--insert-threaded-note root 0 nil nil)
      (nostr--build-thread-tree root-id 1 '())
      (goto-char (point-min))
      (when nostr--thread-entries
        (nostr--thread-highlight-entry
         (car (last nostr--thread-entries)))))))

(defun nostr-open-thread ()
  "Open multi-line thread view for the selected note."
  (interactive)
  (when-let* ((root (nostr--get-selected-note))
              (root-id (alist-get 'id root))
              (buf (get-buffer-create
                    (format "%s-%s*" nostr--thread-buffer-prefix root-id))))
    (with-current-buffer buf
      (nostr-thread-mode)
      (setq-local nostr--thread-root root)
      (nostr--refresh-thread-view buf)
      (select-window (display-buffer buf))
      (nostr--subscribe-to-replies root-id))))

;;;###autoload
(defun nostr-open ()
  "Open the Nostr client buffer and display notes."
  (interactive)
  (nostr--load-pubkey)
  (nostr--open-db nostr-db-path)
  (nostr--connect-to-all-relays)
  (let ((buf (get-buffer-create nostr--buffer-name)))
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook #'nostr-close nil t)
      (nostr-mode)
      (nostr-refresh))
    (switch-to-buffer buf)))

(defun nostr-close ()
  "Cleanup when buffer is killed."
  (interactive)
  (nostr--disconnect-all-relays)
  (emacsql-close nostr--db))

(provide 'nostr)
;;; nostr.el ends here
