;;; nostr.el --- A simple nostr client               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr
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
                              "wss://relay.damus.io"
                              "wss://nos.lol")
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
  (pcase nostr-backend
    ('nostril (nostr--nostril-get-pubkey))))

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
           [:create-table relays
                          ([(url :primary-key)
                            (last_connected_at integer)])]))

(defun nostr--parse-e-tags-by-marker (tags)
  "Return a plist mapping tag markers to a list of event IDs.
TAGS is a list of e-tags: (\"e\" <id> <relay?> <marker?>)."
  (let (result)
    (dolist (tag tags)
      (when (and (string= (car tag) "e")
                 (nth 1 tag))           ; id exists
        (let* ((id (nth 1 tag))
               (marker-str (or (nth 3 tag) "mention"))
               (marker (intern (concat ":" (downcase marker-str))))
               (existing (plist-get result marker)))
          (setq result (plist-put result marker (cons id existing))))))
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

(defun nostr--store-follows (pubkey tags)
  "Parse TAGS and store PUBKEYs follows in database."
  ;; delete old contacts for this pubkey and re-insert
  (emacsql nostr--db
           [:delete-from follows :where (= pubkey $s1)]
           pubkey)
  (dolist (tag tags)
    (when (and (equal (car tag) "p")
               (stringp (cadr tag)))
      (emacsql nostr--db
               [:insert-or-ignore :into follows :values [$s1 $s2]]
               pubkey (cadr tag)))))

(defun nostr--store-metadata (pubkey content)
  "Parse CONTENT from metadata event for PUBKEY."
  (let-alist (ignore-errors (json-parse-string content :object-type 'alist))
    (emacsql nostr--db
             [:insert-or-replace :into users
                                 :values [$s1 $s2 $s3 $s4]]
             pubkey .name .about .picture)))

(defun nostr--fetch-text-notes (since limit root-only)
  "Gets all text notes and from the database SINCE given timestamp and LIMIT.
Includes reply count.  If ROOT-ONLY is t only root notes are returned."
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
              events:root_id]
             :from events
             :left-join users :on (= events:pubkey users:pubkey)
             :where (and (= events:kind 1)
                         (>= events:created_at $s1)
                         ,(if root-only
                              '(is events:root_id nil)
                            'true))
             :order-by [(desc events:created_at)]
             :limit $s1]
           (or limit 50) (or since 0)))

(defun nostr--fetch-replies-from-db (note-id)
  "Fetching replies of a NOTE-ID."
  (emacsql nostr--db
           [:select [events:id
                     events:pubkey
                     users:name
                     users:picture
                     events:created_at
                     events:content
                     events:tags
                     events:reply_id
                     events:reply_count
                     events:root_id]
                    :from events
                    :inner-join users :on (= events:pubkey users:pubkey)
                    :where (= events:reply_id $s1)
                    :order-by [(asc events:created_at)]]
           note-id))

(defun nostr--fetch-contacts (pubkey)
  "Fetch contacts for PUBKEY."
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

(defun nostr--handle-event (relay _sub-id event)
  "Handles EVENT from SUB-ID on RELAY."
  (when (nostr--validate-event event)
    (let-alist event
      (pcase .kind
        ;; Kind 0 — Metadata (replaceable)
        (0
         (nostr--store-metadata .pubkey .content))

        ;; Kind 3 — Contact list (replaceable)
        (3
         (nostr--store-follows .pubkey .tags))

        ;; Kind 1 — Text note
        (1
         (nostr--store-event relay event))

        ;; Default: ignore it, but log
        (_
         (debug-message "skipping event %s" event))))
    (debug-message "Handled event kind %s from %s" (alist-get 'kind event) relay)))

(defun nostr--validate-event (_event)
  "Validates the signature on an EVENT.  Returns true if valid."
  ;; TODO: call out to a cli program to validate
  t)

(defun nostr--event-row-to-alist (row)
  "Convert event ROW to a alist or nil if it doesn't match."
  (pcase row
    (`(,id ,pubkey ,author ,pic ,created-at ,content ,tags
           ,reply-id ,replies ,root-id)
     `((id . ,id) (pubkey . ,pubkey) (author . ,author) (pic . ,pic)
       (created-at . ,created-at) (content . ,content) (tags . ,tags)
       (reply-id . ,reply-id) (replies . ,replies) (root-id . ,root-id)))))

(defun nostr--handle-eose (sub-id)
  "Handles EOSE for SUB-ID.  Auto-unsubscribe for one-off subs."
  (debug-message "EOSE for subscription %s" sub-id)
  (when (string-match-p "\\`\\(replies\\|note\\)-" sub-id)
    (maphash
     (lambda (_url ws)
       (when (websocket-openp ws)
         (websocket-send-text ws (json-encode `["CLOSE" ,sub-id]))))
     nostr--relay-connections)
    (remhash sub-id nostr--subscriptions)))

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
               (let ((pubkey (nostr--load-pubkey)))
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
  "Subscribe to each REQS over websocket WS connection to RELAY.
limited by LIMIT."
  (dolist (req reqs)
    (let* ((sub-id (elt req 1))
           (json (json-encode req)))
      (puthash sub-id t nostr--subscriptions)
      (debug-message "Sending subscription %s to relay %s" json relay)
      (websocket-send-text ws json))))

(defun nostr--subscribe-to-replies (note-id)
  "Subscribe to events that reference NOTE-ID.
Usually the root note that the user will click to open."
  (let ((req (nostr--req-replies-to-note note-id)))
    (maphash
     (lambda (url ws)
       (nostr--subscribe url ws (list req)))
     nostr--relay-connections)))

(defun nostr--unsubscribe (ws sub-id)
  "Sends CLOSE message to unsubscribe from subscription SUB-ID on WS."
  (debug-message "Unsubscribing from subscription %s" sub-id)
  (websocket-send-text ws (json-encode `["CLOSE" ,sub-id])))

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
  "Build a request for PUBKEY's metadata, contacts and personal feed."
  `["REQ" "personal"
    (("kinds" . (,nostr--kind-metadata ,nostr--kind-contacts ,nostr--kind-text-note))
     ("authors" . (,pubkey)))])

(defun nostr--req-contacts-notes (pubkey)
  "Build a request for PUBKEY to get its contacts' notes."
  `["REQ" "contacts-notes"
    (("kinds" . (,nostr--kind-text-note))
     ("authors" . ,(nostr--fetch-contacts pubkey))
     ("limit" . 50))])

(defun nostr--req-contacts-metadata (pubkey)
  "Build a request for PUBKEY to get its contacts' metadata."
  `["REQ" "contacts-metadata"
    (("kinds" . (,nostr--kind-metadata))
     ("authors" . ,(nostr--fetch-contacts pubkey)))])

(defun nostr--req-replies-to-note (event-id)
  "A request that references the note with EVENT-ID."
  `["REQ" ,(format "note-%s" event-id)
    (("kinds" . (,nostr--kind-text-note))
     ("#e" . (,event-id)))])

;; probably too crazy
(defun nostr--req-simple-global-feed (since &optional limit)
  "Build a request to fetch recent global notes.
Query after SINCE with an optional LIMIT."
  `["REQ" "global-feed"
    (("kinds" . (,nostr--kind-text-note))
     ("since" . ,since)
     ("limit" . ,(or limit 20)))])

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
  (setq tabulated-list-format `[("Author" 20 t . (:right-align nil))
                                ("Time" ,(length (nostr--format-timestamp 0)) t)
                                ("Replies" 7 nil . (:right-align t))
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

(defun nostr--one-week-ago ()
  "Unix timestamp from one week ago."
  (- (truncate (float-time)) (* 7 24 60 60)))

(defun nostr-refresh ()
  "Refresh the Nostr note list."
  (interactive)
  (let ((inhibit-read-only t)
        (events (nostr--fetch-text-notes nil 100 t)))
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
                     (nostr--format-content .content))))))
      events))
    (tabulated-list-print t)))

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
Optionally pass REPLY-CONTEXT to
reply to an event."
  (let ((buf (generate-new-buffer "*Nostr Compose*")))
    (with-current-buffer buf
      (erase-buffer)
      (nostr-note-mode)
      (when reply-context
        (setq nostr--reply-context reply-context)
        (insert (format ";; Replying to %s:\n\n;; %s\n\n"
                        (plist-get reply-context :author)
                        (truncate-string-to-width
                         (plist-get reply-context :content) 80 nil nil t))))
      (goto-char (point-max)))
    (switch-to-buffer buf)))

(defun nostr--build-note-tags (reply-to-note)
  "Build tags for a note using REPLY-TO-NOTE tags.
Empty if root note.
TODO: Fix and use respective columns on event"
  (when reply-to-note
    (let* ((r reply-to-note)
           (parent-tags (plist-get r :tags))
           (reply-to-id (plist-get r :id))
           (reply-to-pubkey (plist-get r :pubkey))
           (e-tags
            (seq-filter (lambda (tag) (equal (car tag) "e")) parent-tags))
           (root-id
            (nth 1 (seq-find (lambda (tag) (equal (last tag) '("root"))) e-tags))))
      `(("e" ,(or root-id reply-to-id) "" "root")
        ("e" ,reply-to-id "" "reply")
        ("p" ,reply-to-pubkey)))))

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
  (let-alist (nostr--get-selected-note)
    (when .context
      (nostr--compose .context))))

(defun nostr-create-note ()
  "Start composing a new note."
  (interactive)
  (nostr--compose))

(define-derived-mode nostr-thread-mode special-mode "Nostr-Thread"
  "Major mode for viewing a Nostr thread with multi-line entries."
  (setq buffer-read-only t)
  (setq-local truncate-lines nil))

(defun nostr--insert-threaded-note (note depth last-child)
  "Insert a threaded NOTE at DEPTH.  LAST-CHILD controls drawing └ or ├."
  (let-alist note
    (let* ((timestamp (nostr--format-timestamp .created-at))
           (prefix (concat
                    (make-string (max 0 (1- depth)) ?\s)
                    (when (> depth 0)
                      (concat (if last-child "└─ " "├─ ")))))
           (face (if (zerop depth) 'bold 'default)))
      (insert (propertize (format "%s%s [%s]\n" prefix .author timestamp) 'face face))
      (insert (propertize (format "%s%s\n\n"
                                  (make-string (+ (length prefix)) ?\s)
                                  .content)
                          'face 'default)))))

(defun nostr--build-thread-tree (parent-id &optional depth)
  "Recursively build a thread tree starting from PARENT-ID at DEPTH."
  (let ((replies (nostr--fetch-replies-from-db parent-id))
        (depth (or depth 1)))
    (cl-loop
     for i from 0 below (length replies)
     for row = (nth i replies)
     for last = (= i (1- (length replies)))
     do (let ((event (nostr--event-row-to-alist row)))
          (nostr--insert-threaded-note event depth last)
          (nostr--build-thread-tree (alist-get 'id event) (1+ depth))))))

(defun nostr--render-thread-buffer (note-id)
  "Render a thread rooted at NOTE-ID using tree-style indentation.
TODO: refactor this so we can hit `g' on a thread and reload."
  (let* ((buf (get-buffer-create (format "*Nostr Thread: %s*" note-id)))
         (root (nostr--get-selected-note)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (nostr-thread-mode)
        (setq-local nostr--thread-root note-id)
        (nostr--insert-threaded-note root 0 nil)
        (nostr--build-thread-tree (alist-get 'id root))))
    (goto-char (point-min))
    (display-buffer buf)))

(defun nostr-open-thread ()
  "Open multi-line thread view for the selected note."
  (interactive)
  (let-alist (nostr--get-selected-note)
    (when .id
      (nostr--subscribe-to-replies .id)
      (nostr--render-thread-buffer .id))))

;;;###autoload
(defun nostr-open ()
  "Open the Nostr client buffer and display notes."
  (interactive)
  (unless nostr--db
    (nostr--open-db nostr-db-path))
  (nostr--connect-to-all-relays)
  (let ((buf (get-buffer-create nostr-buffer-name)))
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook #'nostr--cleanup nil t)
      (nostr-mode)
      (nostr-refresh))
    (switch-to-buffer buf)))

(defun nostr--cleanup ()
  "Cleanup when buffer is killed."
  (nostr--disconnect-all-relays)
  (emacsql-close nostr--db))

(provide 'nostr)
;;; nostr.el ends here
