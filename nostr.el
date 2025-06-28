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

(defcustom nostr-timestamp-format "%Y-%m-%d %H:%M"
  "Formatting for a posts timestamp."
  :type 'string
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
  "Decrypt and return your Nostr private key."
  (let ((ctx (epg-make-context 'OpenPGP)))
    (string-trim (epg-decrypt-file ctx (expand-file-name nostr-private-key-path) nil))))

(defun nostr--load-pubkey ()
  "Get public key from nostril."
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
  (debug-message "storing event in database")
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
            [events:id events:pubkey users:name users:picture events:created_at events:content]
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
  (let ((data (json-parse-string frame :object-type 'alist :array-type 'list :false-object nil)))
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

(defun nostr--handle-event (relay _sub-id event)
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
    (nostr-refresh) ; refresh for UI updates
    (debug-message "Handled event kind %s from %s" (alist-get 'kind event) relay)))

(defun nostr--validate-event (_event)
  "Validates the signature on an EVENT.  Returns true if valid."
  ;; TODO: call out to a cli program to validate
  t)

(defun nostr--handle-eose (sub-id)
  "Handles an EOSE message for SUB-ID in `nostr--subscriptions'."
  (debug-message "EOSE for subscription %s" sub-id)
  (nostr-refresh))

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
              (nostr--subscribe url ws '(nostr--req-personal nostr--req-contacts-posts)))
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

(defun nostr--generate-sub-id ()
  "Generate a random subscription id to sent to the relay."
  ;; TODO: check how others do this
  (substring (md5 (format "%s" (current-time))) 0 10))

(defun nostr--subscribe (relay ws reqs)
  "Send a subscription REQ for KINDS over websocket WS connection to RELAY.
limited by LIMIT."
  (dolist (req-fn reqs)
    (let* ((req (funcall req-fn (nostr--load-pubkey)))
           (sub-id (elt req 1))
           (json (json-encode req)))
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
  "Build a request to fetch recent global notes.
Query after SINCE with an optional LIMIT."
  `["REQ" ,(nostr--generate-sub-id)
    (("kinds" . (1))
     ("since" . ,since)
     ("limit" . ,(or limit 20)))])

;;;; UI

(defvar nostr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-refresh)
    (define-key map (kbd "?") #'nostr-ui-popup-actions)
    (define-key map (kbd "r") #'nostr-reply-to-post)
    (define-key map (kbd "c") #'nostr-create-post)
    map)
  "Keymap for `nostr-mode'.")

(define-derived-mode nostr-mode tabulated-list-mode "Nostr"
  "Major mode for viewing Nostr posts in a list."
  (setq tabulated-list-format [("Author" 20 t)
                               ("Time" 20 t)
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

(defun nostr-refresh ()
  "Refresh the Nostr post list."
  (interactive)
  (let ((inhibit-read-only t)
        (posts (nostr--fetch-user-posts nil)))
    (setq-local
     tabulated-list-entries
     (seq-map
      (lambda (post)
        (pcase post
          (`(,id ,pubkey ,name ,_pic ,created-at ,content)
           (list `(:id ,id :pubkey ,pubkey :author ,name :content ,content)
                 (vector name
                         (nostr--format-timestamp created-at)
                         (nostr--format-content content))))))
      posts))
    (tabulated-list-print t)))

(transient-define-prefix nostr-ui-popup-actions ()
  "Actions for a selected Nostr post."
  [["Nostr Actions"
    ("r" "Reply to post" nostr-reply-to-post)
    ;; other actions can go here
    ]])

(defun nostr--get-selected-post ()
  "Return data for the currently selected post metadata as a plist."
  (tabulated-list-get-id))

(defvar nostr-post-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'nostr-send-current-post)
    map)
  "Keymap for `nostr-post-mode'.")

(define-derived-mode nostr-post-mode text-mode "Nostr-Post"
  "Major mode for composing a Nostr post or reply.")

(defvar-local nostr--reply-context nil
  "If non-nil, contains the event being replied to.")

(defun nostr--compose (&optional reply-context)
  "Open a new buffer to compose a post.
Optionally pass REPLY-CONTEXT to
reply to an event."
  (let ((buf (generate-new-buffer "*Nostr Compose*")))
    (with-current-buffer buf
      (erase-buffer)
      (nostr-post-mode)
      (setq nostr--reply-context reply-context)
      (when reply-context
        (insert (format "Replying to %s:\n\n%s\n--\n"
                        (plist-get reply-context :author)
                        (truncate-string-to-width
                         (plist-get reply-context :content) 80 nil nil t))))
      (goto-char (point-max)))
    (switch-to-buffer buf)))

(defun nostr--flatten-tags-for-nostril (tags)
  "Convert nested TAGS list into a flat list of --tagn args for nostril."
  (apply #'append
         (mapcar (lambda (tag)
                   (let ((n (length tag)))
                     (append (list "--tagn" (number-to-string n)) tag)))
                 tags)))

(defun nostr-send-current-post ()
  "Collect, sign, and send post in `nostr-post-mode` buffer."
  (interactive)
  (let ((content (string-trim
                  (cadr (split-string
                         (buffer-substring-no-properties (point-min) (point-max))
                         "--"))))
        (reply-to nostr--reply-context))
    (unless (string-empty-p content)
      (let* ((privkey (nostr--load-private-key))
             (tags (when reply-to
                     (list
                      (list "e" (plist-get reply-to :id) "" "reply")
                      (list "p" (plist-get reply-to :pubkey)))))
             (tag-args (nostr--flatten-tags-for-nostril tags))
             (args (append (list "nostril"
                                 "--envelope"
                                 "--kind" "1"
                                 "--content" content
                                 "--sec" privkey)
                           tag-args))
             (result (with-temp-buffer
                       (let ((exit-code (apply #'call-process (car args) nil t nil (cdr args)))
                             (output (buffer-string)))
                         (cons exit-code output)))))
        ;; try to send
        (if (car result)
            (progn
              (debug-message "got result from nostril: %s" (cdr result))
              (maphash
               (lambda (url ws)
                 (when (websocket-openp ws)
                   (websocket-send-text ws (cdr result))
                   (debug-message "post sent to relay %s" url)))
               nostr--relay-connections)
              (message "Post sent!"))
          (message "Error creating event with nostril: %s" (cdr result)))
        (kill-buffer)))))

(defun nostr-reply-to-post ()
  "Reply to the post at point."
  (interactive)
  (let* ((context (nostr--get-selected-post)))
    (when context
      (nostr--compose context))))

(defun nostr-create-post ()
  "Start composing a new post."
  (interactive)
  (nostr--compose))

(defun nostr-open ()
  "Open the Nostr client buffer and display posts."
  (interactive)
  (unless nostr--db
    (nostr--open-db nostr-db-path))
  (nostr--connect-to-all-relays)
  (let ((buf (get-buffer-create nostr-buffer-name)))
    (with-current-buffer buf
      (nostr-mode)
      (nostr-refresh))
    (switch-to-buffer buf)))

(provide 'nostr)
;;; nostr.el ends here
