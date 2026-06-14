;;; nostr-test.el --- Tests for nostr.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; ERT tests for the modular Nostr client.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'nostr)

(defmacro nostr-test-with-db (&rest body)
  "Run BODY with an isolated in-memory Nostr database."
  (declare (indent 0))
  `(let ((nostr-db--connection (emacsql-sqlite-open nil)))
     (unwind-protect
         (progn
           (nostr-db-init)
           ,@body)
       (emacsql-close nostr-db--connection))))

(defconst nostr-test-root-event
  '((id . "root")
    (pubkey . "alice")
    (created_at . 100)
    (kind . 1)
    (tags . nil)
    (content . "hello http://example.test/a.png")
    (sig . "sig")))

(defconst nostr-test-reply-event
  '((id . "reply")
    (pubkey . "bob")
    (created_at . 101)
    (kind . 1)
    (tags . (("e" "root" "wss://relay.example" "root")
             ("p" "alice")))
    (content . "reply")
    (sig . "sig")))

(defun nostr-test-backend-command ()
  "Return a usable backend command for integration tests, or nil."
  (or (executable-find nostr-backend-command)
      (let ((release (expand-file-name "target/release/nostr-el-backend"
                                       default-directory))
            (debug (expand-file-name "target/debug/nostr-el-backend"
                                     default-directory)))
        (cond
         ((file-executable-p release) release)
         ((file-executable-p debug) debug)))))

(defun nostr-test-wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds pass."
  (let ((deadline (+ (float-time) (or timeout 5)))
        result)
    (while (and (not result) (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (setq result (funcall predicate)))
    result))

(defun nostr-test-store-text-note (id pubkey created-at content &optional root-id reply-id)
  "Store a normalized text note for tests."
  (nostr-db-store-event
   `((id . ,id)
     (pubkey . ,pubkey)
     (created_at . ,created-at)
     (kind . 1)
     (tags . nil)
     (content . ,content)
     (sig . "sig")
     (relay . "relay")
     (root-id . ,root-id)
     (reply-id . ,reply-id)
     (quote-id . nil))))

(ert-deftest nostr-event-normalize-adds-derived-fields ()
  (let ((event (nostr-event-normalize nostr-test-reply-event "wss://relay.example")))
    (should (equal (alist-get 'relay event) "wss://relay.example"))
    (should (equal (alist-get 'root-id event) "root"))
    (should-not (alist-get 'reply-id event))))

(ert-deftest nostr-event-build-reply-tags-root-and-reply ()
  (should (equal (nostr-event-build-reply-tags
                  '((id . "root") (pubkey . "alice"))
                  "wss://relay.example")
                 '(("e" "root" "wss://relay.example" "root")
                   ("p" "alice"))))
  (should (equal (nostr-event-build-reply-tags
                  '((id . "reply") (pubkey . "bob") (root-id . "root"))
                  "wss://relay.example")
                 '(("e" "root" "wss://relay.example" "root")
                   ("e" "reply" "wss://relay.example" "reply")
                   ("p" "bob")))))

(ert-deftest nostr-event-media-urls-finds-images ()
  (should (equal (nostr-event-media-urls
                  "look https://example.test/a.jpg and https://example.test/b.webp?x=1")
                 '("https://example.test/a.jpg"
                   "https://example.test/b.webp?x=1"))))

(ert-deftest nostr-event-media-items-include-mp4 ()
  "Media extraction classifies mp4 URLs as external video media."
  (should (equal (nostr-event-media-items
                  "watch https://example.test/a.mp4 and https://example.test/b.webp")
                 '(((url . "https://example.test/a.mp4") (type . video))
                   ((url . "https://example.test/b.webp") (type . image))))))

(ert-deftest nostr-event-nevents-finds-nostr-uri-values ()
  "Embedded nevent extraction accepts bare and nostr: URI values."
  (should (equal (nostr-event-nevents
                  "see nostr:nevent1qqqqqqqqqqqqqq and nevent1qqqqqqqqqqqqqq")
                 '("nostr:nevent1qqqqqqqqqqqqqq"
                   "nevent1qqqqqqqqqqqqqq"))))

(ert-deftest nostr-event-parses-zap-receipt-target-and-amount ()
  "Zap receipts get their target from e-tags and amount from description JSON."
  (let* ((zap-request "{\"kind\":9734,\"tags\":[[\"e\",\"note-zapped\"],[\"p\",\"alice\"],[\"amount\",\"21000\"]]}")
         (receipt `((id . "zap1")
                    (kind . ,nostr-kind-zap-receipt)
                    (tags . (("p" "alice")
                             ("e" "note-zapped")
                             ("bolt11" "lnbc...")
                             ("description" ,zap-request))))))
    (should (equal (nostr-event-zap-target-event-id receipt) "note-zapped"))
    (should (= (nostr-event-zap-amount-msats receipt) 21000))))

(ert-deftest nostr-event-reaction-target-uses-last-e-tag ()
  "NIP-25 reactions target the last e-tag when extra e-tags are present."
  (let ((reaction '((id . "reaction1")
                    (kind . 7)
                    (tags . (("e" "thread-root" "wss://relay.example" "root")
                             ("e" "actual-target" "wss://relay.example"))))))
    (should (equal (nostr-event-reaction-event-id reaction) "actual-target"))))

(ert-deftest nostr-db-stores-and-selects-feed ()
  (nostr-test-with-db
    (nostr-db-store-event
     '((id . "meta")
       (pubkey . "alice")
       (created_at . 90)
       (kind . 0)
       (content . "{\"name\":\"Alice\",\"display_name\":\"Alice A\"}")))
    (nostr-db-store-event
     '((id . "contacts")
       (pubkey . "me")
       (created_at . 91)
       (kind . 3)
       (tags . (("p" "alice")))))
    (nostr-db-store-event (nostr-event-normalize nostr-test-root-event "relay"))
    (nostr-db-store-event (nostr-event-normalize nostr-test-reply-event "relay"))
    (let ((feed (nostr-db-select-feed "me" 10 t)))
      (should (= (length feed) 1))
      (should (equal (alist-get 'id (car feed)) "root"))
      (should (equal (alist-get 'author (car feed)) "Alice A"))
      (should (= (alist-get 'replies (nostr-db-event-counts "root")) 1)))))

(ert-deftest nostr-db-tracks-inbound-relay-presence ()
  "Storing the same event from multiple relays keeps one event and counts relays."
  (nostr-test-with-db
    (dolist (relay '("wss://relay-a.example" "wss://relay-b.example"))
      (nostr-db-store-event
       `((id . "multi-relay")
         (pubkey . "alice")
         (created_at . 100)
         (kind . 1)
         (tags . nil)
         (content . "hello")
         (sig . "sig")
         (relay . ,relay)
         (root-id . nil)
         (reply-id . nil)
         (quote-id . nil))))
    (should (= 1 (length (emacsql nostr-db--connection
                                  [:select [id] :from events
                                           :where (= id "multi-relay")]))))
    (should (= 2 (alist-get 'relay-count
                            (nostr-db-event-counts "multi-relay"))))))

(ert-deftest nostr-feed-counts-load-lazily-when-rendered ()
  "Feed queries avoid eager count fields; note rendering loads visible counts."
  (nostr-test-with-db
    (nostr-test-store-text-note "root-counts" "me" 100 "root note")
    (nostr-test-store-text-note "reply-counts" "alice" 101 "reply" "root-counts" "root-counts")
    (nostr-db-store-event
     '((id . "reaction-counts")
       (pubkey . "bob")
       (created_at . 102)
       (kind . 7)
       (tags . (("e" "root-counts")))
       (content . "+")
       (sig . "sig")))
    (let ((event (car (nostr-db-select-home-feed "me" 10))))
      (should-not (assoc 'replies event))
      (should-not (assoc 'reactions event))
      (with-temp-buffer
        (nostr-ui-insert-note event)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "↩ 1" text))
          (should (string-match-p "♥ 1" text)))))))

(ert-deftest nostr-ui-note-counts-use-newer-db-values ()
  "Stale inline zero counts do not hide accumulated interaction counts."
  (nostr-test-with-db
    (nostr-test-store-text-note "stale-counts" "me" 100 "root note")
    (nostr-db-store-event
     '((id . "reaction-stale-1")
       (pubkey . "alice")
       (created_at . 101)
       (kind . 7)
       (tags . (("e" "stale-counts")))
       (content . "+")
       (sig . "sig")))
    (nostr-db-store-event
     '((id . "reaction-stale-2")
       (pubkey . "bob")
       (created_at . 102)
       (kind . 7)
       (tags . (("e" "stale-counts")))
       (content . "❤️")
       (sig . "sig")))
    (with-temp-buffer
      (let ((event '((id . "stale-counts")
                     (pubkey . "me")
                     (created-at . 100)
                     (kind . 1)
                     (content . "root note")
                     (reactions . 0)
                     (reposts . 0)
                     (replies . 0))))
        (nostr-ui-insert-note event)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "♥ 2" text)))))))

(ert-deftest nostr-ui-note-counts-use-nip25-reaction-target ()
  "Reaction counts accumulate on the final e-tag target used by NIP-25."
  (nostr-test-with-db
    (nostr-test-store-text-note "thread-root" "alice" 100 "root")
    (nostr-test-store-text-note "actual-target" "alice" 101 "reply" "thread-root" "thread-root")
    (nostr-db-store-event
     '((id . "reaction-nip25")
       (pubkey . "bob")
       (created_at . 102)
       (kind . 7)
       (tags . (("e" "thread-root" "wss://relay.example" "root")
                ("e" "actual-target" "wss://relay.example")))
       (content . "+")
       (sig . "sig")))
    (with-temp-buffer
      (nostr-ui-insert-note '((id . "actual-target")
                              (pubkey . "alice")
                              (created-at . 101)
                              (kind . 1)
                              (content . "reply")))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "♥ 1" text))))))

(ert-deftest nostr-ui-refresh-note-counts-updates-visible-footer ()
  "Targeted count refresh updates the visible card footer without moving point."
  (nostr-test-with-db
    (nostr-test-store-text-note "reacted-note" "alice" 100 "hello")
    (nostr-test-store-text-note "selected-note" "bob" 101 "stay here")
    (with-temp-buffer
      (nostr-ui-insert-note '((id . "reacted-note")
                              (pubkey . "alice")
                              (created-at . 100)
                              (kind . 1)
                              (content . "hello")))
      (nostr-ui-insert-note '((id . "selected-note")
                              (pubkey . "bob")
                              (created-at . 101)
                              (kind . 1)
                              (content . "stay here")))
      (should (string-match-p "♥ 0"
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
      (goto-char (point-min))
      (search-forward "stay here")
      (nostr-db-store-event
       '((id . "reaction-targeted")
         (pubkey . "me")
         (created_at . 101)
         (kind . 7)
         (tags . (("e" "reacted-note")))
         (content . "+")
         (sig . "sig")))
      (let ((selected-state (nostr-ui-capture-position)))
        (should (equal (nostr-ui-section-id (nostr-ui-section-at-point))
                       "selected-note"))
        (nostr-ui-refresh-note-counts "reacted-note")
        (let ((after-state (nostr-ui-capture-position)))
          (should (equal (alist-get 'point after-state)
                         (alist-get 'point selected-state)))))
      (nostr-ui-refresh-note-counts "reacted-note")
      (should (string-match-p "♥ 1"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest nostr-relay-websocket-error-records-relay-status ()
  "Websocket upgrade errors are captured as relay status instead of warnings."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        captured-error-callback
        statuses)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (url &rest args)
                 (should (equal url "wss://relay.example"))
                 (setq captured-error-callback (plist-get args :on-error))
                 'nostr-test-websocket))
              ((symbol-function 'run-at-time)
               (lambda (&rest _args) nil))
              ((symbol-function 'nostr-db-store-relay-status)
               (lambda (url state &optional message)
                 (push (list url state message) statuses)))
              ((symbol-function 'nostr-relay--update-mode-line)
               (lambda () nil)))
      (should (eq (nostr-relay--open-websocket "wss://relay.example" "me")
                  'nostr-test-websocket))
      (should (gethash "wss://relay.example" nostr-relay--connections))
      (should (functionp captured-error-callback))
      (funcall captured-error-callback
               'nostr-test-websocket
               'on-open
               '(websocket-received-error-http-response 503))
      (should-not (gethash "wss://relay.example" nostr-relay--connections))
      (should (equal (cadar statuses) "error"))
      (should (string-match-p "503" (caddar statuses))))))

(ert-deftest nostr-timeline-primary-feeds-render-accumulated-reactions ()
  "Feed, Conversations, and My Posts cards show DB-backed reaction counts."
  (nostr-test-with-db
    (emacsql nostr-db--connection
             [:insert-or-ignore :into follows :values [$s1 $s2 $s3 $s4]]
             "me" "alice" nil nil)
    (nostr-test-store-text-note "feed-note" "alice" 100 "feed note")
    (nostr-test-store-text-note "conversation-note" "alice" 101 "reply" "root" "root")
    (nostr-test-store-text-note "my-note" "me" 102 "my post")
    (dolist (target '("feed-note" "conversation-note" "my-note"))
      (dotimes (i 2)
        (nostr-db-store-event
         `((id . ,(format "reaction-%s-%d" target i))
           (pubkey . ,(format "reactor-%d" i))
           (created_at . ,(+ 200 i))
           (kind . 7)
           (tags . (("e" ,target)))
           (content . "+")
           (sig . "sig")))))
    (cl-letf (((symbol-function 'nostr-timeline--backfill-missing-reposts)
               #'ignore)
              ((symbol-function 'nostr-timeline--backfill-visible-metadata)
               #'ignore)
              ((symbol-function 'nostr-timeline--sync-active-feed)
               #'ignore))
      (dolist (case '((feed . "feed note")
                      (conversations . "reply")
                      (my-posts . "my post")))
        (with-temp-buffer
          (nostr-timeline-mode)
          (setq-local nostr-timeline-current-pubkey "me")
          (setq-local nostr-timeline-feed-kind (car case))
          (nostr-timeline-refresh)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p (cdr case) text))
            (should (string-match-p "♥ 2" text))))))))

(ert-deftest nostr-ui-formats-relative-and-exact-note-times ()
  "Feed cards use relative dates while detail cards use exact dates."
  (should (equal (nostr-ui-format-relative-time 1000 4540) "59 minutes ago"))
  (should (equal (nostr-ui-format-relative-time 1000 (+ 1000 (* 3 86400)))
                 "3 days ago"))
  (should (equal (nostr-ui-format-relative-time 1120 1000) "in 2 minutes"))
  (should (equal (nostr-ui-format-exact-time 1736766000)
                 "13 January 2025 at 14:00")))

(ert-deftest nostr-ui-note-card-renders-author-stats-and-detail-time ()
  "Soft cards include cached identity, compact stats, zaps, and exact detail time."
  (nostr-test-with-db
    (nostr-db-store-profile-event
     '((id . "profile-alice")
       (pubkey . "alice-pubkey")
       (created_at . 100)
       (kind . 0)
       (content . "{\"display_name\":\"Alice\",\"nip05\":\"alice@example.test\"}")))
    (with-temp-buffer
      (nostr-ui-insert-note
       '((id . "note-card")
         (pubkey . "alice-pubkey")
         (created-at . 1736766000)
         (content . "hello card")
         (replies . 2)
         (reactions . 3)
         (reposts . 1)
         (zaps . 2)
         (zap-msats . 21000))
       '(:style detail))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Alice · alice@example.test" text))
        (should (string-match-p "13 January 2025 at 14:00" text))
        (should (string-match-p "hello card" text))
        (should (string-match-p "↩ 2" text))
        (should (string-match-p "♥ 3" text))
        (should (string-match-p "↻ 1" text))
        (should (string-match-p "⚡ 2 (21 sats)" text))))))

(ert-deftest nostr-db-stores-zap-receipts-and-counts-msats ()
  "Zap receipts are counted even when amount parsing fails."
  (nostr-test-with-db
    (nostr-test-store-text-note "zap-target" "alice" 100 "zap me")
    (let ((zap-request "{\"kind\":9734,\"tags\":[[\"e\",\"zap-target\"],[\"amount\",\"21000\"]]}"))
      (nostr-db-store-event
       `((id . "zap-good")
         (pubkey . "ln-service")
         (created_at . 101)
         (kind . ,nostr-kind-zap-receipt)
         (tags . (("e" "zap-target")
                  ("p" "alice")
                  ("description" ,zap-request)))
         (content . "")
         (sig . "sig")))
      (nostr-db-store-event
       `((id . "zap-no-amount")
         (pubkey . "ln-service")
         (created_at . 102)
         (kind . ,nostr-kind-zap-receipt)
         (tags . (("e" "zap-target")
                  ("description" "{not json")))
         (content . "")
         (sig . "sig")))
      (let ((counts (nostr-db-event-counts "zap-target")))
        (should (= (alist-get 'zaps counts) 2))
        (should (= (alist-get 'zap-msats counts) 21000))))))

(ert-deftest nostr-db-profile-metadata-accepts-common-name-keys ()
  "Profile metadata accepts both NIP-style and common client key names."
  (nostr-test-with-db
    (nostr-db-store-profile-event
     '((pubkey . "alice")
       (created_at . 100)
       (kind . 0)
       (content . "{\"username\":\"alice\",\"displayName\":\"Alice Display\"}")))
    (let ((profile (nostr-db-select-profile "alice")))
      (should (equal (nth 1 profile) "alice"))
      (should (equal (nth 2 profile) "Alice Display")))))

(ert-deftest nostr-db-profile-metadata-ignores-json-null-fields ()
  "Profile metadata JSON null values are stored as missing values."
  (nostr-test-with-db
    (nostr-db-store-profile-event
     '((pubkey . "alice")
       (created_at . 100)
       (kind . 0)
       (content . "{\"name\":\"alice\",\"picture\":null,\"nip05\":null}")))
    (let ((profile (nostr-db-select-profile "alice")))
      (should (equal (nth 1 profile) "alice"))
      (should-not (nth 4 profile))
      (should-not (nth 5 profile)))))

(ert-deftest nostr-db-profile-metadata-stores-raw-content ()
  "Profile metadata keeps raw JSON so unsupported fields can be preserved."
  (nostr-test-with-db
    (nostr-db-store-profile-event
     '((pubkey . "alice")
       (created_at . 100)
       (kind . 0)
       (content . "{\"name\":\"alice\",\"website\":\"https://example.test\"}")))
    (let ((profile (nostr-db-select-profile "alice")))
      (should (equal (nth 7 profile)
                     "{\"name\":\"alice\",\"website\":\"https://example.test\"}")))))

(ert-deftest nostr-db-counts-unread-notifications ()
  "Unread notification counts use cached seen state."
  (nostr-test-with-db
    (nostr-db-store-notification "notif1" "mention" "note1" "alice" "me" 100)
    (nostr-db-store-notification "notif2" "reply" "note2" "bob" "me" 101)
    (should (= 2 (nostr-db-unread-notification-count)))
    (nostr-db-mark-notification-seen "notif1")
    (should (= 1 (nostr-db-unread-notification-count)))
    (nostr-db-mark-all-notifications-seen)
    (should (= 0 (nostr-db-unread-notification-count)))))

(ert-deftest nostr-db-feed-includes-own-root-notes ()
  (nostr-test-with-db
    (nostr-db-store-event
     '((id . "own")
       (pubkey . "me")
       (created_at . 100)
       (kind . 1)
       (tags . nil)
       (content . "my note")
       (sig . "sig")
       (relay . "relay")
       (root-id . nil)
       (reply-id . nil)
       (quote-id . nil)))
    (let ((feed (nostr-db-select-feed "me" 10 t)))
      (should (= (length feed) 1))
      (should (equal (alist-get 'id (car feed)) "own")))))

(ert-deftest nostr-db-feed-includes-reposts-by-followed-accounts ()
  "Followed-account reposts insert the reposted root note into the feed."
  (nostr-test-with-db
    (nostr-db-store-event
     '((id . "contacts")
       (pubkey . "me")
       (created_at . 90)
       (kind . 3)
       (tags . (("p" "alice")))))
    (nostr-db-store-event
     '((id . "alice-profile")
       (pubkey . "alice")
       (created_at . 91)
       (kind . 0)
       (content . "{\"display_name\":\"Alice\"}")))
    (nostr-test-store-text-note "reposted-root" "carol" 100 "carol note")
    (nostr-db-store-event
     '((id . "alice-repost")
       (pubkey . "alice")
       (created_at . 120)
       (kind . 6)
       (tags . (("e" "reposted-root")))
       (content . "")
       (sig . "sig")))
    (let ((feed (nostr-db-select-account-feed "me" 10)))
      (should (= (length feed) 1))
      (should (equal (alist-get 'id (car feed)) "reposted-root"))
      (should (equal (alist-get 'reposted-by (car feed)) "alice"))
      (should (= (alist-get 'reposted-at (car feed)) 120))
      (should (equal (alist-get 'reposted-by-name (car feed)) "Alice")))))

(ert-deftest nostr-db-selects-missing-repost-targets ()
  "Missing followed-account repost targets are available for relay backfill."
  (nostr-test-with-db
    (nostr-db-store-event
     '((id . "contacts")
       (pubkey . "me")
       (created_at . 90)
       (kind . 3)
       (tags . (("p" "alice")))))
    (nostr-test-store-text-note "cached-target" "carol" 100 "cached")
    (nostr-db-store-event
     '((id . "cached-repost")
       (pubkey . "alice")
       (created_at . 120)
       (kind . 6)
       (tags . (("e" "cached-target")))
       (content . "")
       (sig . "sig")))
    (nostr-db-store-event
     '((id . "missing-repost")
       (pubkey . "alice")
       (created_at . 121)
       (kind . 6)
       (tags . (("e" "missing-target")))
       (content . "")
       (sig . "sig")))
    (should (equal (nostr-db-select-missing-repost-targets "me" 10)
                   '("missing-target")))))

(ert-deftest nostr-db-init-migrates-existing-cache-schema ()
  "Opening an older cache adds columns used by current queries."
  (let ((nostr-db--connection (emacsql-sqlite-open nil)))
    (unwind-protect
        (progn
          (emacsql nostr-db--connection
                   [:create-table events
                                  ([(id :primary-key)
                                    pubkey
                                    (created_at integer)
                                    (kind integer)
                                    tags
                                    content
                                    sig
                                    relay
                                    root_id
                                    reply_id])])
          (emacsql nostr-db--connection
                   [:create-table profiles
                                  ([(pubkey :primary-key)
                                    name
                                    display_name
                                    about
                                    picture
                                    nip05
                                    (updated_at integer)])])
          (emacsql nostr-db--connection
                   [:create-table follows
                                  ([pubkey contact]
                                   (:unique [pubkey contact]))])
          (nostr-db-init)
          (should (member "quote_id" (nostr-db--table-columns "events")))
          (should (member "lud16" (nostr-db--table-columns "profiles")))
          (should (member "relay" (nostr-db--table-columns "follows")))
          (should (member "petname" (nostr-db--table-columns "follows")))
          (should (member "amount_msats" (nostr-db--table-columns "zaps")))
          (nostr-db-store-profile-event
           '((pubkey . "alice")
             (created_at . 120)
             (kind . 0)
             (content . "{\"name\":\"Alice\",\"lud16\":\"alice@example.test\"}")))
          (nostr-db-store-event
           '((id . "quoted")
             (pubkey . "alice")
             (created_at . 121)
             (kind . 1)
             (tags . nil)
             (content . "quoted note")
             (sig . "sig")
             (relay . "relay")
             (root-id . nil)
             (reply-id . nil)
             (quote-id . "quote-target")))
          (nostr-db-store-event
           '((id . "contacts")
             (pubkey . "alice")
             (created_at . 122)
             (kind . 3)
             (tags . (("p" "bob" "wss://relay.example" "Bob")))))
          (let ((feed (nostr-db-select-global-feed 10))
                (profile (nostr-db-select-profile "alice"))
                (follows (nostr-db-select-follows "alice")))
            (should (equal (alist-get 'id (car feed)) "quoted"))
            (should (equal (alist-get 'quote-id (car feed)) "quote-target"))
            (should (equal (nth 6 profile) "alice@example.test"))
            (should (equal follows '("bob")))))
      (emacsql-close nostr-db--connection))))

(ert-deftest nostr-db-init-repairs-legacy-reaction-column-order ()
  "Opening an older cache restores reaction target ids before counting."
  (let ((nostr-db--connection (emacsql-sqlite-open nil)))
    (unwind-protect
        (progn
          (emacsql nostr-db--connection
                   [:create-table reactions
                                  ([(id :primary-key)
                                    pubkey
                                    (created_at integer)
                                    event_id
                                    content])])
          ;; Row shape produced by current positional inserts against the old
          ;; column order: target id in pubkey, reactor pubkey in created_at.
          (emacsql nostr-db--connection
                   [:insert :into reactions :values [$s1 $s2 $s3 $s4 $s5]]
                   "reaction-scrambled" "note1" "reactor1" "+" 101)
          ;; Row shape from the original old schema: already semantically
          ;; correct for the old column names.
          (emacsql nostr-db--connection
                   [:insert :into reactions :values [$s1 $s2 $s3 $s4 $s5]]
                   "reaction-old-correct" "reactor2" 102 "note2" "🤙")
          (nostr-db-init)
          (should (equal (nostr-db--table-columns "reactions")
                         '("id" "event_id" "pubkey" "content" "created_at")))
          (should (= (alist-get 'reactions (nostr-db-event-counts "note1")) 1))
          (should (= (alist-get 'reactions (nostr-db-event-counts "note2")) 1))
          (should (equal (emacsql nostr-db--connection
                                  [:select [id event_id pubkey content created_at]
                                           :from reactions
                                           :order-by id])
                         '(("reaction-old-correct" "note2" "reactor2" "🤙" 102)
                           ("reaction-scrambled" "note1" "reactor1" "+" 101)))))
      (emacsql-close nostr-db--connection))))

(ert-deftest nostr-db-select-reactions-for-event-joins-profiles ()
  "Reaction detail rows include cached reactor profile metadata."
  (nostr-test-with-db
    (nostr-db-store-event
     '((id . "target")
       (pubkey . "alice")
       (created_at . 100)
       (kind . 1)
       (tags . nil)
       (content . "note")
       (sig . "sig")))
    (nostr-db-store-event
     '((id . "profile")
       (pubkey . "bob")
       (created_at . 101)
       (kind . 0)
       (tags . nil)
       (content . "{\"name\":\"bob\",\"display_name\":\"Bob\"}")
       (sig . "sig")))
    (nostr-db-store-event
     '((id . "reaction")
       (pubkey . "bob")
       (created_at . 102)
       (kind . 7)
       (tags . (("e" "target")))
       (content . "🤙")
       (sig . "sig")))
    (should (equal (car (nostr-db-select-reactions-for-event "target"))
                   '("reaction" "target" "bob" "🤙" 102
                     "bob" "Bob" nil nil nil nil 101)))))

(ert-deftest nostr-db-select-thread-returns-root-and-replies ()
  (nostr-test-with-db
    (nostr-db-store-event (nostr-event-normalize nostr-test-root-event "relay"))
    (nostr-db-store-event (nostr-event-normalize nostr-test-reply-event "relay"))
    (let ((thread (nostr-db-select-thread "root")))
      (should (= (length thread) 2))
      (should (equal (mapcar (lambda (event) (alist-get 'id event)) thread)
                     '("root" "reply"))))))

(ert-deftest nostr-thread-open-reply-loads-root-thread-and-focuses-reply ()
  "Opening a reply shows the full cached root thread and selects the reply."
  (nostr-test-with-db
    (nostr-test-store-text-note "root" "alice" 100 "root note")
    (nostr-test-store-text-note "reply-1" "bob" 101 "first reply" "root" "root")
    (nostr-test-store-text-note "reply-2" "carol" 102 "second reply" "root" "root")
    (let ((reply (car (seq-filter
                       (lambda (event) (equal (alist-get 'id event) "reply-2"))
                       (nostr-db-select-thread "root")))))
      (cl-letf (((symbol-function 'nostr-relay-fetch-event-metadata)
                 (lambda (&rest _args) 0))
                ((symbol-function 'nostr-relay-fetch-events-by-id)
                 (lambda (&rest _args) 0))
                ((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _args)
                   (set-buffer buffer))))
        (nostr-thread-open reply)
        (with-current-buffer "*Nostr Thread root*"
          (unwind-protect
              (let ((text (buffer-substring-no-properties (point-min) (point-max)))
                    (section (nostr-ui-section-at-point)))
                (should (string-match-p "root note" text))
                (should (string-match-p "first reply" text))
                (should (string-match-p "second reply" text))
                (should section)
                (should (equal (nostr-ui-section-id section) "reply-2"))
                (should (equal nostr-thread-focus-id "reply-2")))
            (kill-buffer (current-buffer))))))))

(ert-deftest nostr-thread-open-embedded-nevent-dispatches-selected-note-link ()
  "Thread drill-in opens an embedded nevent from the selected note."
  (let (opened)
    (cl-letf (((symbol-function 'nostr-open-identifier)
               (lambda (value) (setq opened value))))
      (with-temp-buffer
        (nostr-thread-mode)
        (let ((inhibit-read-only t)
              (nostr-ui-show-avatars nil))
          (nostr-ui-clear)
          (nostr-ui-insert-note
           '((id . "note-with-nevent")
             (pubkey . "alice")
             (created-at . 1736776800)
             (content . "see nostr:nevent1qqqqqqqqqqqqqq")
             (replies . 0)
             (reactions . 0)
             (reposts . 0))))
        (nostr-ui-goto-first-section)
        (nostr-thread-open-embedded-nevent)))
    (should (equal opened "nevent1qqqqqqqqqqqqqq"))))

(ert-deftest nostr-ui-section-toggle-and-selection ()
  (with-temp-buffer
    (nostr-timeline-mode)
    (let ((event (nostr-db--event-row-to-alist
                  '("root" "alice" 100 1 nil "hello" "sig" "relay" nil nil nil
                    "Alice" nil nil 0 0 0))))
      (let ((inhibit-read-only t))
        (nostr-ui-clear)
        (nostr-ui-insert-note event))
      (nostr-ui-goto-first-section)
      (should (equal (alist-get 'id (nostr-ui-selected-data)) "root"))
      (should (overlayp nostr-ui--selection-overlay))
      (should (eq (overlay-get nostr-ui--selection-overlay 'face)
                  'nostr-ui-selected-section))
      (nostr-ui-toggle-section)
      (should (nostr-ui-section-folded (nostr-ui-section-at-point)))
      (nostr-ui-toggle-section)
      (should-not (nostr-ui-section-folded (nostr-ui-section-at-point))))))

(ert-deftest nostr-ui-note-heading-does-not-steal-ret ()
  "Navigating to a note heading should leave RET for the mode command."
  (with-temp-buffer
    (nostr-timeline-mode)
    (let ((inhibit-read-only t))
      (nostr-ui-clear)
      (nostr-ui-insert-note
       '((id . "root") (pubkey . "alice") (created-at . 1) (content . "hello"))))
    (nostr-ui-goto-first-section)
    (should (equal (alist-get 'id (nostr-ui-selected-data)) "root"))
    (should-not (button-at (point)))
    (should (eq (lookup-key nostr-timeline-mode-map (kbd "RET"))
                #'nostr-timeline-open-thread))
    (nostr-ui-toggle-section)
    (should (nostr-ui-section-folded (nostr-ui-section-at-point)))))

(ert-deftest nostr-ui-section-navigation-updates-selection-overlay ()
  (with-temp-buffer
    (nostr-timeline-mode)
    (let ((inhibit-read-only t))
      (nostr-ui-clear)
      (nostr-ui-insert-note
       '((id . "first") (pubkey . "alice") (created-at . 1) (content . "first")))
      (nostr-ui-insert-note
       '((id . "second") (pubkey . "bob") (created-at . 2) (content . "second"))))
    (nostr-ui-goto-first-section)
    (should (equal (alist-get 'id (nostr-ui-selected-data)) "first"))
    (let ((first-start (overlay-start nostr-ui--selection-overlay)))
      (nostr-ui-next-section)
      (should (equal (alist-get 'id (nostr-ui-selected-data)) "second"))
      (should (/= first-start (overlay-start nostr-ui--selection-overlay))))
    (nostr-ui-prev-section)
    (should (equal (alist-get 'id (nostr-ui-selected-data)) "first"))))

(ert-deftest nostr-ui-empty-state-and-footer-render ()
  (with-temp-buffer
    (nostr-ui-insert-empty-state "No data." "Try refreshing.")
    (nostr-ui-insert-footer '("g refresh" "q quit"))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "No data." text))
      (should (string-match-p "Try refreshing." text))
      (should (string-match-p "g refresh   q quit" text)))))

(ert-deftest nostr-timeline-renders-buffer-from-db ()
  (nostr-test-with-db
    (nostr-db-store-event
     '((id . "contacts")
       (pubkey . "me")
       (created_at . 91)
       (kind . 3)
       (tags . (("p" "alice")))))
    (nostr-db-store-event (nostr-event-normalize nostr-test-root-event "relay"))
    (with-temp-buffer
      (nostr-timeline-mode)
      (setq-local nostr-timeline-current-pubkey "me")
      (nostr-timeline-refresh)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "\\[Nostr\\]  Feed" text))
        (should (string-match-p "\\[f Feed\\]" text))
        (should (string-match-p " C Conversations " text))
        (should (string-match-p " N Notifications " text))
        (should-not (string-match-p "Feed: home" text))
        (should-not (string-match-p "P posts   M media   c compose" text))
        (should (string-match-p "hello" text))
        (should (string-match-p "\\[image: http://example.test/a.png\\]" text))))))

(ert-deftest nostr-timeline-refresh-preserves-selected-card ()
  "Refreshing a feed keeps point on the same note instead of jumping to top."
  (nostr-test-with-db
    (nostr-db-store-event
     '((id . "contacts")
       (pubkey . "me")
       (created_at . 91)
       (kind . 3)
       (tags . (("p" "alice")))))
    (nostr-test-store-text-note "newer" "alice" 200 "newer note")
    (nostr-test-store-text-note "older" "alice" 100 "older note")
    (cl-letf (((symbol-function 'nostr-relay-fetch-profile) (lambda (&rest _) 0))
              ((symbol-function 'nostr-relay-fetch-event-metadata) (lambda (&rest _) 0)))
      (with-temp-buffer
        (nostr-timeline-mode)
        (setq-local nostr-timeline-current-pubkey "me")
        (nostr-timeline-refresh)
        (goto-char (point-min))
        (search-forward "older note")
        (should (equal (alist-get 'id (nostr-ui-selected-data)) "older"))
        (nostr-timeline-refresh)
        (should (equal (alist-get 'id (nostr-ui-selected-data)) "older"))
        (should (looking-at-p "older note\\|$"))))))

(defun nostr-test-seed-timeline-feeds ()
  "Seed DB with notes covering each timeline feed mode."
  (nostr-db-store-event
   '((id . "contacts")
     (pubkey . "me")
     (created_at . 91)
     (kind . 3)
     (tags . (("p" "alice") ("p" "bob")))))
  (nostr-test-store-text-note "home-root" "alice" 100 "home root")
  (nostr-test-store-text-note "home-reply" "bob" 101 "home reply" "home-root" "home-root")
  (nostr-test-store-text-note "own-root" "me" 102 "own root")
  (nostr-test-store-text-note "own-reply" "me" 103 "own reply" "home-root" "home-root")
  (nostr-test-store-text-note "media-note" "alice" 104 "media https://example.test/pic.jpg")
  (nostr-test-store-text-note "global-root" "carol" 105 "global root"))

(defun nostr-test-render-timeline-feed (kind)
  "Return rendered timeline text for KIND."
  (with-temp-buffer
    (nostr-timeline-mode)
    (setq-local nostr-timeline-current-pubkey "me")
    (setq-local nostr-timeline-feed-kind kind)
    (nostr-timeline-refresh)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest nostr-timeline-feed-modes-filter-cache ()
  "Timeline feed modes render distinct cached event sets."
  (nostr-test-with-db
    (nostr-test-seed-timeline-feeds)
    (nostr-db-store-discover-result "primal" "global" "trending" "global-root" 1 10 110)
    (let ((home (nostr-test-render-timeline-feed 'feed))
          (replies (nostr-test-render-timeline-feed 'conversations))
          (discover (nostr-test-render-timeline-feed 'discover))
          (global (nostr-test-render-timeline-feed 'global))
          (posts (nostr-test-render-timeline-feed 'my-posts))
          (media (nostr-test-render-timeline-feed 'media)))
      (should (string-match-p "\\[Nostr\\]  Feed" home))
      (should (string-match-p "home root" home))
      (should-not (string-match-p "own root" home))
      (should-not (string-match-p "home reply" home))
      (should-not (string-match-p "global root" home))
      (should (string-match-p "\\[Nostr\\]  Conversations" replies))
      (should (string-match-p "\\[C Conversations\\]" replies))
      (should (string-match-p "home reply" replies))
      (should-not (string-match-p "own reply" replies))
      (should-not (string-match-p "home root" replies))
      (should (string-match-p "\\[Nostr\\]  Discover" discover))
      (should (string-match-p "\\[d Discover\\]" discover))
      (should (string-match-p "global root" discover))
      (should (string-match-p "\\[Nostr\\]  Global" global))
      (should-not (string-match-p "G Global" global))
      (should (string-match-p "global root" global))
      (should (string-match-p "home root" global))
      (should (string-match-p "home reply" global))
      (should (string-match-p "\\[Nostr\\]  My Posts" posts))
      (should (string-match-p "\\[P My Posts\\]" posts))
      (should (string-match-p "own root" posts))
      (should (string-match-p "own reply" posts))
      (should-not (string-match-p "home root" posts))
      (should (string-match-p "\\[Nostr\\]  Media" media))
      (should-not (string-match-p "\\[M Media\\]" media))
      (should (string-match-p "media https://example.test/pic.jpg" media))
      (should-not (string-match-p "home root" media)))))

(ert-deftest nostr-timeline-discover-does-not-auto-fetch-media ()
  "Discover disables aggressive media downloads even when auto-preview is on."
  (nostr-test-with-db
    (nostr-test-store-text-note
     "discover-media" "carol" 120 "media https://example.test/discover.jpg")
    (nostr-db-store-discover-result
     "primal" "global" "trending" "discover-media" 1 10 120)
    (let ((nostr-media-auto-preview t)
          (nostr-media-fetch-function
           (lambda (&rest _)
             (ert-fail "Discover should not auto-fetch media"))))
      (with-temp-buffer
        (nostr-timeline-mode)
        (setq-local nostr-timeline-current-pubkey "me")
        (setq-local nostr-timeline-feed-kind 'discover)
        (nostr-timeline-refresh)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "\\[image: https://example.test/discover.jpg\\]"
                                  text))
          (should-not (string-match-p "\\[image loaded:" text)))))))

(ert-deftest nostr-timeline-feed-refresh-backfills-missing-repost-targets ()
  "Feed refresh requests original notes referenced only by cached reposts."
  (nostr-test-with-db
    (nostr-db-store-event
     '((id . "contacts")
       (pubkey . "me")
       (created_at . 90)
       (kind . 3)
       (tags . (("p" "alice")))))
    (nostr-db-store-event
     '((id . "missing-repost")
       (pubkey . "alice")
       (created_at . 121)
       (kind . 6)
       (tags . (("e" "missing-target")))
       (content . "")
       (sig . "sig")))
    (let (requested)
      (cl-letf (((symbol-function 'nostr-relay-fetch-events-by-id)
                 (lambda (event-ids &optional _limit)
                   (setq requested event-ids)
                   1))
                ((symbol-function 'nostr-relay-fetch-event-metadata)
                 (lambda (&rest _args) 0))
                ((symbol-function 'nostr-relay-fetch-profile)
                 (lambda (&rest _args) 0)))
        (with-temp-buffer
          (nostr-timeline-mode)
          (setq-local nostr-timeline-current-pubkey "me")
          (setq-local nostr-timeline-feed-kind 'feed)
          (nostr-timeline-refresh))
        (should (equal requested '("missing-target")))))))

(ert-deftest nostr-timeline-tabs-are-interactive ()
  "Timeline feed tabs expose button actions."
  (nostr-test-with-db
    (with-temp-buffer
      (nostr-timeline-mode)
      (setq-local nostr-timeline-current-pubkey "me")
      (nostr-timeline-refresh)
      (goto-char (point-min))
      (search-forward "d Discover")
      (let ((button (button-at (point))))
        (should button)
        (cl-letf (((symbol-function 'nostr-discover-refresh)
                   (lambda (&optional _append) nil)))
          (button-activate button)))
      (should (eq nostr-timeline-feed-kind 'discover))
      (should (string-match-p "\\[d Discover\\]" (buffer-string))))))

(ert-deftest nostr-db-discover-feed-uses-provider-order-and-stats ()
  "Discover selections preserve provider rank and provider stats."
  (nostr-test-with-db
    (nostr-test-store-text-note "older" "alice" 100 "older")
    (nostr-test-store-text-note "newer" "bob" 200 "newer")
    (nostr-db-store-discover-stats
     '((event_id . "older") (likes . 9) (replies . 2) (reposts . 3)
       (zaps . 1) (satszapped . 42) (score . 10) (score24h . 99)))
    (nostr-db-store-discover-result "primal" "global" "trending" "older" 1 99 300)
    (nostr-db-store-discover-result "primal" "global" "trending" "newer" 2 10 300)
    (let ((events (nostr-db-select-discover-feed "primal" "global" "trending" 10)))
      (should (equal (mapcar (lambda (event) (alist-get 'id event)) events)
                     '("older" "newer")))
      (should (= (alist-get 'reactions (car events)) 9))
      (should (= (alist-get 'replies (car events)) 2))
      (should (= (alist-get 'reposts (car events)) 3))
      (should (= (alist-get 'zaps (car events)) 1))
      (should (= (alist-get 'zap-sats (car events)) 42))
      (should (= (nostr-db-discover-next-cursor "primal" "global" "trending") 10)))))

(ert-deftest nostr-discover-primal-frames-store-ranked-page ()
  "Primal cache EVENT/EOSE frames populate Discover cache rows."
  (nostr-test-with-db
    (let ((nostr-relay-verify-events nil)
          (nostr-discover-provider 'primal)
          (nostr-discover-scope "global")
          (nostr-discover-timeframe "trending")
          (state (list :events nil :order nil)))
      (nostr-discover--handle-frame
       "wss://cache.example"
       (json-encode
        `["EVENT" "sub"
          ((id . "note-a")
           (pubkey . "alice")
           (created_at . 100)
           (kind . 1)
           (tags . [])
           (content . "top")
           (sig . "sig"))])
       state nil)
      (nostr-discover--handle-frame
       "wss://cache.example"
       (json-encode
        `["EVENT" "sub"
          ((id . "stats-a")
           (pubkey . "cache")
           (created_at . 101)
           (kind . 10000100)
           (tags . [])
           (content . ,(json-encode
                        '((event_id . "note-a")
                          (likes . 4)
                          (replies . 3)
                          (reposts . 2)
                          (zaps . 1)
                          (satszapped . 21)
                          (score . 8)
                          (score24h . 7))))
           (sig . "sig"))])
       state nil)
      (nostr-discover--handle-frame
       "wss://cache.example"
       (json-encode
        `["EVENT" "sub"
          ((id . "order")
           (pubkey . "cache")
           (created_at . 102)
           (kind . 10000113)
           (tags . [])
           (content . ,(json-encode '((elements . ["note-a"]))))
           (sig . "sig"))])
       state nil)
      (nostr-discover--handle-frame
       "wss://cache.example"
       (json-encode ["EOSE" "sub"])
       state nil)
      (let ((events (nostr-db-select-discover-feed "primal" "global" "trending" 10)))
        (should (= (length events) 1))
        (should (equal (alist-get 'id (car events)) "note-a"))
        (should (= (alist-get 'reactions (car events)) 4))
        (should (= (nostr-db-discover-next-cursor "primal" "global" "trending") 7))))))

(ert-deftest nostr-timeline-refresh-backfills-visible-profile-metadata ()
  "Rendering a feed requests missing profile and note metadata."
  (nostr-test-with-db
    (nostr-test-store-text-note "global-author" "alice-pubkey" 100 "global note")
    (let (requested-profiles requested-events)
      (cl-letf (((symbol-function 'nostr-relay-fetch-profile)
                 (lambda (pubkey &optional _url)
                   (push pubkey requested-profiles)
                   1))
                ((symbol-function 'nostr-relay-fetch-event-metadata)
                 (lambda (event-ids &optional _limit)
                   (setq requested-events event-ids)
                   1)))
        (with-temp-buffer
          (nostr-timeline-mode)
          (setq-local nostr-timeline-current-pubkey "me")
          (setq-local nostr-timeline-feed-kind 'global)
          (nostr-timeline-refresh)))
      (should (member "alice-pubkey" requested-profiles))
      (should (member "global-author" requested-events)))))

(ert-deftest nostr-timeline-feed-switch-commands-refresh-buffer ()
  "Timeline feed commands update buffer-local feed state."
  (nostr-test-with-db
    (nostr-test-seed-timeline-feeds)
    (with-temp-buffer
      (nostr-timeline-mode)
      (setq-local nostr-timeline-current-pubkey "me")
      (nostr-timeline-conversations)
      (should (eq nostr-timeline-feed-kind 'conversations))
      (should (string-match-p "\\[Nostr\\]  Conversations" (buffer-string)))
      (nostr-timeline-global)
      (should (eq nostr-timeline-feed-kind 'global))
      (should (string-match-p "\\[Nostr\\]  Global" (buffer-string)))
      (nostr-timeline-my-posts)
      (should (eq nostr-timeline-feed-kind 'my-posts))
      (should (string-match-p "\\[Nostr\\]  My Posts" (buffer-string)))
      (nostr-timeline-media)
      (should (eq nostr-timeline-feed-kind 'media))
      (should (string-match-p "\\[Nostr\\]  Media" (buffer-string)))
      (nostr-timeline-feed)
      (should (eq nostr-timeline-feed-kind 'feed))
      (should (string-match-p "\\[Nostr\\]  Feed" (buffer-string))))))

(ert-deftest nostr-compose-content-reads-editable-region ()
  (with-temp-buffer
    (nostr-compose-mode)
    (insert "readonly context\n")
    (setq-local nostr-compose-content-start (copy-marker (point)))
    (insert "hello\n")
    (should (equal (nostr-compose--content) "hello"))))

(ert-deftest nostr-compose-header-shows-context-and-count ()
  (with-temp-buffer
    (nostr-compose-mode)
    (setq-local nostr-compose-reply-to '((id . "reply-id")
                                         (author . "Alice")
                                         (content . "original")))
    (setq-local nostr-compose-extra-tags '(("q" "quote-id" "relay")))
    (insert "hello")
    (should (string-match-p "reply: Alice" (nostr-compose--header-line)))
    (should (string-match-p "quote: quote-id" (nostr-compose--header-line)))
    (should (string-match-p "5 chars" (nostr-compose--header-line)))))

(ert-deftest nostr-compose-open-renders-reply-context ()
  (let (opened)
    (cl-letf (((symbol-function 'nostr-compose--display-buffer)
               (lambda (buffer) (setq opened buffer))))
      (nostr-compose-open '((id . "reply-id")
                            (author . "Alice")
                            (content . "hello from alice"))))
    (unwind-protect
        (with-current-buffer opened
          (should (eq major-mode 'nostr-compose-mode))
          (should (string-match-p "Context" (buffer-string)))
          (should (string-match-p "hello from alice" (buffer-string)))
          (should (get-text-property (point-min) 'read-only))
          (should (markerp nostr-compose-content-start))
          (should-not nostr-compose-dirty))
      (when (buffer-live-p opened)
        (with-current-buffer opened
          (setq nostr-compose--sent t))
        (kill-buffer opened)))))

(ert-deftest nostr-compose-open-renders-cached-quote-context ()
  (nostr-test-with-db
    (nostr-db-store-event
     '((id . "quote-id")
       (pubkey . "bob")
       (created_at . 10)
       (kind . 1)
       (tags . nil)
       (content . "quoted content")
       (sig . "sig")
       (relay . "relay")
       (root-id . nil)
       (reply-id . nil)
       (quote-id . nil)))
    (let (opened)
      (cl-letf (((symbol-function 'nostr-compose--display-buffer)
                 (lambda (buffer) (setq opened buffer))))
        (nostr-compose-open nil '(("q" "quote-id" "relay"))))
      (unwind-protect
          (with-current-buffer opened
            (should (string-match-p "Context" (buffer-string)))
            (should (string-match-p "quoted content" (buffer-string)))
            (should (markerp nostr-compose-content-start))
            (should-not nostr-compose-dirty))
        (when (buffer-live-p opened)
          (with-current-buffer opened
            (setq nostr-compose--sent t))
          (kill-buffer opened))))))

(ert-deftest nostr-compose-confirm-kill-prompts-for-dirty-draft ()
  (with-temp-buffer
    (nostr-compose-mode)
    (let (asked)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_prompt) (setq asked t) nil)))
        (insert "draft")
        (should nostr-compose-dirty)
        (should-not (nostr-compose--confirm-kill))
        (should asked)))))

(ert-deftest nostr-compose-cancel-kills-buffer-when-confirmed ()
  (let ((buffer (generate-new-buffer "*nostr-compose-cancel-test*")))
    (with-current-buffer buffer
      (nostr-compose-mode)
      (insert "draft"))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (with-current-buffer buffer
        (nostr-compose-cancel)))
    (should-not (buffer-live-p buffer))))

(ert-deftest nostr-compose-cancel-records-draft-history ()
  (let* ((dir (make-temp-file "nostr-compose-drafts" t))
         (nostr-compose-draft-directory dir)
         (nostr-compose-draft-history-file nil)
         (buffer (generate-new-buffer "*nostr-compose-history-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (nostr-compose-mode)
            (insert "abandoned draft"))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
            (with-current-buffer buffer
              (nostr-compose-cancel)))
          (let ((history (nostr-compose--read-draft-history)))
            (should (equal (alist-get 'content (car history))
                           "abandoned draft"))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq nostr-compose--sent t))
        (kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest nostr-compose-draft-history-cycles-in-current-buffer ()
  (let* ((dir (make-temp-file "nostr-compose-drafts" t))
         (nostr-compose-draft-directory dir)
         (nostr-compose-draft-history-file nil))
    (unwind-protect
        (progn
          (make-directory dir t)
          (nostr-compose--write-draft-history
           '(((content . "newer draft") (updated-at . 2))
             ((content . "older draft") (updated-at . 1))))
          (with-temp-buffer
            (nostr-compose-mode)
            (setq-local nostr-compose-content-start (copy-marker (point)))
            (insert "current draft")
            (nostr-compose-previous-draft)
            (should (equal (nostr-compose--content) "newer draft"))
            (nostr-compose-previous-draft)
            (should (equal (nostr-compose--content) "older draft"))
            (nostr-compose-next-draft)
            (should (equal (nostr-compose--content) "newer draft"))))
      (delete-directory dir t))))

(ert-deftest nostr-compose-drafts-default-to-temporary-directory ()
  "Draft autosaves and history are temporary by default."
  (should (file-in-directory-p nostr-compose-draft-directory
                               temporary-file-directory))
  (should (equal (nostr-compose--draft-history-path)
                 (expand-file-name "history.el"
                                   nostr-compose-draft-directory))))

(ert-deftest nostr-media-cache-defaults-to-temporary-directory ()
  "Downloaded media defaults to temporary storage."
  (should (file-in-directory-p nostr-media-cache-directory
                               temporary-file-directory)))

(ert-deftest nostr-compose-mention-completion-uses-cached-profile-names ()
  "Compose mention completion offers cached display names and usernames."
  (nostr-test-with-db
    (nostr-db-store-event
     '((id . "alice-profile")
       (pubkey . "alice-pubkey")
       (created_at . 10)
       (kind . 0)
       (tags . nil)
       (content . "{\"name\":\"alice\",\"display_name\":\"Alice Example\",\"nip05\":\"alice@example.test\"}")
       (sig . "sig")))
    (let ((pairs (nostr-compose--completion-candidates)))
      (should (equal (cdr (assoc "Alice Example" pairs)) "alice-pubkey"))
      (should (equal (cdr (assoc "alice" pairs)) "alice-pubkey"))
      (should (equal (cdr (assoc "alice@example.test" pairs)) "alice-pubkey")))))

(ert-deftest nostr-compose-mention-completion-yields-odell-for-at-o ()
  "Compose mention completion offers ODELL after typing @O."
  (nostr-test-with-db
    (nostr-db-store-event
     '((id . "odell-profile")
       (pubkey . "odell-pubkey")
       (created_at . 10)
       (kind . 0)
       (tags . nil)
       (content . "{\"name\":\"ODELL\",\"display_name\":\"ODELL\"}")
       (sig . "sig")))
    (with-temp-buffer
      (nostr-compose-mode)
      (insert "@O")
      (let* ((capf (nostr-compose-complete-mention))
             (start (nth 0 capf))
             (end (nth 1 capf))
             (table (nth 2 capf)))
        (should (equal (buffer-substring-no-properties start end) "O"))
        (should (member "ODELL" (all-completions "O" table)))))))

(ert-deftest nostr-compose-mention-completion-sources-follow-petnames ()
  "Compose mention completion offers followed contact petnames."
  (nostr-test-with-db
    (let ((nostr-current-pubkey "me")
          (nostr-compose-mention-completion-limit 1))
      (nostr-db-store-event
       '((id . "alice-profile")
         (pubkey . "alice-pubkey")
         (created_at . 10)
         (kind . 0)
         (tags . nil)
         (content . "{\"name\":\"Alice\"}")
         (sig . "sig")))
      (nostr-db-store-event
       '((id . "contacts")
         (pubkey . "me")
         (created_at . 11)
         (kind . 3)
         (tags . (("p" "odell-pubkey" nil "ODELL")))))
      (with-temp-buffer
        (nostr-compose-mode)
        (insert "@ODELL")
        (let* ((capf (nostr-compose-complete-mention))
               (start (nth 0 capf))
               (end (nth 1 capf))
               (table (nth 2 capf)))
          (should (equal (buffer-substring-no-properties start end) "ODELL"))
          (should (member "ODELL" (all-completions "ODELL" table)))
          (should (member "ODELL" (all-completions "odell" table))))))))

(ert-deftest nostr-compose-mention-completion-replaces-trigger-with-npub ()
  "Finished mention completion replaces @text with a NIP-19 profile reference."
  (nostr-test-with-db
    (nostr-db-store-event
     '((id . "alice-profile")
       (pubkey . "alice-pubkey")
       (created_at . 10)
       (kind . 0)
       (tags . nil)
       (content . "{\"name\":\"alice\",\"display_name\":\"Alice Example\"}")
       (sig . "sig")))
    (cl-letf (((symbol-function 'nostr-nip19-encode-sync)
               (lambda (entity value)
                 (should (equal entity "npub"))
                 (should (equal value "alice-pubkey"))
                 '((value . "npub1alice")))))
      (with-temp-buffer
        (nostr-compose-mode)
        (insert "@ali")
        (let* ((capf (nostr-compose-complete-mention))
               (start (nth 0 capf))
               (end (nth 1 capf))
               (exit (plist-get (nthcdr 3 capf) :exit-function)))
          (should (equal (buffer-substring-no-properties start end) "ali"))
          (delete-region start end)
          (insert "Alice Example")
          (funcall exit "Alice Example" 'finished)
          (should (equal (buffer-string) "nostr:npub1alice")))))))

(ert-deftest nostr-compose-at-key-starts-mention-completion ()
  "The @ key inserts a mention trigger and invokes completion."
  (with-temp-buffer
    (nostr-compose-mode)
    (should (eq (lookup-key nostr-compose-mode-map (kbd "@"))
                #'nostr-compose-insert-mention-trigger))
    (let (completed)
      (cl-letf (((symbol-function 'completion-at-point)
                 (lambda () (setq completed t))))
        (nostr-compose-insert-mention-trigger))
      (should (equal (buffer-string) "@"))
      (should completed))))

(ert-deftest nostr-compose-send-empty-signals-user-error ()
  (with-temp-buffer
    (nostr-compose-mode)
    (should-error (nostr-compose-send) :type 'user-error)))

(ert-deftest nostr-actions-reaction-labels-use-configured-choices ()
  "Reaction transient labels show configured emoji/content values."
  (let ((nostr-reaction-choices '("+" "🔥" "🤙" "👀" "💜")))
    (should (equal (nostr-actions--reaction-choice-label-1) "+"))
    (should (equal (nostr-actions--reaction-choice-label-2) "🔥"))
    (should (equal (nostr-actions--reaction-choice-label-3) "🤙"))
    (should (equal (nostr-actions--reaction-choice-label-4) "👀"))
    (should (equal (nostr-actions--reaction-choice-label-5) "💜"))))

(ert-deftest nostr-actions-reaction-menu-does-not-echo-choice-list ()
  "Opening the reaction transient does not also echo the choices."
  (let ((event '((id . "note1")))
        messages opened)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages)))
              ((symbol-function 'nostr-actions-reaction-transient)
               (lambda () (setq opened t))))
      (nostr-actions-react-menu event))
    (should opened)
    (should (equal nostr-actions--reaction-event event))
    (should-not messages)))

(ert-deftest nostr-compose-extra-tags-are-sent ()
  (let (captured-kind captured-tags captured-content stored-event hook-event)
    (cl-letf (((symbol-function 'nostr-backend-sign-event)
               (lambda (kind tags content success _error)
                 (setq captured-kind kind
                       captured-tags tags
                       captured-content content)
                 (funcall success '((client_message . "[\"EVENT\",{}]")
                                    (event . ((id . "signed")
                                              (kind . 1)
                                              (created_at . 1)
                                              (tags . nil)))))))
              ((symbol-function 'nostr-relay-send-client-message)
               (lambda (_message) 2))
              ((symbol-function 'nostr-db-store-event)
               (lambda (event) (setq stored-event event))))
      (with-temp-buffer
        (nostr-compose-mode)
        (setq-local nostr-compose-extra-tags '(("q" "quote-id" "relay")))
        (let ((nostr-actions-after-send-hook
               (list (lambda (event) (setq hook-event event)))))
          (insert "quoted note")
          (nostr-compose-send))))
    (should (= captured-kind nostr-kind-text-note))
    (should (equal captured-tags '(("q" "quote-id" "relay"))))
    (should (equal captured-content "quoted note"))
    (should (equal (alist-get 'id stored-event) "signed"))
    (should (equal (alist-get 'id hook-event) "signed"))))

(ert-deftest nostr-compose-send-replaces-attachment-token-before-signing ()
  (let (captured-content)
    (cl-letf (((symbol-function 'nostr-compose--upload-attachments)
               (lambda (_paths success _error)
                 (funcall success '(("/tmp/pic.jpg" . "https://cdn.example/pic.jpg")))))
              ((symbol-function 'nostr-backend-sign-event)
               (lambda (_kind _tags content success _error)
                 (setq captured-content content)
                 (funcall success '((client_message . "[\"EVENT\",{}]")
                                    (event . ((id . "signed")
                                              (kind . 1)
                                              (created_at . 1)
                                              (tags . nil)))))))
              ((symbol-function 'nostr-relay-send-client-message)
               (lambda (_message) 1))
              ((symbol-function 'nostr-db-store-event)
               (lambda (_event) t)))
      (with-temp-buffer
        (nostr-compose-mode)
        (insert "photo\n[attachment:/tmp/pic.jpg]\n")
        (nostr-compose-send)))
    (should (equal captured-content "photo\nhttps://cdn.example/pic.jpg"))))

(ert-deftest nostr-compose-upload-uses-unibyte-binary-request ()
  (let ((file (make-temp-file "nostr-upload" nil ".bin"))
        captured-data
        captured-headers)
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert (unibyte-string #x89 #x50 #xff #x00))
            (write-region (point-min) (point-max) file nil 'silent))
          (cl-letf (((symbol-function 'nostr-backend-blossom-auth)
                     (lambda (_server _sha _expiration success _error)
                       (funcall success
                                `((authorization . ,(copy-sequence "Nostr test"))))))
                    ((symbol-function 'url-retrieve)
                     (lambda (_url _callback &rest _args)
                       (setq captured-data url-request-data)
                       (setq captured-headers url-request-extra-headers))))
            (let ((nostr-blossom-upload-server "https://blossom.example"))
              (nostr-upload-file file #'ignore #'ignore)))
          (should-not (multibyte-string-p captured-data))
          (dolist (header captured-headers)
            (should-not (multibyte-string-p (car header)))
            (should-not (multibyte-string-p (cdr header)))))
      (delete-file file))))

(ert-deftest nostr-compose-upload-sanitizes-binary-request-errors ()
  (should (equal (nostr-upload-sanitize-error
                  "Multibyte text in HTTP request: PUT /upload ...binary...")
                 "Could not build binary upload request")))

(ert-deftest nostr-actions-like-and-repost-build-public-events ()
  (let (sent)
    (cl-letf (((symbol-function 'nostr-backend-sign-event)
               (lambda (kind tags content success _error)
                 (push (list kind tags content) sent)
                 (funcall success `((client_message . "[\"EVENT\",{}]")
                                    (event . ((id . ,(format "signed-%s" kind))
                                              (kind . ,kind)
                                              (created_at . 1)
                                              (tags . ,tags)))))))
              ((symbol-function 'nostr-relay-send-client-message)
               (lambda (_message) t))
              ((symbol-function 'nostr-db-store-event)
               (lambda (_event) t)))
      (let ((event '((id . "note1")
                     (pubkey . "alice")
                     (kind . 1)
                     (relay . "wss://relay.example")
                     (content . "hello"))))
        (nostr-actions-like event)
        (nostr-actions-repost event)))
    (should (= (length sent) 2))
    (should (= (caar (last sent)) nostr-kind-reaction))
    (should (= (caar sent) nostr-kind-repost))))

(ert-deftest nostr-actions-reaction-refreshes-target-counts ()
  "Sending a reaction refreshes the reacted note footer immediately."
  (let (refreshed)
    (cl-letf (((symbol-function 'nostr-backend-sign-event)
               (lambda (kind tags _content success _error)
                 (funcall success `((client_message . "[\"EVENT\",{}]")
                                    (event . ((id . "signed-reaction")
                                              (kind . ,kind)
                                              (created_at . 1)
                                              (tags . ,tags)))))))
              ((symbol-function 'nostr-relay-send-client-message) #'ignore)
              ((symbol-function 'nostr-db-store-event) #'ignore)
              ((symbol-function 'nostr-ui-refresh-note-counts)
               (lambda (event-id) (setq refreshed event-id))))
      (nostr-actions-like '((id . "note1")
                            (pubkey . "alice")
                            (kind . 1)
                            (relay . "wss://relay.example"))))
    (should (equal refreshed "note1"))))

(ert-deftest nostr-actions-generated-key-reaction-updates-visible-card ()
  "Generate a fresh npub, send a signed reaction, and confirm the card updates."
  (let ((backend-command (nostr-test-backend-command)))
    (skip-unless backend-command)
    (nostr-test-with-db
      (let* ((nostr-backend-command backend-command)
             (key-result (nostr-backend-call-sync
                          "generate-key"
                          (make-hash-table :test 'equal)))
             (key-response (cdr key-result))
             (secret (alist-get 'secret_key key-response))
             (pubkey (alist-get 'pubkey key-response))
             (npub (alist-get 'npub key-response))
             (nostr-current-pubkey pubkey)
             (nostr-relay-urls '("wss://relay.example"))
             sent-message)
        (should (zerop (car key-result)))
        (should (alist-get 'ok key-response))
        (should (and (stringp secret) (not (string-empty-p secret))))
        (should (and (stringp pubkey) (= (length pubkey) 64)))
        (should (string-prefix-p "npub1" npub))
        (nostr-test-store-text-note "visual-reaction-target" "alice" 100
                                    "react to me")
        (with-temp-buffer
          (nostr-ui-insert-note
           '((id . "visual-reaction-target")
             (pubkey . "alice")
             (created-at . 100)
             (kind . 1)
             (relay . "wss://relay.example")
             (content . "react to me")))
          (should (string-match-p "♥ 0"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))
          (cl-letf (((symbol-function 'nostr-backend-load-secret)
                     (lambda () secret))
                    ((symbol-function 'nostr-relay-send-client-message)
                     (lambda (client-message)
                       (setq sent-message client-message))))
            (nostr-actions-like
             '((id . "visual-reaction-target")
               (pubkey . "alice")
               (kind . 1)
               (relay . "wss://relay.example")))
            (should (nostr-test-wait-until
                     (lambda ()
                       (string-match-p
                        "♥ 1"
                        (buffer-substring-no-properties
                         (point-min) (point-max))))
                     5)))
          (should (string-prefix-p "[\"EVENT\"" sent-message))
          (should (equal
                   (emacsql nostr-db--connection
                            [:select [pubkey content]
                                     :from reactions
                                     :where (= event_id "visual-reaction-target")])
                   `((,pubkey "+"))))
          (should (= (alist-get 'reactions
                                (nostr-db-event-counts "visual-reaction-target"))
                     1))
          (should (string-match-p "♥ 1"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max)))))))))

(ert-deftest nostr-actions-follow-tags-preserve-existing-follows ()
  (nostr-test-with-db
    (let ((nostr-current-pubkey "me"))
      (nostr-db-store-event
       '((id . "contacts")
         (pubkey . "me")
         (created_at . 10)
         (kind . 3)
         (tags . (("p" "alice" "relay" "Alice")))))
      (should (equal (nostr-actions-current-follow-tags "bob" nil)
                     '(("p" "alice" "relay" "Alice")
                       ("p" "bob"))))
      (should (equal (nostr-actions-current-follow-tags nil "alice")
                     nil)))))

(ert-deftest nostr-db-stores-nip51-mute-list ()
  (nostr-test-with-db
    (nostr-db-store-event
     '((id . "mutes")
       (pubkey . "me")
       (created_at . 10)
       (kind . 10000)
       (tags . (("p" "alice") ("p" "bob") ("e" "note1")))))
    (should (equal (nostr-db-select-mutes "me") '("alice" "bob")))
    (should (nostr-db-muted-p "me" "alice"))
    (nostr-db-store-event
     '((id . "mutes-new")
       (pubkey . "me")
       (created_at . 11)
       (kind . 10000)
       (tags . (("p" "carol")))))
    (should (equal (nostr-db-select-mutes "me") '("carol")))
    (should-not (nostr-db-muted-p "me" "alice"))))

(ert-deftest nostr-actions-mute-tags-preserve-existing-mutes ()
  (nostr-test-with-db
    (let ((nostr-current-pubkey "me"))
      (nostr-db-store-event
       '((id . "mutes")
         (pubkey . "me")
         (created_at . 10)
         (kind . 10000)
         (tags . (("p" "alice")))))
      (should (equal (nostr-actions-current-mute-tags "bob" nil)
                     '(("p" "alice")
                       ("p" "bob"))))
      (should (equal (nostr-actions-current-mute-tags nil "alice")
                     nil)))))

(ert-deftest nostr-relay-stores-mention-reaction-and-follow-notifications ()
  (nostr-test-with-db
    (let ((nostr-current-pubkey "me")
          (nostr-relay-verify-events nil))
      (nostr-relay--handle-event
       "relay" nil
       '((id . "my-note")
         (pubkey . "me")
         (created_at . 1)
         (kind . 1)
         (tags . nil)
         (content . "mine")
         (sig . "sig")))
      (nostr-relay--handle-event
       "relay" nil
       '((id . "mention")
         (pubkey . "alice")
         (created_at . 2)
         (kind . 1)
         (tags . (("p" "me")))
         (content . "hi")
         (sig . "sig")))
      (nostr-relay--handle-event
       "relay" nil
       '((id . "reaction")
         (pubkey . "bob")
         (created_at . 3)
         (kind . 7)
         (tags . (("e" "my-note")))
         (content . "+")
         (sig . "sig")))
      (nostr-relay--handle-event
       "relay" nil
       '((id . "follow")
         (pubkey . "carol")
         (created_at . 4)
         (kind . 3)
         (tags . (("p" "me")))
         (content . "")
         (sig . "sig")))
      (should (equal (sort (mapcar #'car
                                    (emacsql nostr-db--connection
                                             [:select [type] :from notifications]))
                           #'string<)
                     '("follow" "mention" "reaction"))))))

(ert-deftest nostr-relay-verifies-events-before-storing ()
  "Relay ingestion verifies event signatures before writing to the cache."
  (nostr-test-with-db
    (let ((calls nil))
      (cl-letf (((symbol-function 'nostr-backend-call)
                 (lambda (command payload success _error)
                   (push (list command payload) calls)
                   (funcall success '((ok . t) (valid . t)))
                   :process)))
        (should (eq (nostr-relay--handle-event
                     "relay"
                     "sub"
                     '((id . "verified-note")
                       (pubkey . "alice")
                       (created_at . 100)
                       (kind . 1)
                       (tags . nil)
                       (content . "verified")
                       (sig . "sig")))
                    'pending-verification)))
      (should (equal (caar calls) "verify-event"))
      (should (equal (alist-get 'id (alist-get 'event (cadar calls)))
                     "verified-note"))
      (should (equal (nostr-db-event-pubkey "verified-note") "alice")))))

(ert-deftest nostr-relay-rejects-invalid-events ()
  "Invalid relay events are not stored and leave a relay status clue."
  (nostr-test-with-db
    (let ((invalid-response '((ok . t)
                              (valid . nil)
                              (reason . "bad signature")))
          handled)
      (cl-letf (((symbol-function 'nostr-backend-call)
                 (lambda (_command _payload success _error)
                   (funcall success invalid-response)
                   :process)))
        (setq handled
              (nostr-relay--handle-event
               "relay"
               "sub"
               '((id . "invalid-note")
                 (pubkey . "mallory")
                 (created_at . 100)
                 (kind . 1)
                 (tags . nil)
                 (content . "invalid")
                 (sig . "sig")))))
      (should (eq handled 'pending-verification))
      (should-not (nostr-db-event-pubkey "invalid-note"))
      (let ((status (emacsql nostr-db--connection
                             [:select [state message]
                                      :from relay_status
                                      :where (= url "relay")])))
        (should (equal status
                       '(("invalid-event" "invalid-note bad signature"))))))))

(ert-deftest nostr-relay-tracks-publish-receipts ()
  "Publishing records per-relay pending receipts and OK updates."
  (nostr-test-with-db
    (let ((nostr-current-pubkey "me")
          (nostr-relay--connections (make-hash-table :test #'equal))
          sent)
      (cl-letf (((symbol-function 'websocket-openp)
                 (lambda (_ws) t))
                ((symbol-function 'websocket-send-text)
                 (lambda (ws message)
                   (push (list ws message) sent))))
        (puthash "wss://relay-a.example" 'ws-a nostr-relay--connections)
        (puthash "wss://relay-b.example" 'ws-b nostr-relay--connections)
        (should (= (nostr-relay-send-client-message
                    "[\"EVENT\",{\"id\":\"event-pub\",\"content\":\"hello\"}]")
                   2)))
      (should (= (length sent) 2))
      (should (equal (mapcar (lambda (receipt)
                               (list (alist-get 'url receipt)
                                     (alist-get 'state receipt)))
                             (nostr-db-select-publish-receipts "event-pub"))
                     '(("wss://relay-a.example" "pending")
                       ("wss://relay-b.example" "pending"))))
      (nostr-relay-handle-frame
       "wss://relay-a.example"
       "[\"OK\",\"event-pub\",true,\"stored\"]")
      (nostr-relay-handle-frame
       "wss://relay-b.example"
       "[\"OK\",\"event-pub\",false,\"blocked: policy\"]")
      (should (equal (mapcar (lambda (receipt)
                               (list (alist-get 'url receipt)
                                     (alist-get 'state receipt)
                                     (alist-get 'message receipt)))
                             (nostr-db-select-publish-receipts "event-pub"))
                     '(("wss://relay-a.example" "accepted" "stored")
                       ("wss://relay-b.example" "rejected" "blocked: policy")))))))

(ert-deftest nostr-search-relay-subscribes-connected-relays ()
  (let (sent)
    (cl-letf (((symbol-function 'nostr-relay-subscribe)
               (lambda (url sub-id filters)
                 (push (list url sub-id filters) sent))))
      (let ((nostr-relay--connections (make-hash-table :test #'equal))
            (nostr-relay-search-urls nil))
        (puthash "wss://relay-a.example" t nostr-relay--connections)
        (puthash "wss://relay-b.example" t nostr-relay--connections)
        (let ((sub-id (nostr-relay-search "emacs nostr" 25)))
          (should (string-prefix-p "search-" sub-id))
          (should (= (length sent) 2))
          (pcase-let ((`(,_url _sub-id ,filters) (car sent)))
            (should (= (length filters) 2))
            (should (equal (alist-get "search" (nth 0 filters) nil nil #'equal)
                           "emacs nostr"))
            (should (equal (alist-get "limit" (nth 0 filters) nil nil #'equal)
                           25))
            (should (equal (alist-get "kinds" (nth 0 filters) nil nil #'equal)
                           (list nostr-kind-text-note)))
            (should (equal (alist-get "search" (nth 1 filters) nil nil #'equal)
                           "emacs nostr"))
            (should (equal (alist-get "limit" (nth 1 filters) nil nil #'equal)
                           25))
            (should (equal (alist-get "kinds" (nth 1 filters) nil nil #'equal)
                           (list nostr-kind-metadata)))))))))

(ert-deftest nostr-search-relay-queues-configured-search-relays ()
  "Search relays are connected lazily and receive normalized profile queries."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--pending-subscriptions (make-hash-table :test #'equal))
        (nostr-relay-search-urls '("wss://cache2.primal.net/v1"))
        opened)
    (cl-letf (((symbol-function 'nostr-relay-open)
               (lambda (url pubkey)
                 (setq opened (list url pubkey))
                 nil)))
      (let ((sub-id (nostr-relay-search "@ODELL" 25)))
        (should (string-prefix-p "search-" sub-id))
        (should (equal opened '("wss://cache2.primal.net/v1" nil)))
        (pcase-let ((`((,queued-sub-id ,filters))
                     (gethash "wss://cache2.primal.net/v1"
                              nostr-relay--pending-subscriptions)))
          (should (equal queued-sub-id sub-id))
          (should (= (length filters) 1))
          (should (equal (alist-get "cache" (car filters) nil nil #'equal)
                         '("user_search" (("query" . "odell")
                                          ("limit" . 25))))))))))

(ert-deftest nostr-search-relay-installs-search-refresh-hook ()
  "Relay-backed search refreshes visible search buffers as results arrive."
  (let ((nostr-search--refresh-timer nil)
        (nostr-relay-event-hook nil)
        (refreshes 0))
    (unwind-protect
        (cl-letf (((symbol-function 'nostr-relay-search)
                   (lambda (&rest _) "search-sub"))
                  ((symbol-function 'nostr-search-refresh-visible-buffers)
                   (lambda () (setq nostr-search--refresh-timer nil)
                     (cl-incf refreshes))))
          (let ((nostr-search-query "ODELL"))
            (nostr-search-relay)
            (should (memq #'nostr-search--schedule-refresh nostr-relay-event-hook))
            (run-hook-with-args 'nostr-relay-event-hook nil)
            (should (timerp nostr-search--refresh-timer))
            (cancel-timer nostr-search--refresh-timer)
            (setq nostr-search--refresh-timer nil)
            (nostr-search-refresh-visible-buffers)
            (should (= refreshes 1))))
      (when (timerp nostr-search--refresh-timer)
        (cancel-timer nostr-search--refresh-timer)))))

(ert-deftest nostr-search-primal-cache-profile-hints-bypass-verification ()
  "Primal cache profile search hints are stored even when relay verification is on."
  (nostr-test-with-db
    (let ((nostr-relay-verify-events t)
          (nostr-relay--search-profile-queries (make-hash-table :test #'equal)))
      (puthash "search-odell" "ODELL" nostr-relay--search-profile-queries)
      (nostr-relay--handle-event
       "wss://cache2.primal.net/v1"
       "search-odell"
       '((id . "profile-odell")
         (pubkey . "odell-pubkey")
         (created_at . 100)
         (kind . 0)
         (tags . nil)
         (content . "{\"name\":\"ODELL\",\"display_name\":\"ODELL\"}")
         (sig . "cache-sig")))
      (should (nostr-db-select-profile "odell-pubkey")))))

(ert-deftest nostr-search-progress-appears-in-mode-line ()
  "Relay-backed search requests show and clear compact mode-line progress."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--search-request-counts (make-hash-table :test #'equal))
        (nostr-relay--search-author-request-counts (make-hash-table :test #'equal))
        (nostr-relay--search-timeout-timers (make-hash-table :test #'equal))
        (nostr-relay--search-profile-queries (make-hash-table :test #'equal))
        (nostr-relay--pending-subscriptions (make-hash-table :test #'equal))
        (nostr-relay--profile-requests (make-hash-table :test #'equal))
        (nostr-relay--profile-request-counts (make-hash-table :test #'equal))
        (nostr-relay--profile-request-subscriptions (make-hash-table :test #'equal))
        (nostr-relay--mode-line-string nil)
        (nostr-relay--ingested-event-count 0)
        (nostr-relay--connect-queue nil)
        (nostr-relay-search-urls nil))
    (puthash "wss://relay.example" t nostr-relay--connections)
    (cl-letf (((symbol-function 'nostr-relay-subscribe)
               (lambda (&rest _) nil)))
      (let ((sub-id (nostr-relay-search "ODELL" 25)))
        (should (string-match-p " Nostr [⠋⠙⠹⢸⣰⣠⣄⣆⡇⠏]"
                                nostr-relay--mode-line-string))
        (should (timerp (gethash sub-id nostr-relay--search-timeout-timers)))
        (nostr-relay--note-search-eose sub-id)
        (should-not (gethash sub-id nostr-relay--search-timeout-timers))
        (should-not nostr-relay--mode-line-string)))))

(ert-deftest nostr-search-progress-times-out ()
  "Search progress clears even when a relay never sends EOSE/CLOSED."
  (let ((nostr-relay--search-request-counts (make-hash-table :test #'equal))
        (nostr-relay--search-author-request-counts (make-hash-table :test #'equal))
        (nostr-relay--search-timeout-timers (make-hash-table :test #'equal))
        (nostr-relay--profile-requests (make-hash-table :test #'equal))
        (nostr-relay--mode-line-string nil)
        (nostr-relay--ingested-event-count 0)
        (nostr-relay--connect-queue nil)
        (nostr-relay-search-timeout-seconds 30))
    (unwind-protect
        (progn
          (nostr-relay--track-search-request "search-timeout" 1)
          (should (string-match-p " Nostr [⠋⠙⠹⢸⣰⣠⣄⣆⡇⠏]"
                                  nostr-relay--mode-line-string))
          (should (timerp (gethash "search-timeout"
                                   nostr-relay--search-timeout-timers)))
          (nostr-relay--clear-search-progress "search-timeout")
          (should-not (gethash "search-timeout" nostr-relay--search-request-counts))
          (should-not (gethash "search-timeout" nostr-relay--search-timeout-timers))
          (should-not nostr-relay--mode-line-string))
      (maphash (lambda (_sub-id timer)
                 (when (timerp timer)
                   (cancel-timer timer)))
               nostr-relay--search-timeout-timers))))

(ert-deftest nostr-relay-mode-line-shows-unread-notifications ()
  "Unread notifications appear in the compact Nostr mode-line segment."
  (nostr-test-with-db
    (let ((nostr-relay--search-request-counts (make-hash-table :test #'equal))
          (nostr-relay--search-author-request-counts (make-hash-table :test #'equal))
          (nostr-relay--profile-requests (make-hash-table :test #'equal))
          (nostr-relay--mode-line-string nil)
          (nostr-relay--ingested-event-count 0)
          (nostr-relay--connect-queue nil)
          (global-mode-string nil))
      (nostr-db-store-notification "notif1" "mention" "note1" "alice" "me" 100)
      (nostr-db-store-notification "notif2" "reply" "note2" "bob" "me" 101)
      (nostr-relay--update-mode-line)
      (should (equal nostr-relay--mode-line-string " Nostr ◉2"))
      (should (member '(:eval nostr-relay--mode-line-string) global-mode-string))
      (nostr-db-mark-all-notifications-seen)
      (nostr-relay--update-mode-line)
      (should-not nostr-relay--mode-line-string))))

(ert-deftest nostr-relay-mode-line-combines-spinner-and-notifications ()
  "Relay activity and unread notifications share one Nostr mode-line segment."
  (nostr-test-with-db
    (let ((nostr-relay--search-request-counts (make-hash-table :test #'equal))
          (nostr-relay--search-author-request-counts (make-hash-table :test #'equal))
          (nostr-relay--profile-requests (make-hash-table :test #'equal))
          (nostr-relay--mode-line-string nil)
          (nostr-relay--mode-line-spinner-index 0)
          (nostr-relay--ingested-event-count 1)
          (nostr-relay--connect-queue nil))
      (nostr-db-store-notification "notif1" "mention" "note1" "alice" "me" 100)
      (nostr-relay--update-mode-line)
      (should (equal nostr-relay--mode-line-string " Nostr ⠋ ◉1")))))

(ert-deftest nostr-relay-stored-notification-updates-mode-line ()
  "Storing a new notification refreshes the unread mode-line indicator."
  (nostr-test-with-db
    (let ((nostr-current-pubkey "me")
          (nostr-relay--search-request-counts (make-hash-table :test #'equal))
          (nostr-relay--search-author-request-counts (make-hash-table :test #'equal))
          (nostr-relay--profile-requests (make-hash-table :test #'equal))
          (nostr-relay--mode-line-string nil)
          (nostr-relay--ingested-event-count 0)
          (nostr-relay--connect-queue nil))
      (nostr-relay--maybe-store-notification
       '((id . "note1")
         (pubkey . "alice")
         (created_at . 100)
         (kind . 1)
         (tags . (("p" "me")))
         (content . "hello me")))
      (should (equal nostr-relay--mode-line-string " Nostr ◉1")))))

(ert-deftest nostr-search-relay-fetches-authors-from-profile-hits ()
  "Matching profile search events trigger bounded author activity fetches."
  (let ((nostr-relay--search-profile-queries (make-hash-table :test #'equal))
        (nostr-relay--search-profile-author-requests (make-hash-table :test #'equal))
        (nostr-relay-search-author-urls nil)
        fetched)
    (puthash "search-test" "@alice" nostr-relay--search-profile-queries)
    (cl-letf (((symbol-function 'nostr-relay-fetch-author)
               (lambda (pubkey &optional limit)
                 (push (list pubkey limit) fetched)
                 "author-sub")))
      (nostr-relay--maybe-fetch-profile-search-author
       "search-test"
       '((kind . 0)
         (pubkey . "alice-pubkey")
         (content . "{\"name\":\"alice\",\"display_name\":\"Alice Example\"}")))
      (nostr-relay--maybe-fetch-profile-search-author
       "search-test"
       '((kind . 0)
         (pubkey . "alice-pubkey")
         (content . "{\"name\":\"alice\",\"display_name\":\"Alice Example\"}")))
      (should (equal fetched (list (list "alice-pubkey" nostr-default-feed-limit)))))))

(ert-deftest nostr-search-relay-detects-author-identifiers ()
  "Author identifiers use author subscriptions instead of NIP-50 text search."
  (let (sent)
    (cl-letf (((symbol-function 'nostr-relay-subscribe)
               (lambda (url sub-id filters)
                 (push (list url sub-id filters) sent)))
              ((symbol-function 'nostr-nip19-decode-sync)
               (lambda (value)
                 (should (equal value "npub1alice"))
                 '((ok . t) (entity . "npub") (pubkey . "alice-pubkey")))))
      (let ((nostr-relay--connections (make-hash-table :test #'equal)))
        (puthash "wss://relay-a.example" t nostr-relay--connections)
        (let ((nostr-search-query "npub1alice")
              (nostr-search-limit 25))
          (nostr-search-relay))
        (should (= (length sent) 1))
        (pcase-let ((`(,_url ,sub-id ,filters) (car sent)))
          (should (string-prefix-p "author-" sub-id))
          (should-not (alist-get "search" (car filters) nil nil #'equal))
          (should (equal (alist-get "authors" (car filters) nil nil #'equal)
                         '("alice-pubkey")))
          (should (member nostr-kind-metadata
                          (alist-get "kinds" (car filters) nil nil #'equal))))))))

(ert-deftest nostr-search-local-decodes-npub-author-query ()
  "Local search matches npub queries against cached hex pubkeys."
  (nostr-test-with-db
    (nostr-test-store-text-note "alice-note" "alice-pubkey" 100 "hello from alice")
    (cl-letf (((symbol-function 'nostr-nip19-decode-sync)
               (lambda (value)
                 (should (equal value "npub1alice"))
                 '((ok . t) (entity . "npub") (pubkey . "alice-pubkey")))))
      (let ((results (nostr-search--select-local "npub1alice" 10)))
        (should (= (length results) 1))
        (should (equal (alist-get 'id (car results)) "alice-note"))))))

(ert-deftest nostr-search-local-matches-cached-profile-fields ()
  "Local search matches cached author name, display name, and NIP-05 fields."
  (nostr-test-with-db
    (nostr-db-store-profile-event
     '((id . "profile-bob")
       (pubkey . "bob-pubkey")
       (created_at . 100)
       (kind . 0)
       (content . "{\"name\":\"bob\",\"display_name\":\"Bobby Tables\",\"nip05\":\"bob@example.test\"}")))
    (nostr-test-store-text-note "bob-note" "bob-pubkey" 120 "unrelated content")
    (dolist (query '("@bob" "Bobby" "example.test"))
      (let ((results (nostr-search--select-local query 10)))
        (should (= (length results) 1))
        (should (equal (alist-get 'id (car results)) "bob-note"))))))

(ert-deftest nostr-search-open-starts-relay-lookup ()
  "Opening a search buffer queries relays by default."
  (nostr-test-with-db
    (let (searched)
      (cl-letf (((symbol-function 'nostr-search-relay)
                 (lambda () (setq searched nostr-search-query))))
        (let ((nostr-search-auto-relay t))
          (save-window-excursion
            (nostr-search-open "emacs nostr")))
        (should (equal searched "emacs nostr"))))))

(ert-deftest nostr-relay-eose-personal-subscribes-follows-feed ()
  (nostr-test-with-db
    (let ((nostr-current-pubkey "me")
          sent)
      (nostr-db-store-event
       '((id . "contacts")
         (pubkey . "me")
         (created_at . 10)
         (kind . 3)
         (tags . (("p" "alice") ("p" "bob")))))
      (cl-letf (((symbol-function 'nostr-relay-subscribe)
                 (lambda (url sub-id filters)
                   (push (list url sub-id filters) sent))))
        (nostr-relay--handle-eose "wss://relay.example" "personal-abc"))
      (should (= (length sent) 1))
      (pcase-let ((`(,url ,sub-id ,filters) (car sent)))
        (should (equal url "wss://relay.example"))
        (should (string-prefix-p "follows-" sub-id))
        (should (equal (alist-get "authors" (car filters) nil nil #'equal)
                       '("alice" "bob")))))))

(ert-deftest nostr-relay-personal-subscription-separates-metadata-and-mentions ()
  "Personal subscriptions must not AND authors with #p."
  (let (sent)
    (cl-letf (((symbol-function 'nostr-relay--since-for-pubkey)
               (lambda (&rest _) 123))
              ((symbol-function 'nostr-relay-subscribe)
               (lambda (url sub-id filters)
                 (setq sent (list url sub-id filters)))))
      (nostr-relay-subscribe-personal "wss://relay.example" "me"))
    (pcase-let ((`(,url ,sub-id ,filters) sent))
      (should (equal url "wss://relay.example"))
      (should (string-prefix-p "personal-" sub-id))
      (should (= (length filters) 3))
      (should (equal (alist-get "authors" (nth 0 filters) nil nil #'equal)
                     '("me")))
      (should (equal (alist-get "since" (nth 0 filters) nil nil #'equal) 123))
      (should (member nostr-kind-metadata
                      (alist-get "kinds" (nth 0 filters) nil nil #'equal)))
      (should-not (member nostr-kind-zap-receipt
                          (alist-get "kinds" (nth 0 filters) nil nil #'equal)))
      (should-not (alist-get "#p" (nth 0 filters) nil nil #'equal))
      (should (equal (alist-get "#p" (nth 1 filters) nil nil #'equal)
                     '("me")))
      (should (equal (alist-get "since" (nth 1 filters) nil nil #'equal) 123))
      (should (member nostr-kind-zap-receipt
                      (alist-get "kinds" (nth 1 filters) nil nil #'equal)))
      (should-not (alist-get "authors" (nth 1 filters) nil nil #'equal)))))

(ert-deftest nostr-relay-defaults-include-broad-public-relays ()
  "Default relay set includes public bootstrap relays."
  (dolist (url '("wss://relay.primal.net"
                 "wss://relay.damus.io"))
    (should (member url nostr-relay-urls))))

(ert-deftest nostr-relay-received-notes-backfill-author-profile ()
  "Incoming notes request missing kind-0 author metadata."
  (nostr-test-with-db
    (let ((nostr-relay-verify-events nil)
          (nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-relay--profile-requests (make-hash-table :test #'equal))
          sent)
      (puthash "wss://relay.example" t nostr-relay--connections)
      (cl-letf (((symbol-function 'nostr-relay-subscribe)
                 (lambda (url sub-id filters)
                   (push (list url sub-id filters) sent))))
        (nostr-relay--handle-event
         "wss://relay.example"
         "sub"
         '((id . "note1")
           (pubkey . "alice-pubkey")
           (created_at . 100)
           (kind . 1)
           (tags . nil)
           (content . "hello")
           (sig . "sig"))))
        (should (= (length sent) 1))
        (pcase-let ((`(,url ,sub-id ,filters) (car sent)))
          (should (equal url "wss://relay.example"))
          (should (string-prefix-p "profile-" sub-id))
          (should (equal (alist-get "authors" (car filters) nil nil #'equal)
                         '("alice-pubkey")))
          (should (equal (alist-get "kinds" (car filters) nil nil #'equal)
                         (list nostr-kind-metadata)))))))

(ert-deftest nostr-relay-mode-line-shows-ingestion-and-profile-backfill ()
  "Relay ingestion updates a compact global mode-line loading spinner."
  (nostr-test-with-db
    (let ((nostr-relay-verify-events nil)
          (nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-relay--profile-requests (make-hash-table :test #'equal))
          (nostr-relay--profile-request-counts (make-hash-table :test #'equal))
          (nostr-relay--profile-request-subscriptions (make-hash-table :test #'equal))
          (nostr-relay--ingested-event-count 0)
          (nostr-relay--activity-timer nil)
          (nostr-relay--mode-line-string nil)
          (global-mode-string nil))
      (unwind-protect
          (progn
            (puthash "wss://relay.example" t nostr-relay--connections)
            (cl-letf (((symbol-function 'nostr-relay-subscribe)
                       (lambda (_url _sub-id _filters) t)))
              (nostr-relay--handle-event
               "wss://relay.example"
               "sub"
               '((id . "note1")
                 (pubkey . "alice-pubkey")
                 (created_at . 100)
                 (kind . 1)
                 (tags . nil)
                 (content . "hello")
                 (sig . "sig")))
              (should (string-match-p " Nostr [⠋⠙⠹⢸⣰⣠⣄⣆⡇⠏]"
                                      nostr-relay--mode-line-string))
              (nostr-relay--handle-event
               "wss://relay.example"
               "profile-sub"
               '((id . "profile1")
                 (pubkey . "alice-pubkey")
                 (created_at . 101)
                 (kind . 0)
                 (tags . nil)
                 (content . "{\"name\":\"Alice\",\"picture\":\"https://example.test/a.png\"}")
                 (sig . "sig")))
              (should (string-match-p " Nostr [⠋⠙⠹⢸⣰⣠⣄⣆⡇⠏]"
                                      nostr-relay--mode-line-string))
              (nostr-relay--clear-recent-activity)
              (should-not nostr-relay--mode-line-string)))
        (when (timerp nostr-relay--activity-timer)
          (cancel-timer nostr-relay--activity-timer))))))

(ert-deftest nostr-relay-profile-request-eose-clears-pending-backfill ()
  "Profile metadata requests stop blocking retries after relay EOSE."
  (nostr-test-with-db
    (let ((nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-relay--profile-requests (make-hash-table :test #'equal))
          (nostr-relay--profile-request-counts (make-hash-table :test #'equal))
          (nostr-relay--profile-request-subscriptions (make-hash-table :test #'equal))
          (nostr-relay--ingested-event-count 0)
          (nostr-relay--mode-line-string nil)
          (global-mode-string nil))
      (puthash "wss://relay.example" t nostr-relay--connections)
      (cl-letf (((symbol-function 'nostr-relay-subscribe)
                 (lambda (_url _sub-id _filters) t)))
        (should (= (nostr-relay-fetch-profile "alice-pubkey") 1)))
      (let ((sub-id (nostr-relay--profile-sub-id "alice-pubkey")))
        (should (gethash "alice-pubkey" nostr-relay--profile-requests))
        (nostr-relay-handle-frame "wss://relay.example"
                                  (json-encode `["EOSE" ,sub-id]))
        (should-not (gethash "alice-pubkey" nostr-relay--profile-requests))
        (should-not nostr-relay--mode-line-string)))))

(ert-deftest nostr-relay-feed-filter-includes-since-overlap ()
  (nostr-test-with-db
    (let ((nostr-relay-since-overlap-seconds 5))
      (nostr-db-store-event
       '((id . "alice-note")
         (pubkey . "alice")
         (created_at . 100)
         (kind . 1)
         (tags . nil)
         (content . "hello")
         (sig . "sig")
         (relay . "relay")
         (root-id . nil)
         (reply-id . nil)
         (quote-id . nil)))
      (let ((filter (nostr-relay--feed-filter '("alice"))))
        (should (equal (alist-get "since" filter nil nil #'equal) 95))
        (should (equal (alist-get "authors" filter nil nil #'equal) '("alice")))
        (should (member nostr-kind-text-note
                        (alist-get "kinds" filter nil nil #'equal)))
        (should (member nostr-kind-reaction
                        (alist-get "kinds" filter nil nil #'equal)))
        (should-not (member nostr-kind-zap-receipt
                            (alist-get "kinds" filter nil nil #'equal)))))))

(ert-deftest nostr-relay-feed-since-uses-oldest-author-cache-point ()
  "One active followed account must not hide older notes by another."
  (nostr-test-with-db
    (let ((nostr-relay-since-overlap-seconds 5))
      (nostr-test-store-text-note "alice-old" "alice" 100 "old")
      (nostr-test-store-text-note "bob-new" "bob" 1000 "new")
      (let ((filter (nostr-relay--feed-filter '("alice" "bob"))))
        (should (equal (alist-get "since" filter nil nil #'equal) 95))))))

(ert-deftest nostr-relay-fetch-event-metadata-filters-and-dedupes ()
  "Visible note metadata fetches batch #e filters and suppress repeats."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--event-metadata-requests (make-hash-table :test #'equal))
        (nostr-relay-event-metadata-request-ttl 60)
        sent)
    (puthash "wss://relay-a.example" t nostr-relay--connections)
    (puthash "wss://relay-b.example" t nostr-relay--connections)
    (cl-letf (((symbol-function 'nostr-relay-subscribe)
               (lambda (url sub-id filters)
                 (push (list url sub-id filters) sent))))
      (should (= (nostr-relay-fetch-event-metadata '("note1" "note2" "note1" nil)) 8))
      (should (= (length sent) 8))
      (let (kinds)
        (dolist (request sent)
          (pcase-let ((`(,_url ,sub-id ,filters) request))
            (should (string-prefix-p "event-meta-" sub-id))
            (should (equal (alist-get "#e" (car filters) nil nil #'equal)
                           '("note1" "note2")))
            (let ((request-kinds (alist-get "kinds" (car filters) nil nil #'equal)))
              (should (= (length request-kinds) 1))
              (push (car request-kinds) kinds))))
        (dolist (kind (list nostr-kind-text-note
                            nostr-kind-repost
                            nostr-kind-reaction
                            nostr-kind-zap-receipt))
          (should (= 2 (cl-count kind kinds)))))
      (should (= (nostr-relay-fetch-event-metadata '("note1" "note2")) 0))
      (should (= (length sent) 8)))))

(ert-deftest nostr-relay-visible-reactions-ignore-metadata-ttl ()
  "Visible reaction subscriptions stay independent from metadata backfill TTL."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--event-metadata-requests (make-hash-table :test #'equal))
        (nostr-relay--visible-reaction-sub-id nil)
        (nostr-relay--visible-reaction-event-ids nil)
        (nostr-relay--visible-reaction-relays (make-hash-table :test #'equal))
        (nostr-relay-visible-reaction-window-seconds nil)
        (now 1000.0)
        sent)
    (puthash "note1" now nostr-relay--event-metadata-requests)
    (puthash "wss://relay-a.example" t nostr-relay--connections)
    (puthash "wss://relay-b.example" t nostr-relay--connections)
    (cl-letf (((symbol-function 'float-time) (lambda () now))
              ((symbol-function 'nostr-relay-subscribe)
               (lambda (url sub-id filters)
                 (puthash sub-id t nostr-relay--subscriptions)
                 (push (list url sub-id filters) sent))))
      (should (= (nostr-relay-fetch-event-metadata '("note1")) 0))
      (should (= (nostr-relay-subscribe-visible-reactions '("note1")) 2))
      (should (= (length sent) 2))
      (pcase-let ((`(,_url ,sub-id ,filters) (car sent)))
        (should (string-prefix-p "visible-reactions-" sub-id))
        (should (equal (alist-get "kinds" (car filters) nil nil #'equal)
                       (list nostr-kind-reaction)))
        (should (equal (alist-get "#e" (car filters) nil nil #'equal)
                       '("note1")))
        (should-not (alist-get "authors" (car filters) nil nil #'equal))
        (should-not (alist-get "#p" (car filters) nil nil #'equal))
        (should-not (alist-get "since" (car filters) nil nil #'equal))
        (should-not (alist-get "limit" (car filters) nil nil #'equal)))
      (should (= (nostr-relay-subscribe-visible-reactions '("note1")) 0))
      (should (= (length sent) 2)))))

(ert-deftest nostr-relay-visible-reactions-catch-up-later-relays ()
  "Later-opening relays receive the current visible reaction subscription."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--visible-reaction-sub-id nil)
        (nostr-relay--visible-reaction-event-ids nil)
        (nostr-relay--visible-reaction-relays (make-hash-table :test #'equal))
        callbacks
        sent)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (url &rest args)
                 (puthash url (plist-get args :on-open) callbacks)
                 (list :ws url)))
              ((symbol-function 'run-at-time)
               (lambda (&rest _args) nil))
              ((symbol-function 'nostr-db-store-relay-status)
               (lambda (&rest _args) nil))
              ((symbol-function 'nostr-relay-subscribe)
               (lambda (url sub-id filters)
                 (push (list url sub-id filters) sent)))
              ((symbol-function 'nostr-visible-note-ids)
               (lambda () '("note1"))))
      (setq callbacks (make-hash-table :test #'equal))
      (nostr-relay--open-websocket "wss://relay-a.example" nil)
      (funcall (gethash "wss://relay-a.example" callbacks) 'ws-a)
      (nostr-relay--open-websocket "wss://relay-b.example" nil)
      (funcall (gethash "wss://relay-b.example" callbacks) 'ws-b)
      (should (= (length sent) 2))
      (should (equal (sort (mapcar #'car sent) #'string<)
                     '("wss://relay-a.example" "wss://relay-b.example")))
      (dolist (request sent)
        (pcase-let ((`(,_url ,sub-id ,filters) request))
          (should (string-prefix-p "visible-reactions-" sub-id))
          (should (equal (alist-get "kinds" (car filters) nil nil #'equal)
                         (list nostr-kind-reaction)))
          (should (equal (alist-get "#e" (car filters) nil nil #'equal)
                         '("note1"))))))))

(ert-deftest nostr-relay-visible-reactions-resubscribe-on-id-change ()
  "Changing visible note ids closes the old reaction subscription."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--visible-reaction-sub-id nil)
        (nostr-relay--visible-reaction-event-ids nil)
        (nostr-relay--visible-reaction-relays (make-hash-table :test #'equal))
        sent
        closed)
    (puthash "wss://relay-a.example" t nostr-relay--connections)
    (puthash "wss://relay-b.example" t nostr-relay--connections)
    (cl-letf (((symbol-function 'nostr-relay-subscribe)
               (lambda (url sub-id filters)
                 (push (list url sub-id filters) sent)))
              ((symbol-function 'nostr-relay-close-subscription-all)
               (lambda (sub-id)
                 (push sub-id closed))))
      (should (= (nostr-relay-subscribe-visible-reactions '("note1")) 2))
      (let ((old-sub-id nostr-relay--visible-reaction-sub-id))
        (should (= (nostr-relay-subscribe-visible-reactions '("note2")) 2))
        (should (equal closed (list old-sub-id))))
      (should (= (length sent) 4))
      (pcase-let ((`(,_url ,_sub-id ,filters) (car sent)))
        (should (equal (alist-get "#e" (car filters) nil nil #'equal)
                       '("note2")))))))

(ert-deftest nostr-relay-visible-reactions-honor-configured-window ()
  "Configured visible-reaction windows add a since cutoff."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--visible-reaction-sub-id nil)
        (nostr-relay--visible-reaction-event-ids nil)
        (nostr-relay--visible-reaction-relays (make-hash-table :test #'equal))
        (nostr-relay-visible-reaction-window-seconds 300)
        (now 1000.0)
        sent)
    (puthash "wss://relay.example" t nostr-relay--connections)
    (cl-letf (((symbol-function 'float-time) (lambda () now))
              ((symbol-function 'nostr-relay-subscribe)
               (lambda (_url _sub-id filters)
                 (push filters sent))))
      (should (= (nostr-relay-subscribe-visible-reactions '("note1")) 1))
      (should (= (alist-get "since" (caar sent) nil nil #'equal) 700)))))

(ert-deftest nostr-relay-fetch-events-by-id-requests-missing-context ()
  "Thread backfill requests missing root/parent events directly by id."
  (nostr-test-with-db
    (nostr-test-store-text-note "cached-note" "alice" 100 "cached")
    (let ((nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-relay--event-id-requests (make-hash-table :test #'equal))
          (nostr-relay-event-metadata-request-ttl 60)
          sent)
      (puthash "wss://relay-a.example" t nostr-relay--connections)
      (puthash "wss://relay-b.example" t nostr-relay--connections)
      (cl-letf (((symbol-function 'nostr-relay-subscribe)
                 (lambda (url sub-id filters)
                   (push (list url sub-id filters) sent))))
        (should (= (nostr-relay-fetch-events-by-id
                    '("cached-note" "missing-root" "missing-parent" "missing-root"))
                   2))
        (should (= (length sent) 2))
        (pcase-let ((`(,_url ,sub-id ,filters) (car sent)))
          (should (string-prefix-p "event-ids-" sub-id))
          (should (equal (alist-get "ids" (car filters) nil nil #'equal)
                         '("missing-root" "missing-parent"))))
        (should (= (nostr-relay-fetch-events-by-id '("missing-root")) 0))))))

(ert-deftest nostr-relay-repost-ingestion-fetches-missing-target ()
  "Kind 6 reposts request their original event when it is not cached."
  (nostr-test-with-db
    (let (requested)
      (cl-letf (((symbol-function 'nostr-relay-fetch-events-by-id)
                 (lambda (event-ids &optional _limit)
                   (setq requested event-ids)
                   1))
                ((symbol-function 'nostr-relay-fetch-profile)
                 (lambda (&rest _args) 0)))
        (nostr-relay--store-verified-event
         "wss://relay.example"
         '((id . "repost-event")
           (pubkey . "alice")
           (created_at . 120)
           (kind . 6)
           (tags . (("e" "missing-original")))
           (content . "")
           (sig . "sig"))))
        (should (equal requested '("missing-original"))))))

(ert-deftest nostr-dispatch-opens-npub-note-and-search ()
  (let (opened)
    (cl-letf (((symbol-function 'nostr-profile-open)
               (lambda (pubkey) (push (list 'profile pubkey) opened)))
              ((symbol-function 'nostr-thread-open)
               (lambda (event) (push (list 'thread (alist-get 'id event)) opened)))
              ((symbol-function 'nostr-search-open)
               (lambda (query) (push (list 'search query) opened)))
              ((symbol-function 'nostr-nip19-decode-sync)
               (lambda (value)
                 (pcase value
                   ("npub1test" '((ok . t) (entity . "npub") (pubkey . "pubkey1")))
                   ("note1test" '((ok . t) (entity . "note") (event_id . "event1")))
                   ("nevent1test" '((ok . t) (entity . "nevent") (event_id . "event2")))
                   (_ (error "unexpected value")))))
              ((symbol-function 'nostr-dispatch--event-by-id)
               (lambda (event-id)
                 (when (equal event-id "event1")
                   `((id . ,event-id))))))
      (nostr-open-identifier "npub1test")
      (nostr-open-identifier "note1test")
      (nostr-open-identifier "nevent1test")
      (nostr-open-identifier "anything else"))
    (should (equal (nreverse opened)
                   '((profile "pubkey1")
                     (thread "event1")
                     (search "event2")
                     (search "anything else"))))))

(ert-deftest nostr-timeline-open-author-opens-selected-pubkey ()
  (let (opened)
    (cl-letf (((symbol-function 'nostr-profile-open)
               (lambda (pubkey) (setq opened pubkey))))
      (with-temp-buffer
        (nostr-timeline-mode)
        (let ((inhibit-read-only t))
          (nostr-ui-clear)
          (nostr-ui-insert-note
           '((id . "note1")
             (pubkey . "alice")
             (created-at . 1)
             (content . "hello")
             (reactions . 0)
             (replies . 0))))
        (goto-char (point-min))
        (nostr-timeline-open-author)))
    (should (equal opened "alice"))))

(ert-deftest nostr-timeline-ret-on-profile-mention-opens-profile ()
  "RET on an npub mention button opens the linked profile."
  (let ((nostr-ui-show-avatars nil)
        (nostr-ui--npub-decode-cache (make-hash-table :test #'equal))
        opened)
    (cl-letf (((symbol-function 'nostr-nip19-decode-sync)
               (lambda (_value)
                 '((ok . t) (entity . "npub") (pubkey . "profile-pubkey"))))
              ((symbol-function 'nostr-ui--cached-profile)
               (lambda (pubkey)
                 (when (equal pubkey "profile-pubkey")
                   '("profile-pubkey" "ODELL" "ODELL" nil nil nil nil nil))))
              ((symbol-function 'nostr-profile-open)
               (lambda (pubkey) (setq opened pubkey))))
      (with-temp-buffer
        (nostr-timeline-mode)
        (let ((inhibit-read-only t))
          (nostr-ui-clear)
          (nostr-ui-insert-note
           '((id . "mention-note")
             (pubkey . "alice")
             (created-at . 1736776800)
             (content . "hello @npub1q8g803ajr0lw3xngs0k6hn2q3mejf6dtgv05d06h6krqgv9uh97q5382kp")
             (replies . 0)
             (reactions . 0)
             (reposts . 0))))
        (goto-char (point-min))
        (search-forward "@ODELL")
        (nostr-timeline-open-thread)))
    (should (equal opened "profile-pubkey"))))

(ert-deftest nostr-timeline-mode-has-primary-navigation-keys ()
  (should (eq (lookup-key nostr-timeline-mode-map (kbd "/")) #'nostr-search-open))
  (should (eq (lookup-key nostr-timeline-mode-map (kbd "o")) #'nostr-open-identifier))
  (should (eq (lookup-key nostr-timeline-mode-map (kbd "N")) #'nostr-notifications-open))
  (should (eq (lookup-key nostr-timeline-mode-map (kbd "L")) #'nostr-relays-open))
  (should (eq (lookup-key nostr-timeline-mode-map (kbd "S")) #'nostr-setup-status))
  (should (eq (lookup-key nostr-timeline-mode-map (kbd "f")) #'nostr-timeline-feed))
  (should (eq (lookup-key nostr-timeline-mode-map (kbd "h")) #'nostr-timeline-feed))
  (should (eq (lookup-key nostr-timeline-mode-map (kbd "C")) #'nostr-timeline-conversations))
  (should (eq (lookup-key nostr-timeline-mode-map (kbd "e")) #'nostr-timeline-conversations))
  (should (eq (lookup-key nostr-timeline-mode-map (kbd "d")) #'nostr-timeline-discover))
  (should (eq (lookup-key nostr-timeline-mode-map (kbd ">")) #'nostr-discover-load-more))
  (should-not (lookup-key nostr-timeline-mode-map (kbd "G")))
  (should (eq (lookup-key nostr-timeline-mode-map (kbd "P")) #'nostr-timeline-my-posts))
  (should-not (lookup-key nostr-timeline-mode-map (kbd "M"))))

(ert-deftest nostr-notifications-mode-has-primary-navigation-keys ()
  (should (eq (lookup-key nostr-notifications-mode-map (kbd "N")) #'nostr-notifications-open))
  (should (eq (lookup-key nostr-notifications-mode-map (kbd "f")) #'nostr-timeline-feed))
  (should (eq (lookup-key nostr-notifications-mode-map (kbd "h")) #'nostr-timeline-feed))
  (should (eq (lookup-key nostr-notifications-mode-map (kbd "C")) #'nostr-timeline-conversations))
  (should (eq (lookup-key nostr-notifications-mode-map (kbd "e")) #'nostr-timeline-conversations))
  (should (eq (lookup-key nostr-notifications-mode-map (kbd "d")) #'nostr-timeline-discover))
  (should-not (lookup-key nostr-notifications-mode-map (kbd "G")))
  (should (eq (lookup-key nostr-notifications-mode-map (kbd "P")) #'nostr-timeline-my-posts)))

(ert-deftest nostr-note-buffers-bind-view-reactions ()
  "Timeline and thread buffers expose reaction detail commands."
  (should (eq (lookup-key nostr-timeline-mode-map (kbd "v"))
              #'nostr-timeline-view-reactions))
  (should (eq (lookup-key nostr-thread-mode-map (kbd "v"))
              #'nostr-thread-view-reactions))
  (should (commandp #'nostr-timeline-view-reactions))
  (should (commandp #'nostr-thread-view-reactions)))

(ert-deftest nostr-ui-pages-bind-question-mark-to-transients ()
  "Every Nostr UI page exposes its action menu on `?'."
  (dolist (entry `((,nostr-timeline-mode-map . nostr-timeline-actions)
                   (,nostr-thread-mode-map . nostr-thread-actions)
                   (,nostr-profile-mode-map . nostr-profile-actions)
                   (,nostr-search-mode-map . nostr-search-actions)
                   (,nostr-notifications-mode-map . nostr-notifications-actions)
                   (,nostr-relays-mode-map . nostr-relays-actions)
                   (,nostr-compose-mode-map . nostr-compose-actions)
                   (,nostr-setup-mode-map . nostr-setup-actions)))
    (let ((map (car entry))
          (command (cdr entry)))
      (should (fboundp command))
      (should (commandp command))
      (should (eq (lookup-key map (kbd "?")) command)))))

(provide 'nostr-test)
;;; nostr-test.el ends here
