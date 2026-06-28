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
  "Run BODY with an isolated in-memory Nostr database.
Also binds a fresh `nostr-relay--verified-event-ids' so that the session
seen-set does not leak verified ids between tests, and a fresh
`nostr-relay--syncing-subs'/`nostr-relay--sync-timeout-timer' so initial-sync
state set by one test does not bleed into another (e.g. backfill gating)."
  (declare (indent 0))
  `(let ((nostr-db--connection (emacsql-sqlite-open nil))
         (nostr-relay--verified-event-ids (make-hash-table :test #'equal))
         (nostr-relay--syncing-subs (make-hash-table :test #'equal))
         (nostr-relay--sync-timeout-timer nil))
     (unwind-protect
         (progn
           (nostr-db-init)
           ,@body)
       (when (timerp nostr-relay--sync-timeout-timer)
         (cancel-timer nostr-relay--sync-timeout-timer))
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

(ert-deftest nostr-event-public-identifiers-finds-supported-values ()
  "Public NIP-19 extraction covers NIP-21 URI wrapped identifiers."
  (should (equal (nostr-event-public-identifiers
                  (concat "see nostr:nprofile1qqqq and @npub1aaaa "
                          "note1qqqq nevent1cccc naddr1dddd "
                          "nsec1eeee nrelay1ffff"))
                 '("nostr:nprofile1qqqq"
                   "@npub1aaaa"
                   "note1qqqq"
                   "nevent1cccc"
                   "naddr1dddd"))))

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

;; Websocket connection error handling now lives in the Rust daemon (it owns the
;; socket and records relay_status); the Elisp `--open-websocket' test was
;; removed with that code.

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

(ert-deftest nostr-db-profile-metadata-honors-created-at-ordering ()
  "Replaceable kind-0 profiles keep only the newest event (NIP-01)."
  (nostr-test-with-db
    (nostr-db-store-profile-event
     '((pubkey . "alice")
       (created_at . 200)
       (kind . 0)
       (content . "{\"name\":\"new\"}")))
    ;; Older event must not clobber the newer profile.
    (nostr-db-store-profile-event
     '((pubkey . "alice")
       (created_at . 100)
       (kind . 0)
       (content . "{\"name\":\"old\"}")))
    (should (equal (nth 1 (nostr-db-select-profile "alice")) "new"))
    ;; Newer event does replace it.
    (nostr-db-store-profile-event
     '((pubkey . "alice")
       (created_at . 300)
       (kind . 0)
       (content . "{\"name\":\"newest\"}")))
    (should (equal (nth 1 (nostr-db-select-profile "alice")) "newest"))))

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

(ert-deftest nostr-db-select-thread-excludes-reactions ()
  "Reactions targeting a note must not surface as thread replies."
  (nostr-test-with-db
    (nostr-test-store-text-note "root" "alice" 100 "root note")
    (nostr-test-store-text-note "reply" "bob" 101 "a reply" "root" "root")
    ;; Kind-7 reaction whose e-tag makes its root_id point at the note.
    (nostr-db-store-event
     (nostr-event-normalize
      `((id . "react")
        (pubkey . "carol")
        (created_at . 102)
        (kind . 7)
        (tags . (("e" "root")))
        (content . "+")
        (sig . ""))
      "relay"))
    (let ((thread (nostr-db-select-thread "root")))
      (should (equal (mapcar (lambda (event) (alist-get 'id event)) thread)
                     '("root" "reply"))))
    ;; The reply badge counts only the note, not the reaction.
    (should (= (alist-get 'replies (nostr-db-event-counts "root")) 1))))

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

(ert-deftest nostr-thread-depth-terminates-on-reply-id-cycles ()
  "`nostr-thread--depth' returns a finite depth despite cyclic reply ids."
  ;; Self-referential reply-id: must be caught immediately, depth 0.
  (let ((events '(((id . "self") (reply-id . "self")))))
    (should (integerp (nostr-thread--depth (car events) events)))
    (should (= 0 (nostr-thread--depth (car events) events))))
  ;; Two-event mutual cycle: walk stops once a parent id repeats.
  (let* ((a '((id . "a") (reply-id . "b")))
         (b '((id . "b") (reply-id . "a")))
         (events (list a b))
         (depth (nostr-thread--depth a events)))
    (should (integerp depth))
    (should (<= depth nostr-thread-max-depth)))
  ;; Normal linear chain still yields the expected depth.
  (let* ((events '(((id . "root"))
                   ((id . "r1") (reply-id . "root"))
                   ((id . "r2") (reply-id . "r1")))))
    (should (= 0 (nostr-thread--depth (nth 0 events) events)))
    (should (= 1 (nostr-thread--depth (nth 1 events) events)))
    (should (= 2 (nostr-thread--depth (nth 2 events) events)))))

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
      (should (face-attribute 'nostr-ui-selected-section :extend nil 'default))
      (should (or (face-attribute 'nostr-ui-selected-section :background nil 'default)
                  (face-attribute 'nostr-ui-selected-section :inverse-video nil 'default)))
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
          (state (list :events nil :order nil))
          (nostr-discover--active-websocket 'discover-ws))
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
       state nil 'discover-ws)
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
       state nil 'discover-ws)
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
       state nil 'discover-ws)
      (nostr-discover--handle-frame
       "wss://cache.example"
       (json-encode ["EOSE" "sub"])
       state nil 'discover-ws)
      (let ((events (nostr-db-select-discover-feed "primal" "global" "trending" 10)))
        (should (= (length events) 1))
	      (should (equal (alist-get 'id (car events)) "note-a"))
	      (should (= (alist-get 'reactions (car events)) 4))
	      (should (= (nostr-db-discover-next-cursor "primal" "global" "trending") 7))))))

(ert-deftest nostr-discover-ignores-stale-websocket-callbacks ()
  "A replaced Discover websocket cannot finish or clear the active request."
  (let ((nostr-discover--active-websocket 'new-ws)
        (nostr-discover--loading t)
        (nostr-discover--last-status '(:state loading))
        (finished 0))
    (let ((nostr-discover-finished-hook
           (list (lambda () (setq finished (1+ finished))))))
      (nostr-discover--handle-frame
       "wss://cache.example"
       (json-encode ["EOSE" "sub"])
       (list :events nil :order nil)
       nil
       'old-ws)
      (should (eq nostr-discover--active-websocket 'new-ws))
      (should nostr-discover--loading)
      (should (= finished 0)))))

(ert-deftest nostr-timeline-refresh-backfills-visible-profile-metadata ()
  "Rendering a feed batch-requests missing avatars and note metadata."
  (nostr-test-with-db
    (nostr-test-store-text-note "global-author" "alice-pubkey" 100 "global note")
    (let (requested-profiles requested-events)
      (cl-letf (((symbol-function 'nostr-relay-fetch-profiles-batch)
                 (lambda (pubkeys)
                   (setq requested-profiles (append requested-profiles pubkeys))
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

(ert-deftest nostr-relay-relay-list-filter-targets-kind-10002 ()
  "NIP-65 discovery queries kind-10002 for the given authors."
  (let ((filter (nostr-relay--relay-list-filter '("a" "b"))))
    (should (equal (alist-get "kinds" filter nil nil #'equal)
                   (list nostr-kind-relay-list)))
    (should (equal (alist-get "authors" filter nil nil #'equal) '("a" "b")))))

(ert-deftest nostr-relays-relay-list-tags-advertise-configured-relays ()
  "Publishing a relay list advertises each configured relay as an r-tag."
  (let ((nostr-relay-urls '("wss://a.example" "" "wss://b.example")))
    (should (equal (nostr-relays--relay-list-tags)
                   '(("r" "wss://a.example") ("r" "wss://b.example"))))))

(ert-deftest nostr-db-popular-write-relays-ranks-by-frequency ()
  "Discovered write relays are ranked by how many accounts advertise them."
  (nostr-test-with-db
    ;; two accounts write to relay-a, one to relay-b, relay-c is read-only.
    (nostr-db-store-relay-list-event
     '((pubkey . "p1") (created_at . 1) (kind . 10002)
       (tags . (("r" "wss://a") ("r" "wss://b")))))
    (nostr-db-store-relay-list-event
     '((pubkey . "p2") (created_at . 1) (kind . 10002)
       (tags . (("r" "wss://a") ("r" "wss://c" "read")))))
    (let ((popular (nostr-db-select-popular-write-relays 8)))
      ;; relay-a (2 writers) first; relay-c is read-only so excluded.
      (should (equal (car popular) "wss://a"))
      (should (member "wss://b" popular))
      (should-not (member "wss://c" popular)))))

(ert-deftest nostr-relay-connect-discovered-opens-popular-write-relays ()
  "Discovery connects the most-advertised write relays not already open."
  (nostr-test-with-db
    (let ((nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-relay-max-discovered-relays 8)
          opened)
      (puthash "wss://already" t nostr-relay--connections)
      (nostr-db-store-relay-list-event
       '((pubkey . "p1") (created_at . 1) (kind . 10002)
         (tags . (("r" "wss://new") ("r" "wss://already")))))
      (cl-letf (((symbol-function 'nostr-relay-open)
                 (lambda (url &rest _) (push url opened))))
        (nostr-relay-connect-discovered))
      ;; only the not-yet-connected discovered relay is opened.
      (should (equal opened '("wss://new"))))))

(ert-deftest nostr-relay-my-posts-filter-has-no-since-and-pages-with-until ()
  "My Posts backfills full authored history: no since, optional until paging."
  (let ((filter (nostr-relay--my-posts-filter "me")))
    (should-not (assoc "since" filter))
    (should (equal (alist-get "authors" filter nil nil #'equal) '("me")))
    (should (member nostr-kind-text-note (alist-get "kinds" filter nil nil #'equal)))
    (should (member nostr-kind-repost (alist-get "kinds" filter nil nil #'equal))))
  ;; With until, restrict to older history for pagination.
  (let ((filter (nostr-relay--my-posts-filter "me" 1234)))
    (should (equal (alist-get "until" filter nil nil #'equal) 1234))))

(ert-deftest nostr-timeline-load-older-grows-window-and-pages-history ()
  "Loading older grows the render window and fetches events before the cursor."
  (nostr-test-with-db
    (let ((nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-timeline-limit 100)
          fetched)
      (puthash "wss://relay.example" t nostr-relay--connections)
      (cl-letf (((symbol-function 'nostr-db-select-follows)
                 (lambda (&rest _) '("alice")))
                ((symbol-function 'nostr-relay--fetch)
                 (lambda (_sub-id filters) (setq fetched (car filters)) 1))
                ((symbol-function 'nostr-relay-fetch-profiles-batch) #'ignore)
                ((symbol-function 'nostr-relay-fetch-event-metadata) #'ignore)
                ((symbol-function 'nostr-relay-subscribe-visible-reactions) #'ignore)
                ((symbol-function 'nostr-relay-subscribe-global) #'ignore)
                ((symbol-function 'nostr-relay-close-global) #'ignore))
        (with-temp-buffer
          (nostr-timeline-mode)
          (setq-local nostr-timeline-current-pubkey "me")
          (setq-local nostr-timeline-feed-kind 'feed)
          (setq-local nostr-timeline--oldest-rendered 5000)
          (nostr-timeline-load-older)
          ;; render window grew by one page
          (should (= (nostr-timeline--limit) 200))
          ;; fetched older-than-cursor history (until = oldest - 1, no since)
          (should (equal (alist-get "until" fetched nil nil #'equal) 4999))
          (should-not (assoc "since" fetched))
          ;; cursor recorded; a repeat with the same cursor is a no-op
          (should (equal nostr-timeline--last-page-until 5000))
          (setq fetched nil)
          (nostr-timeline-load-older)
          (should-not fetched))))))

(ert-deftest nostr-ui-image-cache-evicts-when-over-cap ()
  "The image cache evicts to half capacity instead of growing without bound."
  (let ((nostr-ui--image-cache (make-hash-table :test #'equal))
        (nostr-ui-image-cache-max 4))
    (dotimes (i 4) (puthash i t nostr-ui--image-cache))
    (nostr-ui--evict-image-cache)
    ;; at cap -> dropped to half, bounded
    (should (<= (hash-table-count nostr-ui--image-cache) 2))))

(ert-deftest nostr-timeline-load-older-stops-at-max-notes ()
  "History paging stops once the render cap is reached, bounding memory."
  (nostr-test-with-db
    (let ((nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-timeline-limit 100)
          (nostr-timeline-max-notes 200)
          fetched)
      (puthash "wss://relay.example" t nostr-relay--connections)
      (cl-letf (((symbol-function 'nostr-db-select-follows) (lambda (&rest _) '("a")))
                ((symbol-function 'nostr-relay--fetch)
                 (lambda (&rest _) (setq fetched t) 1))
                ((symbol-function 'nostr-relay-fetch-profiles-batch) #'ignore)
                ((symbol-function 'nostr-relay-fetch-event-metadata) #'ignore)
                ((symbol-function 'nostr-relay-subscribe-visible-reactions) #'ignore)
                ((symbol-function 'nostr-relay-subscribe-global) #'ignore)
                ((symbol-function 'nostr-relay-close-global) #'ignore))
        (with-temp-buffer
          (nostr-timeline-mode)
          (setq-local nostr-timeline-current-pubkey "me")
          (setq-local nostr-timeline-feed-kind 'feed)
          (setq-local nostr-timeline--render-limit 200) ; already at the cap
          (setq-local nostr-timeline--oldest-rendered 5000)
          (nostr-timeline-load-older)
          (should-not fetched)
          (should (= (nostr-timeline--limit) 200)))))))

(ert-deftest nostr-timeline-my-posts-backfills-once-on-entry ()
  "Entering My Posts backfills own history; refreshing in place does not respawn."
  (nostr-test-with-db
    (let ((nostr-relay--connections (make-hash-table :test #'equal))
          (calls 0))
      (puthash "wss://relay.example" t nostr-relay--connections)
      (cl-letf (((symbol-function 'nostr-relay-fetch-my-posts)
                 (lambda (&rest _) (setq calls (1+ calls)) 1))
                ;; isolate the test from avatar/metadata relay calls
                ((symbol-function 'nostr-relay-fetch-profiles-batch) #'ignore)
                ((symbol-function 'nostr-relay-fetch-event-metadata) #'ignore)
                ((symbol-function 'nostr-relay-subscribe-visible-reactions) #'ignore))
        (with-temp-buffer
          (nostr-timeline-mode)
          (setq-local nostr-timeline-current-pubkey "me")
          (nostr-timeline-my-posts)        ; enter -> 1 backfill
          (should (= calls 1))
          (nostr-timeline-refresh)          ; in-place refresh -> no respawn
          (should (= calls 1))
          (nostr-timeline-feed)             ; leave
          (nostr-timeline-my-posts)         ; re-enter -> backfill again
          (should (= calls 2)))))))

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

;; Signature verification, dedup, and notification derivation moved to the Rust
;; relay daemon (covered by `store::tests` in backend/src/store.rs); the former
;; Elisp verify/ingest tests were removed with that pipeline.

(ert-deftest nostr-relay-tracks-publish-receipts ()
  "Publishing records per-relay pending receipts and hands the event to the daemon.
The daemon performs the actual send and updates the receipts to sent/failed as
relays answer; Emacs only seeds the pending rows and dispatches."
  (nostr-test-with-db
    (let ((nostr-current-pubkey "me")
          (nostr-relay--connections (make-hash-table :test #'equal))
          published)
      (cl-letf (((symbol-function 'nostr-daemon-publish)
                 (lambda (event &optional relays)
                   (push (list event relays) published))))
        (puthash "wss://relay-a.example" t nostr-relay--connections)
        (puthash "wss://relay-b.example" t nostr-relay--connections)
        (should (= (nostr-relay-send-client-message
                    "[\"EVENT\",{\"id\":\"event-pub\",\"content\":\"hello\"}]")
                   2)))
      ;; The signed event is dispatched once, to both connected relays.
      (should (= (length published) 1))
      (pcase-let ((`(,event ,relays) (car published)))
        (should (equal (alist-get 'id event) "event-pub"))
        (should (equal (sort (copy-sequence relays) #'string<)
                       '("wss://relay-a.example" "wss://relay-b.example"))))
      ;; Each target relay gets a pending receipt up front.
      (should (equal (mapcar (lambda (receipt)
                               (list (alist-get 'url receipt)
                                     (alist-get 'state receipt)))
                             (nostr-db-select-publish-receipts "event-pub"))
                     '(("wss://relay-a.example" "pending")
                       ("wss://relay-b.example" "pending")))))))

(ert-deftest nostr-search-relay-subscribes-connected-relays ()
  (let (sent)
    (cl-letf (((symbol-function 'nostr-relay-subscribe)
               (lambda (url sub-id filters &optional _close-on-eose)
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

;; Primal-cache hint verification and the duplicate-event dedup/provenance fast
;; path were part of the Elisp ingest pipeline now owned by the Rust daemon, so
;; their tests were removed along with it.

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

;; Notification derivation (and its unread mode-line bump) is now done by the
;; Rust daemon; the mode-line still refreshes per stored event via
;; `nostr-relay--note-ingested-event'. The derivation test moved to Rust.

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
               (lambda (url sub-id filters &optional _close-on-eose)
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
                 (lambda (url sub-id filters &optional _close-on-eose)
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
               (lambda (url sub-id filters &optional _close-on-eose)
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

(ert-deftest nostr-relay-stored-event-does-not-spam-profile-fetches ()
  "A stored event must not fetch its author profile (that flooded relays).
Profile backfill is bounded and deferred to the visible-notes render path."
  (nostr-test-with-db
    (let ((nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-relay--profile-requests (make-hash-table :test #'equal))
          sent)
      (puthash "wss://relay.example" t nostr-relay--connections)
      (cl-letf (((symbol-function 'nostr-relay-subscribe)
                 (lambda (_url _sub-id _filters &optional _close-on-eose)
                   (setq sent (1+ (or sent 0))))))
        (nostr-relay--on-stored-event
         "sub"
         '((id . "note1") (pubkey . "alice-pubkey") (created_at . 100)
           (kind . 1) (tags . nil) (content . "hello") (sig . "sig"))))
      (should (null sent)))))

(ert-deftest nostr-timeline-backfill-batches-visible-author-profiles ()
  "The render path backfills missing kind-0 metadata for visible authors.
All authors are fetched in one auto-closing batched subscription (eager avatars),
not one subscription per author."
  (nostr-test-with-db
    (let ((nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-relay--profile-batch-requests (make-hash-table :test #'equal))
          sent)
      (puthash "wss://relay.example" t nostr-relay--connections)
      (cl-letf (((symbol-function 'nostr-relay-subscribe)
                 (lambda (url sub-id filters &optional close-on-eose)
                   (push (list url sub-id filters close-on-eose) sent))))
        (nostr-timeline--backfill-profiles
         '(((id . "n1") (pubkey . "alice") (kind . 1))
           ((id . "n2") (pubkey . "bob") (kind . 1))
           ((id . "n3") (pubkey . "alice") (kind . 1)))))
      ;; One subscription (per relay) for both distinct authors.
      (should (= (length sent) 1))
      (pcase-let ((`(,url ,sub-id ,filters ,_close) (car sent)))
        (should (equal url "wss://relay.example"))
        (should (string-prefix-p "profiles-" sub-id))
        (should (equal (sort (copy-sequence
                              (alist-get "authors" (car filters) nil nil #'equal))
                             #'string<)
                       '("alice" "bob")))
        (should (equal (alist-get "kinds" (car filters) nil nil #'equal)
                       (list nostr-kind-metadata)))
        ;; Replaceable kind-0 must not carry a since bound.
        (should-not (assoc "since" (car filters)))))))

(ert-deftest nostr-relay-fetch-profiles-batch-skips-cached-and-dedupes ()
  "Batched profile fetch skips already-cached authors and TTL-deduped repeats."
  (nostr-test-with-db
    (let ((nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-relay--profile-batch-requests (make-hash-table :test #'equal))
          authors)
      (puthash "wss://relay.example" t nostr-relay--connections)
      (nostr-db-store-profile-event
       '((pubkey . "cached") (created_at . 1) (kind . 0)
         (content . "{\"name\":\"c\"}")))
      (cl-letf (((symbol-function 'nostr-relay-subscribe)
                 (lambda (_url _sub-id filters &optional _close)
                   (setq authors (alist-get "authors" (car filters) nil nil #'equal)))))
        (nostr-relay-fetch-profiles-batch '("cached" "fresh" "fresh"))
        ;; "cached" is skipped, "fresh" deduped to one entry.
        (should (equal authors '("fresh")))
        ;; A second call within the TTL issues nothing for the same author.
        (setq authors :unset)
        (nostr-relay-fetch-profiles-batch '("fresh"))
        (should (eq authors :unset))))))

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
                       (lambda (_url _sub-id _filters &optional _close-on-eose) t)))
              (nostr-relay--on-stored-event
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
              (nostr-relay--on-stored-event
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
                 (lambda (_url _sub-id _filters &optional _close-on-eose) t)))
        (should (= (nostr-relay-fetch-profile "alice-pubkey") 1)))
      (let ((sub-id (nostr-relay--profile-sub-id "alice-pubkey")))
        (should (gethash "alice-pubkey" nostr-relay--profile-requests))
        (nostr-relay--on-daemon-notification
         `((event . "eose") (sub . ,sub-id) (relay . "wss://relay.example")))
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

(ert-deftest nostr-relay-follow-metadata-filter-has-no-since ()
  "Kind-0 metadata is replaceable; a since bound would hide month-old profiles.
The filter must omit since and size its limit to cover every followed author."
  (let ((nostr-relay-follow-metadata-limit 25)
        (pubkeys (mapcar #'number-to-string (number-sequence 1 40))))
    (let ((filter (nostr-relay--follow-metadata-filter pubkeys)))
      (should-not (assoc "since" filter))
      (should (equal (alist-get "kinds" filter nil nil #'equal)
                     (list nostr-kind-metadata)))
      ;; limit grows to cover all 40 follows, not the smaller default.
      (should (equal (alist-get "limit" filter nil nil #'equal) 40)))))

(ert-deftest nostr-relay-feed-since-backfills-when-author-uncached ()
  "A followed author with no cached events forces the full backfill window.
Otherwise a few recently-cached events (e.g. mentions) collapse the feed since to
~now and the initial backfill of the other follows never happens."
  (nostr-test-with-db
    (let ((nostr-relay-startup-window-seconds 1000))
      ;; alice has a very recent event; bob has none.
      (nostr-test-store-text-note "alice-recent" "alice" (floor (float-time)) "hi")
      (should (null (nostr-db-oldest-latest-event-time '("alice" "bob"))))
      (let* ((now (floor (float-time)))
             (since (alist-get "since" (nostr-relay--feed-filter '("alice" "bob"))
                               nil nil #'equal)))
        ;; Falls back to ~now - startup-window, not alice's recent timestamp.
        (should (<= (abs (- since (- now 1000))) 2))))))

(ert-deftest nostr-relay-fetch-event-metadata-filters-and-dedupes ()
  "Visible note metadata fetches batch #e filters and suppress repeats."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--event-metadata-requests (make-hash-table :test #'equal))
        (nostr-relay-event-metadata-request-ttl 60)
        sent)
    (puthash "wss://relay-a.example" t nostr-relay--connections)
    (puthash "wss://relay-b.example" t nostr-relay--connections)
    (cl-letf (((symbol-function 'nostr-relay-subscribe)
               (lambda (url sub-id filters &optional _close-on-eose)
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
               (lambda (url sub-id filters &optional _close-on-eose)
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
    (ignore callbacks)
    ;; The daemon owns the socket; `nostr-relay-open' issues the visible-reaction
    ;; subscription synchronously when a relay is registered.
    (cl-letf (((symbol-function 'nostr-daemon-running-p) (lambda () t))
              ((symbol-function 'nostr-daemon-add-relay) #'ignore)
              ((symbol-function 'nostr-daemon-set-pubkey) #'ignore)
              ((symbol-function 'nostr-db-store-relay-status)
               (lambda (&rest _args) nil))
              ((symbol-function 'nostr-relay-subscribe)
               (lambda (url sub-id filters &optional _close-on-eose)
                 (push (list url sub-id filters) sent)))
              ((symbol-function 'nostr-visible-note-ids)
               (lambda () '("note1"))))
      (nostr-relay-open "wss://relay-a.example" nil)
      (nostr-relay-open "wss://relay-b.example" nil)
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
               (lambda (url sub-id filters &optional _close-on-eose)
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

(ert-deftest nostr-visible-note-ids-tracks-window-range ()
  "Visible note ids come from sections overlapping window start/end."
  (nostr-test-with-db
    (let ((buffer (get-buffer-create " *nostr-visible-note-ids-test*"))
          first-end
          second-start)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (nostr-timeline-mode)
              (let ((inhibit-read-only t))
                (nostr-ui-clear)
                (nostr-ui-insert-note
                 '((id . "note-a")
                   (pubkey . "alice")
                   (created_at . 100)
                   (kind . 1)
                   (tags . nil)
                   (content . "first")))
                (setq first-end (point))
                (dotimes (_ 80)
                  (insert "\n"))
                (setq second-start (point))
                (nostr-ui-insert-note
                 '((id . "note-b")
                   (pubkey . "bob")
                   (created_at . 101)
                   (kind . 1)
                   (tags . nil)
                   (content . "second")))))
            (cl-letf (((symbol-function 'window-list)
                       (lambda (&rest _) '(fake-window)))
                      ((symbol-function 'window-buffer)
                       (lambda (_window) buffer))
                      ((symbol-function 'window-start)
                       (lambda (_window) (point-min)))
                      ((symbol-function 'window-end)
                       (lambda (&rest _) first-end)))
              (should (equal (nostr-visible-note-ids) '("note-a"))))
            (cl-letf (((symbol-function 'window-list)
                       (lambda (&rest _) '(fake-window)))
                      ((symbol-function 'window-buffer)
                       (lambda (_window) buffer))
                      ((symbol-function 'window-start)
                       (lambda (_window) second-start))
                      ((symbol-function 'window-end)
                       (lambda (&rest _) (point-max))))
              (should (equal (nostr-visible-note-ids) '("note-b")))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

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
               (lambda (_url _sub-id filters &optional _close-on-eose)
                 (push filters sent))))
	      (should (= (nostr-relay-subscribe-visible-reactions '("note1")) 1))
	      (should (= (alist-get "since" (caar sent) nil nil #'equal) 700)))))

(ert-deftest nostr-relay-sync-visible-reactions-uses-visible-note-union ()
  "UI reconciliation subscribes to the union of notes visible in Nostr windows."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--visible-reaction-sub-id nil)
        (nostr-relay--visible-reaction-event-ids nil)
        (nostr-relay--visible-reaction-relays (make-hash-table :test #'equal))
        sent)
    (puthash "wss://relay.example" t nostr-relay--connections)
    (cl-letf (((symbol-function 'nostr-visible-note-ids)
               (lambda () '("note-a" "note-b")))
              ((symbol-function 'nostr-relay-subscribe)
               (lambda (_url _sub-id filters &optional _close-on-eose)
                 (push filters sent))))
      (should (= (nostr-relay-sync-visible-reactions) 1))
      (should (equal (alist-get "#e" (caar sent) nil nil #'equal)
                     '("note-a" "note-b"))))))

(ert-deftest nostr-relay-sync-visible-reactions-closes-when-no-notes-visible ()
  "UI reconciliation closes the live reaction subscription when no notes remain."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--visible-reaction-sub-id "visible-reactions-old")
        (nostr-relay--visible-reaction-event-ids '("note-old"))
        (nostr-relay--visible-reaction-relays (make-hash-table :test #'equal))
        closed)
    (cl-letf (((symbol-function 'nostr-visible-note-ids) (lambda () nil))
              ((symbol-function 'nostr-relay-close-subscription-all)
               (lambda (sub-id) (push sub-id closed))))
      (should (= (nostr-relay-sync-visible-reactions) 0))
      (should (equal closed '("visible-reactions-old")))
      (should-not nostr-relay--visible-reaction-sub-id)
      (should-not nostr-relay--visible-reaction-event-ids))))

(ert-deftest nostr-search-refresh-empty-results-preserves-visible-reactions ()
  "An empty search refresh must not close reactions for notes visible elsewhere."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--visible-reaction-sub-id nil)
        (nostr-relay--visible-reaction-event-ids nil)
        (nostr-relay--visible-reaction-relays (make-hash-table :test #'equal))
        sent
        closed)
    (puthash "wss://relay.example" t nostr-relay--connections)
    (with-temp-buffer
      (nostr-search-mode)
      (setq-local nostr-search-query "missing")
      (cl-letf (((symbol-function 'nostr-search--select-profiles) (lambda (&rest _) nil))
                ((symbol-function 'nostr-search--select-local) (lambda (&rest _) nil))
                ((symbol-function 'nostr-relay-fetch-profiles-batch) (lambda (&rest _) 0))
                ((symbol-function 'nostr-relay-fetch-event-metadata) (lambda (&rest _) 0))
                ((symbol-function 'nostr-visible-note-ids) (lambda () '("still-visible")))
                ((symbol-function 'nostr-relay-subscribe)
                 (lambda (_url _sub-id filters &optional _close-on-eose)
                   (push filters sent)))
                ((symbol-function 'nostr-relay-close-subscription-all)
                 (lambda (sub-id) (push sub-id closed))))
        (nostr-search-refresh)
        (should-not closed)
        (should (equal (alist-get "#e" (caar sent) nil nil #'equal)
                       '("still-visible")))))))

(ert-deftest nostr-search-refresh-hidden-buffer-marks-dirty ()
  "Relay-search updates mark hidden search buffers for refresh when shown."
  (let ((buffer (get-buffer-create " *nostr-hidden-search-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (nostr-search-mode)
            (setq-local nostr-search-query "hidden")
            (setq-local nostr--refresh-dirty nil))
          (cl-letf (((symbol-function 'nostr-search-refresh)
                     (lambda () (error "hidden buffer should not refresh"))))
            (nostr-search-refresh-visible-buffers))
          (with-current-buffer buffer
            (should nostr--refresh-dirty)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest nostr-relay-close-subscription-all-clears-pending-requests ()
  "Closing a subscription id also removes queued copies not yet sent to relays."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--pending-subscriptions (make-hash-table :test #'equal))
        (nostr-relay--syncing-subs (make-hash-table :test #'equal))
        (nostr-relay--sync-timeout-timer nil))
    (puthash "wss://relay.example"
             '(("drop-me" (((kinds . (1))))) ("keep-me" (((kinds . (0))))))
             nostr-relay--pending-subscriptions)
    (puthash "wss://relay.example\0drop-me" t nostr-relay--syncing-subs)
    (puthash "wss://relay.example\0keep-me" t nostr-relay--syncing-subs)
    (nostr-relay-close-subscription-all "drop-me")
    (should (equal (mapcar #'car (gethash "wss://relay.example"
                                          nostr-relay--pending-subscriptions))
                   '("keep-me")))
    (should-not (gethash "wss://relay.example\0drop-me" nostr-relay--syncing-subs))
    (should (gethash "wss://relay.example\0keep-me" nostr-relay--syncing-subs))))

(ert-deftest nostr-relay-forget-relay-scrubs-local-subscription-state ()
  "Disconnecting one relay clears its local subscription and catch-up state."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--connecting (make-hash-table :test #'equal))
        (nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--pending-subscriptions (make-hash-table :test #'equal))
        (nostr-relay--syncing-subs (make-hash-table :test #'equal))
        (nostr-relay--sync-timeout-timer nil)
        (nostr-relay--visible-reaction-relays (make-hash-table :test #'equal))
        (nostr-relay--connect-queue '(("wss://drop.example" . "me")
                                      ("wss://keep.example" . "me"))))
    (puthash "wss://drop.example" t nostr-relay--connections)
    (puthash "wss://drop.example" t nostr-relay--connecting)
    (puthash "wss://drop.example\0sub" t nostr-relay--subscriptions)
    (puthash "wss://keep.example\0sub" t nostr-relay--subscriptions)
    (puthash "wss://drop.example\0sub" t nostr-relay--syncing-subs)
    (puthash "wss://keep.example\0sub" t nostr-relay--syncing-subs)
    (puthash "wss://drop.example" '(("sub" nil)) nostr-relay--pending-subscriptions)
    (puthash "wss://drop.example" t nostr-relay--visible-reaction-relays)
    (cl-letf (((symbol-function 'nostr-relay--update-mode-line) #'ignore))
      (nostr-relay-forget-relay "wss://drop.example"))
    (should-not (gethash "wss://drop.example" nostr-relay--connections))
    (should-not (gethash "wss://drop.example" nostr-relay--connecting))
    (should-not (gethash "wss://drop.example\0sub" nostr-relay--subscriptions))
    (should (gethash "wss://keep.example\0sub" nostr-relay--subscriptions))
    (should-not (gethash "wss://drop.example\0sub" nostr-relay--syncing-subs))
    (should (gethash "wss://keep.example\0sub" nostr-relay--syncing-subs))
    (should-not (gethash "wss://drop.example" nostr-relay--pending-subscriptions))
    (should-not (gethash "wss://drop.example" nostr-relay--visible-reaction-relays))
    (should (equal nostr-relay--connect-queue '(("wss://keep.example" . "me"))))))

(ert-deftest nostr-relay-daemon-add-relay-error-forgets-local-relay ()
  "Daemon add-relay failures clear optimistic local relay bookkeeping."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--connecting (make-hash-table :test #'equal))
        (nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--pending-subscriptions (make-hash-table :test #'equal))
        (nostr-relay--syncing-subs (make-hash-table :test #'equal))
        (nostr-relay--sync-timeout-timer nil)
        (nostr-relay--visible-reaction-relays (make-hash-table :test #'equal))
        (nostr-relay--connect-queue nil))
    (puthash "wss://bad.example" t nostr-relay--connections)
    (puthash "wss://bad.example\0sub" t nostr-relay--subscriptions)
    (cl-letf (((symbol-function 'nostr-relay--update-mode-line) #'ignore))
      (nostr-relay--on-daemon-notification
       '((event . "error") (code . "add-relay") (url . "wss://bad.example"))))
    (should-not (gethash "wss://bad.example" nostr-relay--connections))
    (should-not (gethash "wss://bad.example\0sub" nostr-relay--subscriptions))))

(ert-deftest nostr-relay-daemon-subscribe-error-clears-subscription-state ()
  "Daemon subscribe failures clear active, pending, and syncing state for SUB."
  (let ((nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--pending-subscriptions (make-hash-table :test #'equal))
        (nostr-relay--syncing-subs (make-hash-table :test #'equal))
        (nostr-relay--sync-timeout-timer nil))
    (puthash "wss://relay.example\0bad-sub" t nostr-relay--subscriptions)
    (puthash "wss://relay.example" '(("bad-sub" nil) ("keep-sub" nil))
             nostr-relay--pending-subscriptions)
    (puthash "wss://relay.example\0bad-sub" t nostr-relay--syncing-subs)
    (nostr-relay--on-daemon-notification
     '((event . "error") (code . "subscribe") (sub . "bad-sub")))
    (should-not (gethash "wss://relay.example\0bad-sub" nostr-relay--subscriptions))
    (should (equal (mapcar #'car (gethash "wss://relay.example"
                                          nostr-relay--pending-subscriptions))
                   '("keep-sub")))
    (should-not (gethash "wss://relay.example\0bad-sub" nostr-relay--syncing-subs))))

(ert-deftest nostr-relay-addressable-fetch-closes-queued-disconnected-relays ()
  "Queued naddr fetches still arm the one-shot close timer."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--pending-subscriptions (make-hash-table :test #'equal))
        (nostr-relay--addressable-event-requests (make-hash-table :test #'equal))
        scheduled
        opened)
    (cl-letf (((symbol-function 'nostr-relay-open)
               (lambda (url _pubkey) (push url opened) t))
              ((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (push (cons fn args) scheduled)
                 'timer)))
      (should (= (nostr-relay-fetch-addressable-event
                  30023 "author" "article" '("wss://relay.example"))
                 1))
      (should (equal opened '("wss://relay.example")))
      (should (gethash "wss://relay.example" nostr-relay--pending-subscriptions))
      (pcase-let ((`(,fn . ,args) (car scheduled)))
        (should (eq fn #'nostr-relay-close-subscription-all))
        (should (string-prefix-p "naddr-" (car args)))))))

(ert-deftest nostr-relay-reconcile-account-replaces-personal-subscriptions ()
  "Changing accounts updates daemon pubkey and re-primes personal/follows feeds."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--pending-subscriptions (make-hash-table :test #'equal))
        (nostr-relay--daemon-pubkey "old")
        closed
        pubkeys
        personal
        follows)
    (puthash "wss://relay-a.example" t nostr-relay--connections)
    (puthash "wss://relay-b.example" t nostr-relay--connections)
    (puthash "wss://relay-a.example\0personal-old" t nostr-relay--subscriptions)
    (puthash "wss://relay-a.example\0follows-old" t nostr-relay--subscriptions)
    (puthash "wss://relay-a.example\0visible-reactions-old" t nostr-relay--subscriptions)
    (cl-letf (((symbol-function 'nostr-daemon-running-p) (lambda () t))
              ((symbol-function 'nostr-daemon-set-pubkey)
               (lambda (pubkey) (push pubkey pubkeys)))
              ((symbol-function 'nostr-relay-close-subscription-all)
               (lambda (sub-id) (push sub-id closed)))
              ((symbol-function 'nostr-relay-subscribe-personal)
               (lambda (url pubkey) (push (list url pubkey) personal)))
              ((symbol-function 'nostr-relay-subscribe-follows-feed)
               (lambda (url pubkey) (push (list url pubkey) follows))))
      (nostr-relay-reconcile-account "new")
      (should (equal pubkeys '("new")))
      (should (member "personal-old" closed))
      (should (member "follows-old" closed))
      (should-not (member "visible-reactions-old" closed))
      (should (= (length personal) 2))
      (should (= (length follows) 2))
      (should (equal nostr-relay--daemon-pubkey "new")))))

(ert-deftest nostr-relay-reconcile-account-drops-stale-connect-queue ()
  "Account switches discard deferred relay opens for the previous account."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--pending-subscriptions (make-hash-table :test #'equal))
        (nostr-relay--connect-queue '(("wss://old.example" . "old")
                                      ("wss://new.example" . "new")))
        (nostr-relay--daemon-pubkey "old")
        opened
        pubkeys)
    (cl-letf (((symbol-function 'nostr-daemon-running-p) (lambda () t))
              ((symbol-function 'nostr-daemon-set-pubkey)
               (lambda (pubkey) (push pubkey pubkeys)))
              ((symbol-function 'nostr-relay--update-mode-line) #'ignore)
              ((symbol-function 'nostr-daemon-add-relay)
               (lambda (url) (push url opened)))
              ((symbol-function 'nostr-relay-subscribe-personal) #'ignore)
              ((symbol-function 'nostr-relay-subscribe-follows-feed) #'ignore))
      (nostr-relay-reconcile-account "new")
      (should (equal pubkeys '("new")))
      (should (equal nostr-relay--connect-queue '(("wss://new.example" . "new"))))
      (nostr-relay--drain-connect-queue)
      (should (equal opened '("wss://new.example")))
      (should (equal nostr-relay--daemon-pubkey "new")))))

(ert-deftest nostr-close-removes-search-refresh-hook ()
  "Closing the client removes the relay-search refresh hook too."
  (let ((nostr-relay-event-hook '(nostr--schedule-refresh
                                  nostr-search--schedule-refresh)))
    (cl-letf (((symbol-function 'nostr-relay--flush-ingestion) #'ignore)
              ((symbol-function 'nostr-discover-close) #'ignore)
              ((symbol-function 'nostr-relay-disconnect-all) #'ignore)
              ((symbol-function 'nostr-db-close) #'ignore))
      (nostr-close)
      (should-not (memq #'nostr-search--schedule-refresh nostr-relay-event-hook)))))

(ert-deftest nostr-sync-ui-relay-interests-closes-hidden-view-work ()
  "UI reconciliation closes view-owned live work when those views are hidden."
  (let (visible-synced
        global-closed
        discover-closed)
    (cl-letf (((symbol-function 'nostr-relay-sync-visible-reactions)
               (lambda (&rest _) (setq visible-synced t)))
              ((symbol-function 'nostr-relay-close-global)
               (lambda () (setq global-closed t)))
              ((symbol-function 'nostr-discover-close)
               (lambda (&optional quiet)
                 (setq discover-closed quiet))))
      (nostr-sync-ui-relay-interests)
      (should visible-synced)
      (should global-closed)
      (should discover-closed))))

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
                 (lambda (url sub-id filters &optional _close-on-eose)
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
        (nostr-relay--on-stored-event
         "sub"
         '((id . "repost-event")
           (pubkey . "alice")
           (created_at . 120)
           (kind . 6)
           (tags . (("e" "missing-original")))
           (content . "")
           (sig . "sig"))))
        (should (equal requested '("missing-original"))))))

(ert-deftest nostr-dispatch-opens-public-nip19-and-search ()
  (let (opened)
    (cl-letf (((symbol-function 'nostr-profile-open)
               (lambda (pubkey) (push (list 'profile pubkey) opened)))
              ((symbol-function 'nostr-thread-open)
               (lambda (event) (push (list 'thread (alist-get 'id event)) opened)))
              ((symbol-function 'nostr-search-open)
               (lambda (query) (push (list 'search query) opened)))
              ((symbol-function 'nostr-db-select-addressable-event)
               (lambda (_kind _pubkey _identifier) nil))
              ((symbol-function 'nostr-relay-fetch-addressable-event)
               (lambda (kind pubkey identifier relays)
                 (push (list 'fetch-naddr kind pubkey identifier relays) opened)))
              ((symbol-function 'nostr-nip19-decode-sync)
               (lambda (value)
                 (pcase value
                   ("npub1test" '((ok . t) (entity . "npub") (pubkey . "pubkey1")))
                   ("nprofile1test" '((ok . t) (entity . "nprofile") (pubkey . "pubkey2")))
                   ("note1test" '((ok . t) (entity . "note") (event_id . "event1")))
                   ("nevent1test" '((ok . t) (entity . "nevent") (event_id . "event2")))
                   ("naddr1test" '((ok . t) (entity . "naddr") (kind . 30023)
                                    (pubkey . "pubkey3") (identifier . "article")
                                    (relays . ("wss://relay.example"))))
                   (_ (error "unexpected value")))))
              ((symbol-function 'nostr-dispatch--event-by-id)
               (lambda (event-id)
                 (when (equal event-id "event1")
                   `((id . ,event-id))))))
      (nostr-open-identifier "npub1test")
      (nostr-open-identifier "nostr:nprofile1test")
      (nostr-open-identifier "note1test")
      (nostr-open-identifier "nevent1test")
      (nostr-open-identifier "naddr1test")
      (nostr-open-identifier "anything else"))
    (should (equal (nreverse opened)
                   '((profile "pubkey1")
                     (profile "pubkey2")
                     (thread "event1")
                     (search "event2")
                     (fetch-naddr 30023 "pubkey3" "article" ("wss://relay.example"))
                     (search "30023:pubkey3:article")
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

(ert-deftest nostr-note-buffer-selected-note-commands-error-without-selection ()
  "Timeline and thread note actions report a missing selected note."
  (with-temp-buffer
    (nostr-timeline-mode)
    (dolist (command '(nostr-timeline-reply
                       nostr-timeline-like
                       nostr-timeline-view-reactions
                       nostr-timeline-repost
                       nostr-timeline-quote
                       nostr-timeline-open-author
                       nostr-timeline-open-thread))
      (should-error (call-interactively command) :type 'user-error)))
  (with-temp-buffer
    (nostr-thread-mode)
    (dolist (command '(nostr-thread-reply
                       nostr-thread-like
                       nostr-thread-view-reactions
                       nostr-thread-repost
                       nostr-thread-quote
                       nostr-thread-open-author
                       nostr-thread-open-at-point))
      (should-error (call-interactively command) :type 'user-error))))

(ert-deftest nostr-ui-pages-bind-question-mark-to-transients ()
  "Every Nostr UI page exposes its action menu on `?'."
  (dolist (entry `((,nostr-timeline-mode-map . nostr-timeline-actions)
                   (,nostr-thread-mode-map . nostr-thread-actions)
                   (,nostr-profile-mode-map . nostr-profile-actions)
                   (,nostr-profile-list-mode-map . nostr-profile-list-actions)
                   (,nostr-search-mode-map . nostr-search-actions)
                   (,nostr-notifications-mode-map . nostr-notifications-actions)
                   (,nostr-reactions-mode-map . nostr-reactions-actions)
                   (,nostr-relays-mode-map . nostr-relays-actions)
                   (,nostr-setup-mode-map . nostr-setup-actions)))
    (let ((map (car entry))
          (command (cdr entry)))
      (should (fboundp command))
      (should (commandp command))
      (should (eq (lookup-key map (kbd "?")) command)))))

(ert-deftest nostr-sectioned-buffers-bind-tab-to-toggle ()
  "Sectioned Nostr buffers expose folding on TAB."
  (dolist (map (list nostr-timeline-mode-map
                     nostr-thread-mode-map
                     nostr-profile-mode-map
                     nostr-profile-list-mode-map
                     nostr-search-mode-map
                     nostr-notifications-mode-map
                     nostr-reactions-mode-map
                     nostr-relays-mode-map))
    (should (eq (lookup-key map (kbd "TAB")) #'nostr-ui-toggle-section))))

(ert-deftest nostr-compose-binds-actions-off-question-mark ()
  "Compose buffers expose their menu on `C-c ?' so `?' self-inserts."
  (should (commandp #'nostr-compose-actions))
  (should (eq (lookup-key nostr-compose-mode-map (kbd "C-c ?"))
             #'nostr-compose-actions))
  (should-not (eq (lookup-key nostr-compose-mode-map (kbd "?"))
                  #'nostr-compose-actions)))

;;;; Relay daemon driver

(ert-deftest nostr-daemon-filter-dispatches-complete-lines ()
  "The stdout filter parses whole JSON lines and buffers partial ones."
  (let ((nostr-daemon--process 'sentinel)
        (nostr-daemon--stdout-acc "")
        (nostr-daemon--pending-lines nil)
        (nostr-daemon--draining nil)
        (nostr-daemon--refresh-timer nil)
        seen)
    (let ((nostr-daemon-event-hook
           (list (lambda (n) (push (alist-get 'event n) seen)))))
      ;; Two complete lines plus a partial third.
      (nostr-daemon--filter
       'sentinel
       "{\"event\":\"ready\"}\n{\"event\":\"eose\",\"sub\":\"x\"}\n{\"event\":\"sto")
      (should (equal (reverse seen) '("ready" "eose")))
      ;; The partial line is held until its newline arrives.
      (should (equal nostr-daemon--stdout-acc "{\"event\":\"sto"))
      (nostr-daemon--filter 'sentinel "red\",\"id\":\"abc\"}\n")
      (should (equal (reverse seen) '("ready" "eose" "stored")))
      (should (string-empty-p nostr-daemon--stdout-acc)))
    ;; A `stored' line arms the debounced refresh timer.
    (should (timerp nostr-daemon--refresh-timer))
    (cancel-timer nostr-daemon--refresh-timer)
    (setq nostr-daemon--refresh-timer nil)))

(ert-deftest nostr-daemon-filter-is-reentrancy-safe ()
  "A handler that re-enters the filter (as emacsql does) must not corrupt parsing.
Line handlers run emacsql queries, which pump process output and re-enter the
filter mid-handling.  Every line must still be handled exactly once, in order,
with no `Args out of range' from a mutated accumulator."
  (let ((nostr-daemon--process 'sentinel)
        (nostr-daemon--stdout-acc "")
        (nostr-daemon--pending-lines nil)
        (nostr-daemon--draining nil)
        handled)
    (let ((nostr-daemon-event-hook
           (list (lambda (n)
                   (push (alist-get 'id n) handled)
                   ;; Mimic emacsql pumping output: feed more lines re-entrantly
                   ;; while handling the first one.
                   (when (equal (alist-get 'id n) "a")
                     (nostr-daemon--filter
                      'sentinel
                      "{\"event\":\"stored\",\"id\":\"b\"}\n{\"event\":\"stored\",\"id\":\"c\"}\n"))))))
      (nostr-daemon--filter 'sentinel "{\"event\":\"stored\",\"id\":\"a\"}\n")
      ;; All three handled exactly once, in arrival order, accumulator drained.
      (should (equal (reverse handled) '("a" "b" "c")))
      (should (string-empty-p nostr-daemon--stdout-acc))
      (should (null nostr-daemon--pending-lines)))
    (when (timerp nostr-daemon--refresh-timer)
      (cancel-timer nostr-daemon--refresh-timer)
      (setq nostr-daemon--refresh-timer nil))))

(ert-deftest nostr-daemon-subscribe-encodes-filter-vectors ()
  "Subscribe commands serialize filters as JSON arrays of objects."
  (let (sent)
    (cl-letf (((symbol-function 'nostr-daemon-running-p) (lambda () t))
              ((symbol-function 'process-send-string)
               (lambda (_proc str) (setq sent (concat sent str)))))
      (let ((nostr-daemon--process 'sentinel))
        (nostr-daemon-subscribe "feed" (list '((kinds . [1]) (limit . 5))))))
    (let ((parsed (nostr-backend--json-read (string-trim sent))))
      (should (equal (alist-get 'op parsed) "subscribe"))
      (should (equal (alist-get 'id parsed) "feed"))
      (let ((filter (car (alist-get 'filters parsed))))
        (should (equal (alist-get 'kinds filter) '(1)))
        (should (equal (alist-get 'limit filter) 5))))
    ;; Each command must be newline-terminated so the daemon reads one per line.
    (should (string-suffix-p "\n" sent))))

(provide 'nostr-test)
;;; nostr-test.el ends here
