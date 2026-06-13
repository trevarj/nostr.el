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
        (should (string-match-p "Feed  Nostr" text))
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
    (let ((home (nostr-test-render-timeline-feed 'feed))
          (replies (nostr-test-render-timeline-feed 'conversations))
          (global (nostr-test-render-timeline-feed 'global))
          (posts (nostr-test-render-timeline-feed 'my-posts))
          (media (nostr-test-render-timeline-feed 'media)))
      (should (string-match-p "Feed  Nostr" home))
      (should (string-match-p "home root" home))
      (should-not (string-match-p "own root" home))
      (should-not (string-match-p "home reply" home))
      (should-not (string-match-p "global root" home))
      (should (string-match-p "Conversations  Nostr" replies))
      (should (string-match-p "\\[C Conversations\\]" replies))
      (should (string-match-p "home reply" replies))
      (should-not (string-match-p "own reply" replies))
      (should-not (string-match-p "home root" replies))
      (should (string-match-p "Global  Nostr" global))
      (should (string-match-p "\\[G Global\\]" global))
      (should (string-match-p "global root" global))
      (should (string-match-p "home root" global))
      (should (string-match-p "home reply" global))
      (should (string-match-p "My Posts  Nostr" posts))
      (should (string-match-p "\\[P My Posts\\]" posts))
      (should (string-match-p "own root" posts))
      (should (string-match-p "own reply" posts))
      (should-not (string-match-p "home root" posts))
      (should (string-match-p "Media  Nostr" media))
      (should-not (string-match-p "\\[M Media\\]" media))
      (should (string-match-p "media https://example.test/pic.jpg" media))
      (should-not (string-match-p "home root" media)))))

(ert-deftest nostr-timeline-tabs-are-interactive ()
  "Timeline feed tabs expose button actions."
  (nostr-test-with-db
    (with-temp-buffer
      (nostr-timeline-mode)
      (setq-local nostr-timeline-current-pubkey "me")
      (nostr-timeline-refresh)
      (goto-char (point-min))
      (search-forward "G Global")
      (let ((button (button-at (point))))
        (should button)
        (button-activate button))
      (should (eq nostr-timeline-feed-kind 'global))
      (should (string-match-p "\\[G Global\\]" (buffer-string))))))

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
      (should (string-match-p "Conversations  Nostr" (buffer-string)))
      (nostr-timeline-global)
      (should (eq nostr-timeline-feed-kind 'global))
      (should (string-match-p "Global  Nostr" (buffer-string)))
      (nostr-timeline-my-posts)
      (should (eq nostr-timeline-feed-kind 'my-posts))
      (should (string-match-p "My Posts  Nostr" (buffer-string)))
      (nostr-timeline-media)
      (should (eq nostr-timeline-feed-kind 'media))
      (should (string-match-p "Media  Nostr" (buffer-string)))
      (nostr-timeline-feed)
      (should (eq nostr-timeline-feed-kind 'feed))
      (should (string-match-p "Feed  Nostr" (buffer-string))))))

(ert-deftest nostr-compose-content-strips-comment-context ()
  (with-temp-buffer
    (nostr-compose-mode)
    (insert ";; Replying to Alice\n\nhello\n")
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
    (cl-letf (((symbol-function 'switch-to-buffer)
               (lambda (buffer) (setq opened buffer))))
      (nostr-compose-open '((id . "reply-id")
                            (author . "Alice")
                            (content . "hello from alice"))))
    (unwind-protect
        (with-current-buffer opened
          (should (eq major-mode 'nostr-compose-mode))
          (should (string-match-p ";; Replying to Alice" (buffer-string)))
          (should (string-match-p ";; hello from alice" (buffer-string)))
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
      (cl-letf (((symbol-function 'switch-to-buffer)
                 (lambda (buffer) (setq opened buffer))))
        (nostr-compose-open nil '(("q" "quote-id" "relay"))))
      (unwind-protect
          (with-current-buffer opened
            (should (string-match-p ";; Quoting bob" (buffer-string)))
            (should (string-match-p ";; quoted content" (buffer-string)))
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

(ert-deftest nostr-compose-send-empty-signals-user-error ()
  (with-temp-buffer
    (nostr-compose-mode)
    (should-error (nostr-compose-send) :type 'user-error)))

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
      (let ((nostr-relay--connections (make-hash-table :test #'equal)))
        (puthash "wss://relay-a.example" t nostr-relay--connections)
        (puthash "wss://relay-b.example" t nostr-relay--connections)
        (let ((sub-id (nostr-relay-search "emacs nostr" 25)))
          (should (string-prefix-p "search-" sub-id))
          (should (= (length sent) 2))
          (pcase-let ((`(,_url _sub-id ,filters) (car sent)))
            (should (equal (alist-get "search" (car filters) nil nil #'equal)
                           "emacs nostr"))
            (should (equal (alist-get "limit" (car filters) nil nil #'equal)
                           25))))))))

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
    (cl-letf (((symbol-function 'nostr-relay-subscribe)
               (lambda (url sub-id filters)
                 (setq sent (list url sub-id filters)))))
      (nostr-relay-subscribe-personal "wss://relay.example" "me"))
    (pcase-let ((`(,url ,sub-id ,filters) sent))
      (should (equal url "wss://relay.example"))
      (should (string-prefix-p "personal-" sub-id))
      (should (= (length filters) 3))
      (should (equal (alist-get "authors" (nth 0 filters) nil nil #'equal)
                     '("me")))
      (should (member nostr-kind-metadata
                      (alist-get "kinds" (nth 0 filters) nil nil #'equal)))
      (should (member nostr-kind-zap-receipt
                      (alist-get "kinds" (nth 0 filters) nil nil #'equal)))
      (should-not (alist-get "#p" (nth 0 filters) nil nil #'equal))
      (should (equal (alist-get "#p" (nth 1 filters) nil nil #'equal)
                     '("me")))
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
  "Relay ingestion updates a compact global mode-line loading indicator."
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
              (should (equal nostr-relay--mode-line-string
                             " Nostr:loading 1 events/1 profiles"))
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
              (should (equal nostr-relay--mode-line-string
                             " Nostr:loading 2 events"))
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
        (should (member nostr-kind-zap-receipt
                        (alist-get "kinds" filter nil nil #'equal)))))))

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
      (should (= (nostr-relay-fetch-event-metadata '("note1" "note2" "note1" nil)) 2))
      (should (= (length sent) 2))
      (pcase-let ((`(,_url ,sub-id ,filters) (car sent)))
        (should (string-prefix-p "event-meta-" sub-id))
        (should (equal (alist-get "#e" (car filters) nil nil #'equal)
                       '("note1" "note2")))
        (dolist (kind (list nostr-kind-text-note
                            nostr-kind-repost
                            nostr-kind-reaction
                            nostr-kind-zap-receipt))
          (should (member kind (alist-get "kinds" (car filters) nil nil #'equal)))))
      (should (= (nostr-relay-fetch-event-metadata '("note1" "note2")) 0))
      (should (= (length sent) 2)))))

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
                   (_ (error "unexpected value")))))
              ((symbol-function 'nostr-dispatch--event-by-id)
               (lambda (event-id)
                 (when (equal event-id "event1")
                   `((id . ,event-id))))))
      (nostr-open-identifier "npub1test")
      (nostr-open-identifier "note1test")
      (nostr-open-identifier "anything else"))
    (should (equal (nreverse opened)
                   '((profile "pubkey1")
                     (thread "event1")
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
  (should (eq (lookup-key nostr-timeline-mode-map (kbd "G")) #'nostr-timeline-global))
  (should (eq (lookup-key nostr-timeline-mode-map (kbd "P")) #'nostr-timeline-my-posts))
  (should-not (lookup-key nostr-timeline-mode-map (kbd "M"))))

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
