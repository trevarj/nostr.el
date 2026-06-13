;;; nostr-operational-buffers-test.el --- Operational buffer tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'nostr)
(require 'nostr-compose)
(require 'nostr-db)
(require 'nostr-dispatch)
(require 'nostr-notifications)
(require 'nostr-profile)
(require 'nostr-relays)
(require 'nostr-search)
(require 'nostr-thread)

(declare-function make-websocket "websocket")

(defmacro nostr-operational-test--with-db (&rest body)
  "Run BODY with a temporary Nostr database."
  (declare (indent 0))
  `(let ((path (make-temp-file "nostr-operational" nil ".sqlite")))
     (unwind-protect
         (progn
           (nostr-db-open path)
           ,@body)
       (nostr-db-close)
       (when (file-exists-p path)
         (delete-file path)))))

(defun nostr-operational-test--goto-first-section ()
  "Move point to the first rendered section."
  (goto-char (point-min))
  (search-forward "▾"))

(ert-deftest nostr-notifications-render-and-select ()
  "Notifications render stored rows and expose data at point."
  (nostr-operational-test--with-db
    (nostr-db-store-profile-event
     '((pubkey . "actor1")
       (created_at . 10)
       (content . "{\"name\":\"alice\",\"display_name\":\"Alice\"}")))
    (nostr-db-store-text-event
     '((id . "note1")
       (pubkey . "actor1")
       (created_at . 20)
       (kind . 1)
       (tags . nil)
       (content . "hello from nostr")
       (sig . "sig")
       (relay . "wss://relay.example")
       (root-id . nil)
       (reply-id . nil)
       (quote-id . nil)))
    (emacsql nostr-db--connection
             [:insert :into notifications :values [$s1 $s2 $s3 $s4 $s5 $s6 $s7]]
             "notif1" "mention" "note1" "actor1" "me" 30 0)
    (with-temp-buffer
      (nostr-notifications-mode)
      (nostr-notifications-refresh)
      (should (eq major-mode 'nostr-notifications-mode))
      (should (string-match-p "Notifications  Nostr" (buffer-string)))
      (should (string-match-p "Alice mentioned you" (buffer-string)))
      (should (string-match-p "●" (buffer-string)))
      (should (string-match-p "hello from nostr" (buffer-string)))
      (nostr-operational-test--goto-first-section)
      (should (equal (alist-get 'id (nostr-notifications-selected)) "notif1"))
      (should (equal (alist-get 'event-id (nostr-notifications-selected)) "note1")))))

(ert-deftest nostr-notifications-render-reaction-content-and-thread-context ()
  "Reaction notifications show the reaction and open the reacted-to thread."
  (nostr-operational-test--with-db
    (nostr-db-store-profile-event
     '((pubkey . "alice")
       (created_at . 10)
       (content . "{\"display_name\":\"Alice\"}")))
    (nostr-db-store-text-event
     '((id . "my-note")
       (pubkey . "me")
       (created_at . 20)
       (kind . 1)
       (tags . nil)
       (content . "target note")
       (sig . "sig")
       (relay . "wss://relay.example")
       (root-id . nil)
       (reply-id . nil)
       (quote-id . nil)))
    (nostr-db-store-reaction-event
     '((id . "reaction1")
       (pubkey . "alice")
       (created_at . 30)
       (kind . 7)
       (tags . (("e" "my-note")))
       (content . "🚀")
       (sig . "sig")))
    (emacsql nostr-db--connection
             [:insert :into notifications :values [$s1 $s2 $s3 $s4 $s5 $s6 $s7]]
             "notif-reaction" "reaction" "reaction1" "alice" "me" 31 0)
    (let (opened-thread)
      (cl-letf (((symbol-function 'nostr-thread-open)
                 (lambda (event) (setq opened-thread event))))
        (with-temp-buffer
          (nostr-notifications-mode)
          (nostr-notifications-refresh)
          (let ((text (buffer-string)))
            (should (string-match-p "Alice reacted 🚀" text))
            (should (string-match-p "target note" text))
            (should-not (string-match-p "actor alice-pubkey" text)))
          (nostr-operational-test--goto-first-section)
          (should (equal (alist-get 'context-event-id
                                    (nostr-notifications-selected))
                         "my-note"))
          (nostr-notifications-open-at-point)
          (should (equal (alist-get 'id opened-thread) "my-note")))))))

(ert-deftest nostr-notifications-tolerate-non-string-type ()
  "Malformed legacy notification rows do not crash the renderer."
  (nostr-operational-test--with-db
    (nostr-db-store-text-event
     '((id . "note1")
       (pubkey . "alice")
       (created_at . 20)
       (kind . 1)
       (tags . nil)
       (content . "legacy notification target")
       (sig . "sig")
       (relay . "wss://relay.example")
       (root-id . nil)
       (reply-id . nil)
       (quote-id . nil)))
    (emacsql nostr-db--connection
             [:insert :into notifications :values [$s1 $s2 $s3 $s4 $s5 $s6 $s7]]
             "notif-legacy" 1781299801 "note1" "alice" "me" 30 0)
    (with-temp-buffer
      (nostr-notifications-mode)
      (nostr-notifications-refresh)
      (should (string-match-p "notification" (buffer-string)))
      (should (string-match-p "legacy notification target" (buffer-string))))))

(ert-deftest nostr-notifications-mark-seen-updates-db-and-buffer ()
  "Notifications can be marked seen from the buffer."
  (nostr-operational-test--with-db
    (emacsql nostr-db--connection
             [:insert :into notifications :values [$s1 $s2 $s3 $s4 $s5 $s6 $s7]]
             "notif1" "mention" "note1" "actor1" "me" 30 0)
    (with-temp-buffer
      (nostr-notifications-mode)
      (nostr-notifications-refresh)
      (should (string-match-p "●" (buffer-string)))
      (nostr-operational-test--goto-first-section)
      (nostr-notifications-mark-seen)
      (should-not (string-match-p "●" (buffer-string)))
        (should (equal (emacsql nostr-db--connection
                                [:select [seen] :from notifications :where (= id "notif1")])
                     '((1)))))))

(ert-deftest nostr-notifications-open-event-and-actor ()
  "Notification commands open cached event threads and actor profiles."
  (nostr-operational-test--with-db
    (nostr-db-store-text-event
     '((id . "note1")
       (pubkey . "actor1")
       (created_at . 20)
       (kind . 1)
       (tags . nil)
       (content . "hello from nostr")
       (sig . "sig")
       (relay . "wss://relay.example")
       (root-id . nil)
       (reply-id . nil)
       (quote-id . nil)))
    (emacsql nostr-db--connection
             [:insert :into notifications :values [$s1 $s2 $s3 $s4 $s5 $s6 $s7]]
             "notif1" "mention" "note1" "actor1" "me" 30 0)
    (let (opened-thread opened-profile)
      (cl-letf (((symbol-function 'nostr-thread-open)
                 (lambda (event) (setq opened-thread event)))
                ((symbol-function 'nostr-profile-open)
                 (lambda (pubkey) (setq opened-profile pubkey))))
        (with-temp-buffer
          (nostr-notifications-mode)
          (nostr-notifications-refresh)
          (nostr-operational-test--goto-first-section)
          (nostr-notifications-open-at-point)
          (should (equal (alist-get 'id opened-thread) "note1"))
          (nostr-notifications-open-actor)
          (should (equal opened-profile "actor1")))))))

(ert-deftest nostr-notifications-open-uncached-event-searches ()
  "Notification open falls back to local search when the event is not cached."
  (nostr-operational-test--with-db
    (emacsql nostr-db--connection
             [:insert :into notifications :values [$s1 $s2 $s3 $s4 $s5 $s6 $s7]]
             "notif1" "mention" "missing-note" "actor1" "me" 30 0)
    (let (searched)
      (cl-letf (((symbol-function 'nostr-search-open)
                 (lambda (query) (setq searched query))))
        (with-temp-buffer
          (nostr-notifications-mode)
          (nostr-notifications-refresh)
          (nostr-operational-test--goto-first-section)
          (nostr-notifications-open-at-point)
          (should (equal searched "missing-note")))))))

(ert-deftest nostr-notifications-open-from-timeline-switches-in-place ()
  "Notifications behave like a primary nav tab from the timeline buffer."
  (nostr-operational-test--with-db
    (let (popped)
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _args)
                   (setq popped buffer)
                   buffer)))
        (with-temp-buffer
          (let ((buffer (current-buffer)))
            (nostr-timeline-mode)
            (nostr-notifications-open)
            (should (eq (current-buffer) buffer))
            (should-not popped)
            (should (eq major-mode 'nostr-notifications-mode))
            (should (string-match-p "Notifications  Nostr" (buffer-string)))
            (should (string-match-p "\\[N Notifications\\]" (buffer-string)))))))))

(ert-deftest nostr-relays-render-and-select ()
  "Relays render cached and configured state and expose data at point."
  (nostr-operational-test--with-db
    (let ((nostr-relay-urls '("wss://relay-a.example" "wss://relay-b.example")))
      (nostr-db-store-relay-status "wss://relay-b.example" "notice" "maintenance")
      (with-temp-buffer
        (nostr-relays-mode)
        (nostr-relays-refresh)
        (should (eq major-mode 'nostr-relays-mode))
        (should (string-match-p "wss://relay-a.example  source:configured  cache:candidate" (buffer-string)))
        (should (string-match-p "wss://relay-b.example  source:configured  cache:notice" (buffer-string)))
        (should (string-match-p "Policy: read=yes write=yes" (buffer-string)))
        (should (string-match-p "maintenance" (buffer-string)))
        (nostr-operational-test--goto-first-section)
        (should (equal (alist-get 'url (nostr-relays-selected))
                       "wss://relay-a.example"))))))

(ert-deftest nostr-relays-render-nip65-candidates ()
  "Relays buffer renders cached NIP-65 relays and policy."
  (nostr-operational-test--with-db
    (let ((old-pubkey (and (boundp 'nostr-current-pubkey) nostr-current-pubkey))
          (nostr-relay-urls '("wss://configured.example")))
      (unwind-protect
          (progn
            (setq nostr-current-pubkey "me")
            (nostr-db-store-event
             '((id . "relay-list")
               (pubkey . "me")
               (created_at . 10)
               (kind . 10002)
               (tags . (("r" "wss://read.example" "read")
                        ("r" "wss://write.example" "write")))
               (content . "")
               (sig . "sig")))
            (with-temp-buffer
              (nostr-relays-mode)
              (nostr-relays-refresh)
              (should (string-match-p "wss://read.example  source:nip65" (buffer-string)))
              (should (string-match-p "wss://write.example  source:nip65" (buffer-string)))
              (should (string-match-p "Policy: read=yes write=no" (buffer-string)))
              (should (string-match-p "Policy: read=no write=yes" (buffer-string)))))
        (setq nostr-current-pubkey old-pubkey)))))

(ert-deftest nostr-relays-disconnect-selected-updates-status ()
  "Disconnecting selected relay records a local closed state."
  (nostr-operational-test--with-db
    (let ((nostr-relay-urls '("wss://relay.example"))
          (nostr-relay--connections (make-hash-table :test #'equal)))
      (with-temp-buffer
        (nostr-relays-mode)
        (nostr-relays-refresh)
        (nostr-operational-test--goto-first-section)
        (nostr-relays-disconnect)
        (should (string-match-p "cache:closed" (buffer-string)))
        (should (equal (emacsql nostr-db--connection
                                [:select [state message]
                                         :from relay_status
                                         :where (= url "wss://relay.example")])
                       '(("closed" "Disconnected locally"))))))))

(ert-deftest nostr-relays-add-remove-and-copy-url ()
  "Relay management commands update session config and copy selected URLs."
  (nostr-operational-test--with-db
    (let ((nostr-relay-urls nil)
          (nostr-relay--connections (make-hash-table :test #'equal))
          copied)
      (cl-letf (((symbol-function 'kill-new) (lambda (value) (setq copied value))))
        (with-temp-buffer
          (nostr-relays-mode)
          (nostr-relays-add "wss://added.example")
          (should (member "wss://added.example" nostr-relay-urls))
          (nostr-operational-test--goto-first-section)
          (nostr-relays-copy-url)
          (should (equal copied "wss://added.example"))
          (nostr-relays-remove)
          (should-not (member "wss://added.example" nostr-relay-urls)))))))

(ert-deftest nostr-open-renders-main-buffer-without-blocking-on-relays ()
  "Main entrypoint opens the timeline even when one relay start is slow."
  (let ((path (make-temp-file "nostr-open-e2e" nil ".sqlite"))
        (nostr-current-pubkey "me")
        (nostr-relay-urls '("wss://slow.example" "wss://fast.example"))
        (nostr-relay-open-timeout-seconds 0.1)
        (nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--profile-requests (make-hash-table :test #'equal))
        (nostr-relay--connect-queue nil)
        (nostr-relay--connect-timer nil)
        opened-relays
        opened-buffers)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (url &rest _args)
                 (push url opened-relays)
                 (if (equal url "wss://slow.example")
                     (sleep-for 1)
                   (let ((proc (start-process "nostr-test-sleep" nil "sleep" "1")))
                     (make-websocket :conn proc :url url)))))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _args)
                 (push (buffer-name buffer) opened-buffers)
                 buffer)))
      (let ((nostr-db-path path))
        (unwind-protect
            (progn
              (nostr-open)
              (should (member nostr-buffer-name opened-buffers))
              (with-current-buffer nostr-buffer-name
                (should (eq major-mode 'nostr-timeline-mode))
                (should (string-match-p "Feed  Nostr" (buffer-string))))
              (should-not opened-relays)
              (should (= (length nostr-relay--connect-queue) 2)))
          (nostr-close)
          (when (get-buffer nostr-buffer-name)
            (kill-buffer nostr-buffer-name))
          (when (file-exists-p path)
            (delete-file path)))))))

(ert-deftest nostr-open-renders-before-account-derivation-finishes ()
  "Main entrypoint uses async account derivation instead of blocking setup."
  (let ((path (make-temp-file "nostr-open-async-account" nil ".sqlite"))
        (nostr-current-pubkey nil)
        (nostr--opening-account nil)
        (nostr-relay--connect-queue nil)
        (nostr-relay--connect-timer nil)
        opened-buffers
        async-called)
    (cl-letf (((symbol-function 'nostr-setup-derive-pubkey)
               (lambda () (error "sync derivation must not run from nostr-open")))
              ((symbol-function 'nostr-setup-derive-pubkey-async)
               (lambda (_success _error)
                 (setq async-called t)))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _args)
                 (push (buffer-name buffer) opened-buffers)
                 buffer)))
      (let ((nostr-db-path path))
        (unwind-protect
            (progn
              (nostr-open)
              (should async-called)
              (should (member nostr-buffer-name opened-buffers))
              (with-current-buffer nostr-buffer-name
                (should (eq major-mode 'nostr-timeline-mode))
                (should (string-match-p "Deriving configured account pubkey"
                                        (buffer-string)))))
          (nostr-close)
          (when (get-buffer nostr-buffer-name)
            (kill-buffer nostr-buffer-name))
          (when (file-exists-p path)
            (delete-file path)))))))

(ert-deftest nostr-e2e-major-interactive-buffers-render ()
  "Exercise primary interactive buffers against one realistic local cache."
  (nostr-operational-test--with-db
    (let ((nostr-current-pubkey "me")
          (nostr-relay-urls '("wss://relay.example"))
          (nostr-search-auto-relay nil))
      (nostr-db-store-profile-event
       '((pubkey . "alice")
         (created_at . 10)
         (kind . 0)
         (content . "{\"display_name\":\"Alice\",\"nip05\":\"alice@example.test\"}")))
      (nostr-db-store-event
       '((id . "contacts")
         (pubkey . "me")
         (created_at . 11)
         (kind . 3)
         (tags . (("p" "alice")))))
      (nostr-db-store-text-event
       '((id . "root")
         (pubkey . "alice")
         (created_at . 20)
         (kind . 1)
         (tags . nil)
         (content . "hello e2e")
         (sig . "sig")
         (relay . "wss://relay.example")
         (root-id . nil)
         (reply-id . nil)
         (quote-id . nil)))
      (emacsql nostr-db--connection
               [:insert :into notifications :values [$s1 $s2 $s3 $s4 $s5 $s6 $s7]]
               "notif-root" "mention" "root" "alice" "me" 21 0)
      (with-temp-buffer
        (nostr-timeline-mode)
        (setq-local nostr-timeline-current-pubkey "me")
        (nostr-timeline-refresh)
        (should (string-match-p "hello e2e" (buffer-string))))
      (with-temp-buffer
        (nostr-thread-mode)
        (setq-local nostr-thread-root-event '((id . "root")))
        (nostr-thread-refresh)
        (should (string-match-p "hello e2e" (buffer-string))))
      (with-temp-buffer
        (nostr-profile-mode)
        (setq-local nostr-profile-pubkey "alice")
        (nostr-profile-refresh)
        (should (string-match-p "Profile  Alice" (buffer-string))))
      (with-temp-buffer
        (nostr-search-mode)
        (setq-local nostr-search-query "e2e")
        (nostr-search-refresh)
        (should (string-match-p "hello e2e" (buffer-string))))
      (with-temp-buffer
        (nostr-notifications-mode)
        (nostr-notifications-refresh)
        (should (string-match-p "Alice .*mentioned you" (buffer-string))))
      (with-temp-buffer
        (nostr-relays-mode)
        (nostr-relays-refresh)
        (should (string-match-p "wss://relay.example" (buffer-string))))
      (with-temp-buffer
        (nostr-compose-mode)
        (insert "hello compose")
        (should (equal (nostr-compose--content) "hello compose"))))))

(provide 'nostr-operational-buffers-test)
;;; nostr-operational-buffers-test.el ends here
