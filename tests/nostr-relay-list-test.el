;;; nostr-relay-list-test.el --- NIP-65 relay list DB tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for kind 10002 relay-list storage.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'nostr-db)
(require 'nostr-relay)

(defmacro nostr-relay-list-test-with-db (&rest body)
  "Run BODY with an isolated in-memory Nostr database."
  (declare (indent 0))
  `(let ((nostr-db--connection (emacsql-sqlite-open nil)))
     (unwind-protect
         (progn
           (nostr-db-init)
           ,@body)
       (emacsql-close nostr-db--connection))))

(ert-deftest nostr-db-stores-nip65-relay-list-markers ()
  "Kind 10002 r-tags are stored with marker and read/write policy."
  (nostr-relay-list-test-with-db
    (nostr-db-store-event
     '((id . "relay-list")
       (pubkey . "alice")
       (created_at . 100)
       (kind . 10002)
       (tags . (("r" "wss://both.example")
                ("r" "wss://read.example" "read")
                ("r" "wss://write.example" "write")
                ("r")
                ("p" "not-a-relay")))
       (content . "")
       (sig . "sig")))
    (should
     (equal (nostr-db-select-relay-list "alice")
            '(((url . "wss://both.example")
               (marker)
               (read . t)
               (write . t))
              ((url . "wss://read.example")
               (marker . "read")
               (read . t)
               (write))
              ((url . "wss://write.example")
               (marker . "write")
               (read)
               (write . t)))))))

(ert-deftest nostr-db-replaces-nip65-relay-list-for-pubkey ()
  "A new kind 10002 event replaces the cached relay list for its pubkey."
  (nostr-relay-list-test-with-db
    (nostr-db-store-event
     '((id . "old-relay-list")
       (pubkey . "alice")
       (created_at . 100)
       (kind . 10002)
       (tags . (("r" "wss://old.example" "read")))
       (content . "")
       (sig . "sig")))
    (nostr-db-store-event
     '((id . "new-relay-list")
       (pubkey . "alice")
       (created_at . 101)
       (kind . 10002)
       (tags . (("r" "wss://new.example" "write")))
       (content . "")
       (sig . "sig")))
    (nostr-db-store-event
     '((id . "bob-relay-list")
       (pubkey . "bob")
       (created_at . 101)
       (kind . 10002)
       (tags . (("r" "wss://bob.example")))
       (content . "")
       (sig . "sig")))
    (should (equal (nostr-db-select-relay-list "alice")
                   '(((url . "wss://new.example")
                      (marker . "write")
                      (read)
                      (write . t)))))
    (should (equal (nostr-db-select-relay-list "bob")
                   '(((url . "wss://bob.example")
                      (marker)
                      (read . t)
                      (write . t)))))))

(ert-deftest nostr-relay-urls-for-pubkey-combines-configured-and-nip65 ()
  "Relay candidates include static relays and cached NIP-65 relays."
  (nostr-relay-list-test-with-db
    (let ((nostr-relay-urls '("wss://static.example" "wss://read.example")))
      (nostr-db-store-event
       '((id . "relay-list")
         (pubkey . "alice")
         (created_at . 100)
         (kind . 10002)
         (tags . (("r" "wss://read.example" "read")
                  ("r" "wss://write.example" "write")
                  ("r" "wss://both.example")))
         (content . "")
         (sig . "sig")))
      (should (equal (nostr-relay-urls-for-pubkey "alice")
                     '("wss://static.example"
                       "wss://read.example"
                       "wss://both.example"
                       "wss://write.example")))
      (should (equal (nostr-relay-urls-for-pubkey "alice" 'read)
                     '("wss://static.example"
                       "wss://read.example"
                       "wss://both.example")))
      (should (equal (nostr-relay-urls-for-pubkey "alice" 'write)
                     '("wss://static.example"
                       "wss://read.example"
                       "wss://both.example"
                       "wss://write.example"))))))

(ert-deftest nostr-relay-connect-all-opens-cached-nip65-relays ()
  "Connecting all relays uses cached relay-list candidates."
  (nostr-relay-list-test-with-db
    (let ((nostr-relay-urls '("wss://static.example"))
          (nostr-relay--connections (make-hash-table :test #'equal))
          opened)
      (nostr-db-store-event
       '((id . "relay-list")
         (pubkey . "alice")
         (created_at . 100)
         (kind . 10002)
         (tags . (("r" "wss://read.example" "read")))
         (content . "")
         (sig . "sig")))
      (cl-letf (((symbol-function 'nostr-relay-open)
                 (lambda (url _pubkey)
                   (push url opened)
                   (puthash url :ws nostr-relay--connections))))
        (nostr-relay-connect-all "alice"))
      (should (equal (sort opened #'string<)
                     '("wss://read.example" "wss://static.example"))))))

(ert-deftest nostr-relay-connect-all-deferred-queues-cached-nip65-relays ()
  "Deferred connecting queues startup relays without opening them immediately."
  (nostr-relay-list-test-with-db
    (let ((nostr-relay-urls '("wss://static.example"))
          (nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-relay--connect-queue nil)
          (nostr-relay--connect-timer nil)
          opened)
      (nostr-db-store-event
       '((id . "relay-list")
         (pubkey . "alice")
         (created_at . 100)
         (kind . 10002)
         (tags . (("r" "wss://read.example" "read")))
         (content . "")
         (sig . "sig")))
      (cl-letf (((symbol-function 'nostr-relay-open)
                 (lambda (url _pubkey)
                   (push url opened)
                   (puthash url :ws nostr-relay--connections)))
                ((symbol-function 'nostr-relay-discover-and-connect) #'ignore))
        (unwind-protect
            (progn
              (nostr-relay-connect-all-deferred "alice")
              (should-not opened)
              (should (equal (sort (mapcar #'car nostr-relay--connect-queue) #'string<)
                             '("wss://read.example" "wss://static.example")))
              (nostr-relay--drain-connect-queue)
              (should (= (length opened) 1))
              (nostr-relay--drain-connect-queue)
              (should (equal (sort opened #'string<)
                             '("wss://read.example" "wss://static.example"))))
          (when (timerp nostr-relay--connect-timer)
            (cancel-timer nostr-relay--connect-timer)))))))

(ert-deftest nostr-relay-send-client-message-prefers-write-relays ()
  "Publishing targets cached write relays when available."
  (nostr-relay-list-test-with-db
    (let ((old-pubkey (and (boundp 'nostr-current-pubkey) nostr-current-pubkey))
          (nostr-relay--connections (make-hash-table :test #'equal))
          sent)
      (unwind-protect
          (progn
            (setq nostr-current-pubkey "alice")
            (nostr-db-store-event
             '((id . "relay-list")
               (pubkey . "alice")
               (created_at . 100)
               (kind . 10002)
               (tags . (("r" "wss://read.example" "read")
                        ("r" "wss://write.example" "write")))
               (content . "")
               (sig . "sig")))
            (puthash "wss://read.example" t nostr-relay--connections)
            (puthash "wss://write.example" t nostr-relay--connections)
            (cl-letf (((symbol-function 'nostr-daemon-publish)
                       (lambda (_event urls) (setq sent urls))))
              (nostr-relay-send-client-message
               "[\"EVENT\",{\"id\":\"note-1\",\"pubkey\":\"alice\"}]"))
            (should (equal sent '("wss://write.example"))))
        (setq nostr-current-pubkey old-pubkey)))))

(ert-deftest nostr-relay-retry-publish-targets-failed-stale-and-missing ()
  "Retry publishing skips accepted and fresh pending relays."
  (nostr-relay-list-test-with-db
    (let ((old-pubkey (and (boundp 'nostr-current-pubkey) nostr-current-pubkey))
          (nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-relay-publish-pending-stale-seconds 60)
          sent)
      (unwind-protect
          (progn
            (setq nostr-current-pubkey "alice")
            (nostr-db-store-event
             '((id . "relay-list")
               (pubkey . "alice")
               (created_at . 100)
               (kind . 10002)
               (tags . (("r" "wss://accepted.example" "write")
                        ("r" "wss://rejected.example" "write")
                        ("r" "wss://fresh.example" "write")
                        ("r" "wss://stale.example" "write")
                        ("r" "wss://missing.example" "write")))
               (content . "")
               (sig . "sig")))
            (dolist (url '("wss://accepted.example"
                           "wss://rejected.example"
                           "wss://fresh.example"
                           "wss://stale.example"
                           "wss://missing.example"))
              (puthash url t nostr-relay--connections))
            (nostr-db-store-publish-receipt "note-1" "wss://accepted.example" "accepted")
            (nostr-db-store-publish-receipt "note-1" "wss://rejected.example" "rejected")
            (nostr-db-store-publish-receipt "note-1" "wss://fresh.example" "pending")
            (nostr-db-store-publish-receipt "note-1" "wss://stale.example" "pending")
            (emacsql nostr-db--connection
                     [:update publish_receipts
                              :set (= updated_at 1)
                              :where (and (= event_id "note-1")
                                          (= url "wss://stale.example"))])
            (cl-letf (((symbol-function 'nostr-daemon-publish)
                       (lambda (_event urls) (setq sent urls))))
              (should
               (= 3
                  (nostr-relay-retry-publish
                   '((id . "note-1")
                     (pubkey . "alice")
                     (created-at . 120)
                     (kind . 1)
                     (tags . nil)
                     (content . "hello")
                     (sig . "sig"))))))
            (should (equal (sort sent #'string<)
                           '("wss://missing.example"
                             "wss://rejected.example"
                             "wss://stale.example"))))
        (setq nostr-current-pubkey old-pubkey)))))

(ert-deftest nostr-relay-retry-publish-rejects-other-authors ()
  "Retry publishing is restricted to the current account's own notes."
  (nostr-relay-list-test-with-db
    (let ((old-pubkey (and (boundp 'nostr-current-pubkey) nostr-current-pubkey)))
      (unwind-protect
          (progn
            (setq nostr-current-pubkey "alice")
            (should-error
             (nostr-relay-retry-publish
              '((id . "note-1")
                (pubkey . "bob")
                (created-at . 120)
                (kind . 1)
                (tags . nil)
                (content . "hello")
                (sig . "sig")))
             :type 'user-error))
        (setq nostr-current-pubkey old-pubkey)))))

(provide 'nostr-relay-list-test)
;;; nostr-relay-list-test.el ends here
