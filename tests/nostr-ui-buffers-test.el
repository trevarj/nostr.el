;;; nostr-ui-buffers-test.el --- Tests for Nostr UI buffers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'nostr-db)
(require 'nostr-profile)
(require 'nostr-search)

(defmacro nostr-ui-buffers-test-with-db (&rest body)
  "Run BODY with a temporary Nostr database."
  (declare (indent 0))
  `(let ((nostr-db--connection (emacsql-sqlite-open nil)))
     (unwind-protect
         (progn
           (nostr-db-init)
           ,@body)
       (emacsql-close nostr-db--connection))))

(defun nostr-ui-buffers-test-store-event (event)
  "Store EVENT in the temporary test database."
  (nostr-db-store-event (nostr-event-normalize event "wss://relay.test")))

(ert-deftest nostr-profile-renders-metadata-and-recent-notes ()
  "Profile buffers show cached metadata and authored notes."
  (nostr-ui-buffers-test-with-db
    (nostr-ui-buffers-test-store-event
     '((id . "profile-event")
       (pubkey . "alice-pubkey")
       (created_at . 100)
       (kind . 0)
       (tags . nil)
       (content . "{\"name\":\"alice\",\"display_name\":\"Alice\",\"about\":\"Builds things\",\"nip05\":\"alice@example.com\",\"picture\":\"https://example.test/alice.png\"}")
       (sig . "sig")))
    (nostr-ui-buffers-test-store-event
     '((id . "note-1")
       (pubkey . "alice-pubkey")
       (created_at . 110)
       (kind . 1)
       (tags . nil)
       (content . "hello from alice")
       (sig . "sig")))
    (with-temp-buffer
      (nostr-profile-mode)
      (setq-local nostr-profile-pubkey "alice-pubkey")
      (nostr-profile-refresh)
      (should (equal major-mode 'nostr-profile-mode))
      (should (string-match-p "Alice" (buffer-string)))
      (should (string-match-p "alice@example.com" (buffer-string)))
      (should (string-match-p "\\[Avatar\\]" (buffer-string)))
      (should (string-match-p "hello from alice" (buffer-string)))
      (goto-char (point-min))
      (search-forward "hello from alice")
      (should (equal (alist-get 'id (nostr-ui-selected-data)) "note-1")))))

(ert-deftest nostr-profile-renders-social-context ()
  "Profile buffers show cached follow state, counts, and relay hints."
  (nostr-ui-buffers-test-with-db
    (let ((old-pubkey (and (boundp 'nostr-current-pubkey) nostr-current-pubkey)))
      (unwind-protect
          (progn
            (setq nostr-current-pubkey "me")
            (nostr-ui-buffers-test-store-event
             '((id . "profile-event")
               (pubkey . "alice-pubkey")
               (created_at . 100)
               (kind . 0)
               (tags . nil)
               (content . "{\"display_name\":\"Alice\"}")
               (sig . "sig")))
            (nostr-db-store-event
             '((id . "me-follows")
               (pubkey . "me")
               (created_at . 101)
               (kind . 3)
               (tags . (("p" "alice-pubkey")))))
            (nostr-db-store-event
             '((id . "me-mutes")
               (pubkey . "me")
               (created_at . 101)
               (kind . 10000)
               (tags . (("p" "alice-pubkey")))))
            (nostr-db-store-event
             '((id . "alice-follows")
               (pubkey . "alice-pubkey")
               (created_at . 102)
               (kind . 3)
               (tags . (("p" "bob-pubkey") ("p" "carol-pubkey")))))
            (nostr-db-store-event
             '((id . "bob-follows")
               (pubkey . "bob-pubkey")
               (created_at . 103)
               (kind . 3)
               (tags . (("p" "alice-pubkey")))))
            (nostr-db-store-event
             '((id . "alice-relays")
               (pubkey . "alice-pubkey")
               (created_at . 104)
               (kind . 10002)
               (tags . (("r" "wss://read.example" "read")
                        ("r" "wss://write.example" "write")
                        ("r" "wss://both.example")))
               (content . "")
               (sig . "sig")))
            (with-temp-buffer
              (nostr-profile-mode)
              (setq-local nostr-profile-pubkey "alice-pubkey")
              (nostr-profile-refresh)
              (let ((text (buffer-string)))
                (should (string-match-p "You follow yes" text))
                (should (string-match-p "Muted      yes" text))
                (should (string-match-p "Followers  2" text))
                (should (string-match-p "Following  2" text))
                (should (string-match-p "Relays     1 read  1 write  1 both" text)))))
        (setq nostr-current-pubkey old-pubkey)))))

(ert-deftest nostr-profile-mute-and-unmute-use-profile-pubkey ()
  "Profile mute commands publish updates for the displayed account."
  (nostr-ui-buffers-test-with-db
    (let (actions)
      (cl-letf (((symbol-function 'nostr-actions-mute)
                 (lambda (pubkey) (push (list 'mute pubkey) actions)))
                ((symbol-function 'nostr-actions-unmute)
                 (lambda (pubkey) (push (list 'unmute pubkey) actions))))
        (with-temp-buffer
          (nostr-profile-mode)
          (setq-local nostr-profile-pubkey "alice-pubkey")
          (nostr-profile-mute)
          (nostr-profile-unmute)))
      (should (equal (nreverse actions)
                     '((mute "alice-pubkey")
                       (unmute "alice-pubkey")))))))

(ert-deftest nostr-profile-verify-nip05-uses-cached-profile ()
  "Profile verification command uses cached NIP-05 metadata."
  (nostr-ui-buffers-test-with-db
    (clrhash nostr-ui--verified-nip05-cache)
    (nostr-ui-buffers-test-store-event
     '((id . "profile-event")
       (pubkey . "alice-pubkey")
       (created_at . 100)
       (kind . 0)
       (tags . nil)
       (content . "{\"nip05\":\"alice@example.com\"}")
       (sig . "sig")))
    (let (requested)
      (cl-letf (((symbol-function 'nostr-nip05-verify)
                 (lambda (identifier pubkey success _error)
                   (setq requested (list identifier pubkey))
                   (funcall success `((identifier . ,identifier)
                                      (pubkey . ,pubkey)
                                      (verified . t))))))
        (with-temp-buffer
          (nostr-profile-mode)
          (setq-local nostr-profile-pubkey "alice-pubkey")
          (nostr-profile-verify-nip05)))
      (should (equal requested '("alice@example.com" "alice-pubkey")))
      (should (nostr-ui-nip05-verified-p "alice-pubkey" "alice@example.com")))))

(ert-deftest nostr-search-renders-local-results ()
  "Search buffers show local DB results and keep point identity."
  (nostr-ui-buffers-test-with-db
    (nostr-ui-buffers-test-store-event
     '((id . "profile-event")
       (pubkey . "bob-pubkey")
       (created_at . 100)
       (kind . 0)
       (tags . nil)
       (content . "{\"display_name\":\"Bob\"}")
       (sig . "sig")))
    (nostr-ui-buffers-test-store-event
     '((id . "note-2")
       (pubkey . "bob-pubkey")
       (created_at . 120)
       (kind . 1)
       (tags . nil)
       (content . "searchable nostr note")
       (sig . "sig")))
    (with-temp-buffer
      (nostr-search-mode)
      (setq-local nostr-search-query "searchable")
      (nostr-search-refresh)
      (should (equal major-mode 'nostr-search-mode))
      (should (string-match-p "Search  searchable" (buffer-string)))
      (should (string-match-p "searchable nostr note" (buffer-string)))
      (goto-char (point-min))
      (search-forward "searchable nostr note")
      (should (equal (alist-get 'id (nostr-ui-selected-data)) "note-2")))))

(ert-deftest nostr-ui-note-renders-publish-receipts ()
  "Note rendering shows cached per-relay publish receipt status."
  (nostr-ui-buffers-test-with-db
    (nostr-ui-buffers-test-store-event
     '((id . "published-note")
       (pubkey . "me")
       (created_at . 120)
       (kind . 1)
       (tags . nil)
       (content . "published locally")
       (sig . "sig")))
    (nostr-db-store-publish-receipt
     "published-note" "wss://relay-a.example" "accepted" "stored")
    (nostr-db-store-publish-receipt
     "published-note" "wss://relay-b.example" "rejected" "blocked")
    (with-temp-buffer
      (nostr-ui-insert-note
       '((id . "published-note")
         (pubkey . "me")
         (created-at . 120)
         (content . "published locally")))
      (let ((text (buffer-string)))
        (should (string-match-p "⚡ 0.*✓ 1.*! 1" text))
        (let ((case-fold-search nil))
          (should-not (string-match-p "Publish" text)))
        (should-not (string-match-p "accepted:1" text))
        (should-not (string-match-p "pending" text))))))

(ert-deftest nostr-ui-note-hides-publish-receipts-for-other-authors ()
  "Publish receipts are only rendered for the current account's own notes."
  (nostr-ui-buffers-test-with-db
    (let ((nostr-current-pubkey "me"))
      (nostr-ui-buffers-test-store-event
       '((id . "other-note")
         (pubkey . "other")
         (created_at . 120)
         (kind . 1)
         (tags . nil)
         (content . "not mine")
         (sig . "sig")))
      (nostr-db-store-publish-receipt
       "other-note" "wss://relay-a.example" "accepted" "stored")
      (with-temp-buffer
        (nostr-ui-insert-note
         '((id . "other-note")
           (pubkey . "other")
           (created-at . 120)
           (content . "not mine")))
        (should-not (string-match-p "Publish" (buffer-string)))))))

(ert-deftest nostr-ui-publish-details-renders-relay-messages ()
  "Publish details include relay URLs, states, and messages."
  (let ((text (nostr-ui--publish-details-text
               '((id . "published-note") (pubkey . "me"))
               '(((url . "wss://relay-a.example")
                  (state . "rejected")
                  (message . "blocked")
                  (updated-at . 120))))))
    (should (string-match-p "Publish details for published-note" text))
    (should (string-match-p "rejected" text))
    (should (string-match-p "wss://relay-a.example" text))
    (should (string-match-p "blocked" text))))

(provide 'nostr-ui-buffers-test)
;;; nostr-ui-buffers-test.el ends here
