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

(ert-deftest nostr-profile-social-count-buttons-open-lists ()
  "Follower and following counts open cached profile relationship lists."
  (nostr-ui-buffers-test-with-db
    (nostr-db-store-event
     '((id . "alice-follows")
       (pubkey . "alice-pubkey")
       (created_at . 100)
       (kind . 3)
       (tags . (("p" "bob-pubkey")))))
    (nostr-db-store-event
     '((id . "bob-follows")
       (pubkey . "bob-pubkey")
       (created_at . 101)
       (kind . 3)
       (tags . (("p" "alice-pubkey")))))
    (let (opened)
      (cl-letf (((symbol-function 'nostr-profile-open-list)
                 (lambda (pubkey kind) (push (list pubkey kind) opened))))
        (with-temp-buffer
          (nostr-profile-mode)
          (setq-local nostr-profile-pubkey "alice-pubkey")
          (nostr-profile-refresh)
          (goto-char (point-min))
          (search-forward "Followers")
          (search-forward "1")
          (button-activate (or (button-at (point))
                               (button-at (1- (point)))))
          (goto-char (point-min))
          (search-forward "Following")
          (search-forward "1")
          (button-activate (or (button-at (point))
                               (button-at (1- (point)))))))
      (should (equal (nreverse opened)
                     '(("alice-pubkey" followers)
                       ("alice-pubkey" following)))))))

(ert-deftest nostr-profile-list-renders-and-opens-selected-profile ()
  "Profile list buffers render cached relations and open selected rows."
  (nostr-ui-buffers-test-with-db
    (nostr-ui-buffers-test-store-event
     '((id . "bob-profile")
       (pubkey . "bob-pubkey")
       (created_at . 90)
       (kind . 0)
       (tags . nil)
       (content . "{\"display_name\":\"Bob\",\"nip05\":\"bob@example.test\"}")
       (sig . "sig")))
    (nostr-db-store-event
     '((id . "alice-follows")
       (pubkey . "alice-pubkey")
       (created_at . 100)
       (kind . 3)
       (tags . (("p" "bob-pubkey")))))
    (let (opened)
      (cl-letf (((symbol-function 'nostr-relay-fetch-profile) (lambda (&rest _) nil))
                ((symbol-function 'nostr-profile-open)
                 (lambda (pubkey) (setq opened pubkey))))
        (with-temp-buffer
          (nostr-profile-list-mode)
          (setq-local nostr-profile-list-owner-pubkey "alice-pubkey")
          (setq-local nostr-profile-list-kind 'following)
          (nostr-profile-list-refresh)
          (let ((text (buffer-string)))
            (should (string-match-p "\\[Nostr\\]  Following" text))
            (should (string-match-p "Bob" text))
            (should (string-match-p "bob@example.test" text)))
          (goto-char (point-min))
          (search-forward "Bob")
          (nostr-profile-list-open-at-point)))
      (should (equal opened "bob-pubkey")))))

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

(ert-deftest nostr-profile-follow-and-unfollow-use-profile-pubkey ()
  "Profile follow commands publish updates for the displayed account."
  (nostr-ui-buffers-test-with-db
    (let ((refreshed 0)
          actions)
      (cl-letf (((symbol-function 'nostr-actions-follow)
                 (lambda (pubkey &optional after-send)
                   (push (list 'follow pubkey) actions)
                   (when after-send (funcall after-send '((kind . 3))))))
                ((symbol-function 'nostr-actions-unfollow)
                 (lambda (pubkey &optional after-send)
                   (push (list 'unfollow pubkey) actions)
                   (when after-send (funcall after-send '((kind . 3))))))
                ((symbol-function 'nostr-profile-refresh)
                 (lambda () (cl-incf refreshed))))
        (with-temp-buffer
          (nostr-profile-mode)
          (setq-local nostr-profile-pubkey "alice-pubkey")
          (nostr-profile-follow)
          (nostr-profile-unfollow)))
      (should (equal (nreverse actions)
                     '((follow "alice-pubkey")
                       (unfollow "alice-pubkey"))))
      (should (= refreshed 2)))))

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

(ert-deftest nostr-search-renders-profile-results ()
  "Search buffers show profile-only matches and open them from RET."
  (nostr-ui-buffers-test-with-db
    (nostr-ui-buffers-test-store-event
     '((id . "profile-event")
       (pubkey . "alice-pubkey")
       (created_at . 100)
       (kind . 0)
       (tags . nil)
       (content . "{\"name\":\"alice\",\"display_name\":\"Alice Example\",\"about\":\"Builds clients\",\"nip05\":\"alice@example.test\"}")
       (sig . "sig")))
    (let (opened)
      (cl-letf (((symbol-function 'nostr-profile-open)
                 (lambda (pubkey) (setq opened pubkey))))
        (with-temp-buffer
          (nostr-search-mode)
          (setq-local nostr-search-query "Alice")
          (nostr-search-refresh)
          (let ((text (buffer-string)))
            (should (string-match-p "1 cached result" text))
            (should (string-match-p "1 profile" text))
            (should (string-match-p "0 notes" text))
            (should (string-match-p "Profile: Alice Example" text))
            (should (string-match-p "alice@example.test" text)))
          (goto-char (point-min))
          (search-forward "Profile: Alice Example")
          (should (equal (alist-get 'pubkey (nostr-ui-selected-data))
                         "alice-pubkey"))
          (nostr-search-open-at-point)
          (should (equal opened "alice-pubkey")))))))

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
