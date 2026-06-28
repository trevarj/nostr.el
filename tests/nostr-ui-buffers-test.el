;;; nostr-ui-buffers-test.el --- Tests for Nostr UI buffers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'nostr-db)
(require 'nostr-profile)
(require 'nostr-reactions)
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
       (content . "{\"name\":\"bob\",\"display_name\":\"Bob Example\",\"about\":\"Builder of useful Nostr tools.\",\"picture\":\"https://example.test/bob.png\",\"nip05\":\"bob@example.test\"}")
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
            (should (string-match-p "\\[Avatar\\]" text))
            (should (string-match-p "Bob Example" text))
            (should (string-match-p "@bob" text))
            (should (string-match-p "bob@example.test" text))
            (should (string-match-p "1 follower" text))
            (should (string-match-p "0 following" text))
            (should (string-match-p "Builder of useful Nostr tools" text))
            (should (string-match-p "TAB toggle" text))
            (should (string-match-p "\\? actions" text))
            (should-not (string-match-p "Pubkey     bob-pubkey" text)))
          (goto-char (point-min))
          (search-forward "Bob Example")
          (nostr-profile-list-open-at-point)))
      (should (equal opened "bob-pubkey")))))

(ert-deftest nostr-profile-list-open-without-selection-errors ()
  "Profile list open reports an actionable error when no profile is selected."
  (with-temp-buffer
    (nostr-profile-list-mode)
    (should-error (nostr-profile-list-open-at-point) :type 'user-error)))

(ert-deftest nostr-profile-open-at-point-without-selection-errors ()
  "Profile open reports an actionable error when no item is selected."
  (with-temp-buffer
    (nostr-profile-mode)
    (should-error (nostr-profile-open-at-point) :type 'user-error)))

(ert-deftest nostr-profile-list-refresh-preserves-selected-profile ()
  "Profile list refreshes keep point on the selected profile row."
  (nostr-ui-buffers-test-with-db
    (nostr-ui-buffers-test-store-event
     '((id . "bob-profile")
       (pubkey . "bob-pubkey")
       (created_at . 90)
       (kind . 0)
       (tags . nil)
       (content . "{\"display_name\":\"Bob Example\"}")
       (sig . "sig")))
    (nostr-ui-buffers-test-store-event
     '((id . "carol-profile")
       (pubkey . "carol-pubkey")
       (created_at . 91)
       (kind . 0)
       (tags . nil)
       (content . "{\"display_name\":\"Carol Example\"}")
       (sig . "sig")))
    (nostr-db-store-event
     '((id . "alice-follows")
       (pubkey . "alice-pubkey")
       (created_at . 100)
       (kind . 3)
       (tags . (("p" "bob-pubkey") ("p" "carol-pubkey")))))
    (cl-letf (((symbol-function 'nostr-relay-fetch-profile) (lambda (&rest _) nil)))
      (with-temp-buffer
        (nostr-profile-list-mode)
        (setq-local nostr-profile-list-owner-pubkey "alice-pubkey")
        (setq-local nostr-profile-list-kind 'following)
        (nostr-profile-list-refresh)
        (goto-char (point-min))
        (search-forward "Carol Example")
        (should (equal (alist-get 'pubkey (nostr-ui-selected-data))
                       "carol-pubkey"))
        (nostr-profile-list-refresh)
        (should (equal (alist-get 'pubkey (nostr-ui-selected-data))
                       "carol-pubkey"))))))

(ert-deftest nostr-reactions-renders-and-opens-selected-profile ()
  "Reaction buffers render cached reactor profile cards."
  (nostr-ui-buffers-test-with-db
    (nostr-ui-buffers-test-store-event
     '((id . "target")
       (pubkey . "alice-pubkey")
       (created_at . 100)
       (kind . 1)
       (tags . nil)
       (content . "note")
       (sig . "sig")))
    (nostr-ui-buffers-test-store-event
     '((id . "bob-profile")
       (pubkey . "bob-pubkey")
       (created_at . 101)
       (kind . 0)
       (tags . nil)
       (content . "{\"name\":\"bob\",\"display_name\":\"Bob Example\",\"picture\":\"https://example.test/bob.png\",\"nip05\":\"bob@example.test\"}")
       (sig . "sig")))
    (nostr-db-store-event
     '((id . "reaction-1")
       (pubkey . "bob-pubkey")
       (created_at . 102)
       (kind . 7)
       (tags . (("e" "target")))
       (content . "+")
       (sig . "sig")))
    (let (opened fetched)
      (cl-letf (((symbol-function 'nostr-relay-fetch-profile)
                 (lambda (pubkey) (push pubkey fetched)))
                ((symbol-function 'nostr-profile-open)
                 (lambda (pubkey) (setq opened pubkey))))
        (with-temp-buffer
          (nostr-reactions-mode)
          (setq-local nostr-reactions-event
                      '((id . "target") (pubkey . "alice-pubkey")))
          (nostr-reactions-refresh)
          (let ((text (buffer-string)))
            (should (string-match-p "\\[Nostr\\]  Reactions" text))
            (should (string-match-p "1 cached reaction" text))
            (should (string-match-p "♥" text))
            (should (string-match-p "\\[Avatar\\]" text))
            (should (string-match-p "Bob Example" text))
            (should (string-match-p "@bob" text))
            (should (string-match-p "bob@example.test" text))
            (should (string-match-p "TAB toggle" text))
            (should (string-match-p "\\? actions" text)))
          (goto-char (point-min))
          (search-forward "Bob Example")
          (nostr-reactions-open-profile)))
      (should (equal fetched '("bob-pubkey")))
      (should (equal opened "bob-pubkey")))))

(ert-deftest nostr-reactions-open-profile-without-selection-errors ()
  "Reaction profile open reports an actionable error when no reactor is selected."
  (with-temp-buffer
    (nostr-reactions-mode)
    (should-error (nostr-reactions-open-profile) :type 'user-error)))

(ert-deftest nostr-reactions-refresh-preserves-selected-profile ()
  "Reaction detail refreshes keep point on the selected reactor."
  (nostr-ui-buffers-test-with-db
    (nostr-ui-buffers-test-store-event
     '((id . "target")
       (pubkey . "alice-pubkey")
       (created_at . 100)
       (kind . 1)
       (tags . nil)
       (content . "note")
       (sig . "sig")))
    (nostr-ui-buffers-test-store-event
     '((id . "bob-profile")
       (pubkey . "bob-pubkey")
       (created_at . 101)
       (kind . 0)
       (tags . nil)
       (content . "{\"display_name\":\"Bob Example\"}")
       (sig . "sig")))
    (nostr-ui-buffers-test-store-event
     '((id . "carol-profile")
       (pubkey . "carol-pubkey")
       (created_at . 102)
       (kind . 0)
       (tags . nil)
       (content . "{\"display_name\":\"Carol Example\"}")
       (sig . "sig")))
    (nostr-db-store-event
     '((id . "reaction-bob")
       (pubkey . "bob-pubkey")
       (created_at . 103)
       (kind . 7)
       (tags . (("e" "target")))
       (content . "+")
       (sig . "sig")))
    (nostr-db-store-event
     '((id . "reaction-carol")
       (pubkey . "carol-pubkey")
       (created_at . 104)
       (kind . 7)
       (tags . (("e" "target")))
       (content . "🚀")
       (sig . "sig")))
    (cl-letf (((symbol-function 'nostr-relay-fetch-profile) (lambda (&rest _) nil)))
      (with-temp-buffer
        (nostr-reactions-mode)
        (setq-local nostr-reactions-event
                    '((id . "target") (pubkey . "alice-pubkey")))
        (nostr-reactions-refresh)
        (goto-char (point-min))
        (search-forward "Bob Example")
        (should (equal (alist-get 'pubkey (nostr-ui-selected-data))
                       "bob-pubkey"))
        (nostr-reactions-refresh)
        (should (equal (alist-get 'pubkey (nostr-ui-selected-data))
                       "bob-pubkey"))))))

(ert-deftest nostr-reactions-open-uses-temporary-buffer ()
  "Reaction popups use hidden buffers and `q' kills them."
  (nostr-ui-buffers-test-with-db
    (let (opened)
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (buffer &optional _action)
                   (setq opened buffer)
                   (display-buffer-same-window buffer nil))))
        (nostr-reactions-open '((id . "target") (pubkey . "alice-pubkey"))))
      (unwind-protect
          (progn
            (should (buffer-live-p opened))
            (should (string-prefix-p " " (buffer-name opened)))
            (should (eq (lookup-key nostr-reactions-mode-map (kbd "q"))
                        #'nostr-reactions-quit))
            (with-current-buffer opened
              (should (eq major-mode 'nostr-reactions-mode))))
        (when (buffer-live-p opened)
          (kill-buffer opened))))))

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

(ert-deftest nostr-profile-open-self-uses-current-pubkey ()
  "The self-profile command opens the active local account profile."
  (let ((nostr-current-pubkey "me-pubkey")
        opened)
    (cl-letf (((symbol-function 'nostr-profile-open)
               (lambda (pubkey) (setq opened pubkey))))
      (nostr-profile-open-self))
    (should (equal opened "me-pubkey"))))

(ert-deftest nostr-profile-edit-requires-own-profile ()
  "Profile editing is only available on the current account's profile."
  (let ((nostr-current-pubkey "me-pubkey"))
    (with-temp-buffer
      (nostr-profile-mode)
      (setq-local nostr-profile-pubkey "alice-pubkey")
      (should-error (nostr-profile-edit) :type 'user-error))))

(ert-deftest nostr-profile-edit-metadata-preserves-unknown-raw-fields ()
  "Publishing common fields preserves unsupported cached metadata keys."
  (let* ((profile '((pubkey . "me-pubkey")
                    (name . "old")
                    (display-name . "Old")
                    (picture . "https://example.test/old.png")
                    (raw-content . "{\"name\":\"old\",\"displayName\":\"Old Alias\",\"website\":\"https://example.test\",\"bot\":false}")))
         (json (nostr-profile-edit--metadata-json
                profile
                '((name . "new")
                  (display_name . "New Display")
                  (nip05 . "")
                  (lud16 . "tips@example.test")
                  (picture . "")
                  (about . "hello"))))
         (metadata (json-parse-string json :object-type 'alist :array-type 'list
                                      :false-object json-false)))
    (should (equal (alist-get 'name metadata) "new"))
    (should (equal (alist-get 'display_name metadata) "New Display"))
    (should (equal (alist-get 'lud16 metadata) "tips@example.test"))
    (should (equal (alist-get 'about metadata) "hello"))
    (should (equal (alist-get 'website metadata) "https://example.test"))
    (should (eq (alist-get 'bot metadata) json-false))
    (should-not (assq 'displayName metadata))
    (should-not (assq 'picture metadata))))

(ert-deftest nostr-profile-edit-reconstructs-common-fields-without-raw-content ()
  "Older cached profiles can still publish common metadata fields."
  (let* ((profile '((pubkey . "me-pubkey")
                    (name . "old")
                    (display-name . "Old")
                    (nip05 . "old@example.test")
                    (raw-content . nil)))
         (json (nostr-profile-edit--metadata-json
                profile
                '((name . "old")
                  (display_name . "Old")
                  (nip05 . "")
                  (lud16 . "")
                  (picture . "")
                  (about . ""))))
         (metadata (json-parse-string json :object-type 'alist :array-type 'list)))
    (should (equal (alist-get 'name metadata) "old"))
    (should (equal (alist-get 'display_name metadata) "Old"))
    (should-not (assq 'nip05 metadata))))

(ert-deftest nostr-profile-edit-validates-obvious-bad-fields ()
  "Profile edit validation rejects malformed URL and address fields."
  (should-error
   (nostr-profile-edit--metadata-json
    '((pubkey . "me-pubkey"))
    '((name . "") (display_name . "") (nip05 . "") (lud16 . "")
      (picture . "ftp://example.test/avatar.png") (about . "")))
   :type 'user-error)
  (should-error
   (nostr-profile-edit--metadata-json
    '((pubkey . "me-pubkey"))
    '((name . "") (display_name . "") (nip05 . "not-an-address") (lud16 . "")
      (picture . "") (about . "")))
   :type 'user-error))

(ert-deftest nostr-profile-edit-avatar-upload-fills-picture-field ()
  "Avatar attach uploads through the shared helper and fills Picture."
  (let ((file "/tmp/avatar.png"))
    (with-temp-buffer
      (nostr-profile-edit-mode)
      (nostr-profile-edit--insert-form '((pubkey . "me-pubkey")))
      (setq nostr-profile-edit-dirty nil)
      (cl-letf (((symbol-function 'nostr-upload-file)
                 (lambda (selected success _error)
                   (should (equal selected file))
                   (funcall success selected "https://cdn.example/avatar.png"))))
        (nostr-profile-edit-attach-avatar file))
      (should (string-match-p "^Picture: https://cdn.example/avatar.png$"
                              (buffer-string)))
      (should nostr-profile-edit-dirty))))

(ert-deftest nostr-profile-edit-publish-closes-and-refreshes-target ()
  "Publishing signs kind-0 metadata, closes the editor, and refreshes target."
  (let (published-json refreshed)
    (with-temp-buffer
      (nostr-profile-mode)
      (setq-local nostr-profile-pubkey "me-pubkey")
      (let ((target (current-buffer))
            (editor (generate-new-buffer " *nostr-profile-edit-test*")))
        (unwind-protect
            (progn
              (with-current-buffer editor
                (nostr-profile-edit-mode)
                (setq-local nostr-profile-edit-original
                            '((pubkey . "me-pubkey")
                              (raw-content . "{\"website\":\"https://example.test\"}")))
                (setq-local nostr-profile-edit-target-buffer target)
                (nostr-profile-edit--insert-form '((name . "Alice")))
                (nostr-profile-edit--set-field "Name" "alice")
                (cl-letf (((symbol-function 'nostr-actions-publish-profile)
                           (lambda (json after-send &optional _after-error)
                             (setq published-json json)
                             (funcall after-send '((id . "profile-event")))))
                          ((symbol-function 'nostr-profile-refresh)
                           (lambda () (setq refreshed t))))
                  (nostr-profile-edit-publish)))
              (should (not (buffer-live-p editor)))
              (should refreshed)
              (should (string-match-p "\"name\":\"alice\"" published-json))
              (should (string-match-p "\"website\":\"https://example.test\"" published-json)))
          (when (buffer-live-p editor)
            (kill-buffer editor)))))))

(ert-deftest nostr-profile-open-requests-recent-author-events ()
  "Opening a profile requests recent author activity from relays."
  (nostr-ui-buffers-test-with-db
    (let ((nostr-relay-search-author-urls '("wss://relay.primal.net"))
          fetched
          fetched-extra
          opened)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'nostr-relay-fetch-author)
                       (lambda (pubkey &optional limit)
                         (push (list pubkey limit) fetched)
                         "author-sub"))
                      ((symbol-function 'nostr-relay-fetch-author-from-urls)
                       (lambda (pubkey &optional limit urls track-progress)
                         (push (list pubkey limit urls track-progress) fetched-extra)
                         "author-extra-sub"))
                      ((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _args)
                         (setq opened buffer)
                         buffer)))
              (nostr-profile-open "alice-pubkey"))
            (should (equal fetched '(("alice-pubkey" 50))))
            (should (equal fetched-extra
                           '(("alice-pubkey" 50 ("wss://relay.primal.net") nil)))))
        (when (buffer-live-p opened)
          (kill-buffer opened))))))

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

(ert-deftest nostr-search-open-without-selection-errors ()
  "Search open reports an actionable error when no result is selected."
  (with-temp-buffer
    (nostr-search-mode)
    (should-error (nostr-search-open-at-point) :type 'user-error)))

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
