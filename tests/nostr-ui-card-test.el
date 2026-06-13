;;; nostr-ui-card-test.el --- Shared note card rendering tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for shared Nostr note card formatting.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'nostr-ui)

(ert-deftest nostr-ui-formats-relative-dates ()
  "Feed dates are compact and tolerate future timestamps."
  (should (equal (nostr-ui-format-relative-time 1000 1000) "just now"))
  (should (equal (nostr-ui-format-relative-time 941 1000) "just now"))
  (should (equal (nostr-ui-format-relative-time 940 1000) "1 minute ago"))
  (should (equal (nostr-ui-format-relative-time (- 1000 (* 59 60)) 1000)
                 "59 minutes ago"))
  (should (equal (nostr-ui-format-relative-time (- 1000 (* 3 86400)) 1000)
                 "3 days ago"))
  (should (equal (nostr-ui-format-relative-time 1030 1000) "just now"))
  (should (equal (nostr-ui-format-relative-time 1200 1000) "in 3 minutes")))

(ert-deftest nostr-ui-formats-exact-dates ()
  "Detail/thread dates use exact long timestamps."
  (let ((timestamp (float-time (encode-time 0 0 14 13 1 2025))))
    (should (equal (nostr-ui-format-exact-time timestamp)
                   "13 January 2025 at 14:00"))))

(ert-deftest nostr-ui-author-prefers-nip05-and-caches-npub ()
  "Author formatting uses NIP-05 first, then cached npub, then hex fallback."
  (clrhash nostr-ui--npub-cache)
  (should (equal (nostr-ui-format-author
                  '((pubkey . "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                    (author . "Alice")
                    (nip05 . "alice@example.test")))
                 "Alice · alice@example.test"))
  (let ((calls 0))
    (cl-letf (((symbol-function 'nostr-nip19-encode-sync)
               (lambda (entity value)
                 (setq calls (1+ calls))
                 (should (equal entity "npub"))
                 (should (equal value "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))
                 '((ok . t)
                   (value . "npub1bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")))))
      (should (equal (nostr-ui-format-author
                      '((pubkey . "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")))
                     "npub1bbb...bbbbbbbb"))
      (should (equal (nostr-ui-format-author
                      '((pubkey . "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")))
                     "npub1bbb...bbbbbbbb"))
      (should (= calls 1))))
  (should (equal (nostr-ui-format-author
                  '((pubkey . "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")))
                 "cccccccc...cccccccc")))

(ert-deftest nostr-ui-avatar-image-props-center-inline ()
  "Avatar image props vertically center the image on the author line."
  (should (equal (plist-get (nostr-ui--image-display-props 32) :ascent)
                 nostr-ui-avatar-ascent))
  (should (> nostr-ui-avatar-ascent 50)))

(ert-deftest nostr-ui-note-card-renders-content-footer-and-selection ()
  "Note cards preserve section identity and render compact metadata."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (nostr-ui-clear)
      (nostr-ui-insert-note
       '((id . "note-card")
         (pubkey . "alice-pubkey")
         (author . "Alice")
         (nip05 . "alice@example.test")
         (created-at . 1736776800)
         (content . "hello from a card")
         (replies . 2)
         (reactions . 5)
         (reposts . 3)
         (zaps . 4)
         (zap-sats . 21000))
       '(:style exact)))
    (nostr-ui-goto-first-section)
    (should (equal (alist-get 'id (nostr-ui-selected-data)) "note-card"))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Alice · alice@example.test" text))
      (should (= (how-many "Alice · alice@example.test" (point-min) (point-max)) 1))
      (should (string-match-p "\\[Ostrich\\]" text))
      (should (string-match-p "hello from a card" text))
      (should (string-match-p "↩ 2   ♥ 5   ↻ 3   ⚡ 4 (21000 sats)" text)))
    (nostr-ui-toggle-section)
    (should (nostr-ui-section-folded (nostr-ui-section-at-point)))))

(ert-deftest nostr-ui-note-card-renders-avatar-button ()
  "Cards render a loadable avatar when an author picture URL is available."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (nostr-ui-clear)
      (nostr-ui-insert-note
       '((id . "note-avatar")
         (pubkey . "alice-pubkey")
         (author . "Alice")
         (picture . "https://example.test/alice.png")
         (created-at . 1736776800)
         (content . "with avatar")
         (replies . 0)
         (reactions . 0)
         (reposts . 0))))
    (goto-char (point-min))
    (search-forward "[Avatar]")
    (should (save-excursion
              (beginning-of-line)
              (looking-at-p "▾ .*\\[Avatar\\] Alice")))
    (let ((button (button-at (1- (point)))))
      (should button)
      (should (equal (button-get button 'nostr-media-url)
                     "https://example.test/alice.png")))))

(ert-deftest nostr-ui-avatar-load-replaces-placeholder-in-place ()
  "Loading an avatar replaces its button instead of appending repeated images."
  (let* ((dir (make-temp-file "nostr-avatar-test" t))
         (url "https://example.test/alice.png")
         (file nil)
         (nostr-media-cache-directory dir)
         (nostr-media-fetch-function
          (lambda (_url success _error)
            (funcall success
                     '(("content-type" . "image/png")
                       ("content-length" . "8"))
                     "png-data"))))
    (unwind-protect
        (with-temp-buffer
          (let ((inhibit-read-only t))
            (nostr-ui-clear)
            (nostr-ui-insert-avatar url 18))
          (goto-char (point-min))
          (search-forward "[Avatar]")
          (should (equal (nostr-ui-load-avatar-at-point) url))
          (setq file (nostr-media-cache-file url))
          (should (file-exists-p file))
          (goto-char (point-min))
          (should (get-text-property (point) 'nostr-media-cache-file))
          (should-not (button-at (point)))
          (should-error (nostr-ui-load-avatar-at-point) :type 'user-error)
          (should (= (how-many "\\[Avatar\\]" (point-min) (point-max)) 1)))
      (when (and file (file-exists-p file))
        (delete-file file))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest nostr-ui-note-card-toggles-media-previews ()
  "The note-level media command loads and removes rendered previews."
  (let* ((dir (make-temp-file "nostr-note-media-test" t))
         (url "https://example.test/photo.png")
         (nostr-media-cache-directory dir)
         (nostr-media-fetch-function
          (lambda (_url success _error)
            (funcall success
                     '(("content-type" . "image/png")
                       ("content-length" . "8"))
                     "png-data"))))
    (unwind-protect
        (with-temp-buffer
          (let ((inhibit-read-only t))
            (nostr-ui-clear)
            (nostr-ui-insert-note
             `((id . "note-media")
               (pubkey . "alice")
               (created-at . 1736776800)
               (content . ,(format "look %s" url))
               (replies . 0)
               (reactions . 0)
               (reposts . 0))))
          (nostr-ui-goto-first-section)
          (should (string-match-p "\\[image: https://example.test/photo.png\\]"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))
          (nostr-ui-toggle-note-media)
          (should (file-exists-p (nostr-media-cache-file url)))
          (should (text-property-any (point-min) (point-max)
                                     'nostr-media-rendered t))
          (nostr-ui-toggle-note-media)
          (should-not (text-property-any (point-min) (point-max)
                                         'nostr-media-rendered t))
          (should (= (how-many "\\[image loaded:" (point-min) (point-max)) 0))
          (should (= (how-many "\\[image: https://example.test/photo.png\\]"
                               (point-min) (point-max))
                     1))
          (should (string-match-p "\\[image: https://example.test/photo.png\\]"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max)))))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest nostr-ui-note-card-wraps-long-body-text ()
  "Card body text is filled like `fill-paragraph' instead of one long line."
  (let ((nostr-ui-card-fill-column 34))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (nostr-ui-clear)
        (nostr-ui-insert-note
         '((id . "note-wrap")
           (pubkey . "alice")
           (created-at . 1736776800)
           (content . "This is a long note body that should wrap inside the card instead of running across the whole window.")
           (replies . 0)
           (reactions . 0)
           (reposts . 0))))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "This is a long note body that\n  should wrap inside the card" text))
        (dolist (line (split-string text "\n" t))
          (when (string-match-p "long note body\\|should wrap inside\\|whole window" line)
            (should (<= (length line) nostr-ui-card-fill-column))))))))

(ert-deftest nostr-ui-note-card-preserves-depth-argument ()
  "Numeric second argument still indents thread cards."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (nostr-ui-clear)
      (nostr-ui-insert-note
       '((id . "reply")
         (pubkey . "alice")
         (created-at . 1736776800)
         (content . "indented reply")
         (replies . 0)
         (reactions . 0)
         (reposts . 0))
       2))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "^    ▾ \\[Ostrich\\] alice" text)))))

(ert-deftest nostr-ui-default-avatar-renders-without-picture ()
  "Cards render the checked-in default avatar when no profile picture is known."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (nostr-ui-show-avatars t))
      (nostr-ui-clear)
      (nostr-ui-insert-note
       '((id . "note-default-avatar")
         (pubkey . "alice")
         (created-at . 1736776800)
         (content . "default avatar")
         (replies . 0)
         (reactions . 0)
         (reposts . 0))))
    (should (string-match-p "\\[Ostrich\\]"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))

(provide 'nostr-ui-card-test)
;;; nostr-ui-card-test.el ends here
