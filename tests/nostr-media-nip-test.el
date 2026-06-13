;;; nostr-media-nip-test.el --- Media and NIP helper tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'button)
(require 'nostr-media)
(require 'nostr-nip)

(ert-deftest nostr-media-loads-placeholder-into-cache ()
  "Media placeholders can load an image and render cache metadata."
  (let* ((dir (make-temp-file "nostr-media-test" t))
         (url "https://example.com/picture.png")
         (nostr-media-cache-directory dir)
         (nostr-media-max-bytes 100)
         (nostr-media-fetch-function
          (lambda (requested-url success _error)
            (should (equal requested-url url))
            (funcall success
                     '(("content-type" . "image/png")
                       ("content-length" . "8"))
                     "PNGDATA!"))))
    (unwind-protect
        (with-temp-buffer
          (insert-text-button "[image]"
                              'nostr-media-url url
                              'follow-link t)
          (goto-char (point-min))
          (should (equal (nostr-media-load-at-point) url))
          (should (string-match-p "\\[image loaded:" (buffer-string)))
          (should (file-exists-p (nostr-media-cache-file url)))
          (search-forward "[image loaded:")
          (should (equal (get-text-property (point) 'nostr-media-url) url))
          (should (equal (get-text-property (point) 'nostr-media-cache-file)
                         (nostr-media-cache-file url))))
      (delete-directory dir t))))

(ert-deftest nostr-media-rejects-oversized-downloads ()
  "Media downloads over the size guard are not cached or rendered."
  (let* ((dir (make-temp-file "nostr-media-test" t))
         (url "https://example.com/large.jpg")
         (nostr-media-cache-directory dir)
         (nostr-media-max-bytes 4)
         (nostr-media-fetch-function
          (lambda (_requested-url success _error)
            (funcall success
                     '(("content-type" . "image/jpeg")
                       ("content-length" . "9"))
                     "too-large"))))
    (unwind-protect
        (with-temp-buffer
          (insert-text-button "[image]"
                              'nostr-media-url url
                              'follow-link t)
          (goto-char (point-min))
          (nostr-media-load-at-point)
          (should-not (file-exists-p (nostr-media-cache-file url)))
          (should-not (string-match-p "\\[image loaded:" (buffer-string))))
      (delete-directory dir t))))

(ert-deftest nostr-media-cached-only-does-not-fetch ()
  "Cached-only media rendering never starts a network fetch."
  (let* ((dir (make-temp-file "nostr-media-cached-only-test" t))
         (url "https://example.com/missing.png")
         (nostr-media-cache-directory dir)
         (nostr-media-fetch-function
          (lambda (&rest _)
            (ert-fail "cached-only media render should not fetch"))))
    (unwind-protect
        (with-temp-buffer
          (insert-text-button "[image]"
                              'nostr-media-url url
                              'follow-link t)
          (goto-char (point-min))
          (should-not (nostr-media-load-at-point t))
          (should-not (file-exists-p (nostr-media-cache-file url)))
          (should-not (string-match-p "\\[image loaded:" (buffer-string))))
      (delete-directory dir t))))

(ert-deftest nostr-nip05-verifies-matching-pubkey ()
  "NIP-05 verification succeeds when the names map contains the pubkey."
  (let ((nostr-nip05-fetch-function
         (lambda (url success _error)
           (should (equal url "https://example.com/.well-known/nostr.json?name=alice"))
           (funcall success "{\"names\":{\"alice\":\"pubkey-1\"}}"))))
    (let ((result (nostr-nip05-verify-sync "alice@example.com" "pubkey-1")))
      (should (alist-get 'verified result))
      (should (equal (alist-get 'resolved-pubkey result) "pubkey-1")))))

(ert-deftest nostr-nip05-reports-mismatch ()
  "NIP-05 verification returns false for a mismatched pubkey."
  (let ((nostr-nip05-fetch-function
         (lambda (_url success _error)
           (funcall success "{\"names\":{\"alice\":\"pubkey-2\"}}"))))
    (let ((result (nostr-nip05-verify-sync "alice@example.com" "pubkey-1")))
      (should-not (alist-get 'verified result))
      (should (equal (alist-get 'resolved-pubkey result) "pubkey-2")))))

(ert-deftest nostr-nip19-sync-wrappers-use-backend-protocol ()
  "NIP-19 sync helpers pass the expected command payloads to the backend."
  (cl-letf (((symbol-function 'nostr-backend-call-sync)
             (lambda (command payload)
               (pcase command
                 ("nip19-decode"
                  (should (equal payload '((value . "npub1test"))))
                  (cons 0 '((ok . t) (entity . "npub") (pubkey . "pubkey-1"))))
                 ("nip19-encode"
                  (should (equal payload '((entity . "npub") (value . "pubkey-1"))))
                  (cons 0 '((ok . t) (value . "npub1test"))))
                 (_ (error "unexpected command: %s" command))))))
    (should (equal (alist-get 'pubkey (nostr-nip19-decode-sync "npub1test"))
                   "pubkey-1"))
    (should (equal (alist-get 'value (nostr-nip19-encode-sync "npub" "pubkey-1"))
                   "npub1test"))))

(ert-deftest nostr-nip19-async-wrappers-use-backend-protocol ()
  "NIP-19 async helpers call the backend with the expected payloads."
  (let (decoded encoded)
    (cl-letf (((symbol-function 'nostr-backend-call)
               (lambda (command payload success _error)
                 (pcase command
                   ("nip19-decode"
                    (should (equal payload '((value . "note1test"))))
                    (funcall success '((ok . t) (entity . "note") (event_id . "event-1"))))
                   ("nip19-encode"
                    (should (equal payload '((entity . "note") (value . "event-1"))))
                    (funcall success '((ok . t) (value . "note1test"))))
                   (_ (error "unexpected command: %s" command))))))
      (nostr-nip19-decode "note1test"
                          (lambda (response) (setq decoded response))
                          #'ignore)
      (nostr-nip19-encode "note" "event-1"
                          (lambda (response) (setq encoded response))
                          #'ignore))
    (should (equal (alist-get 'event_id decoded) "event-1"))
    (should (equal (alist-get 'value encoded) "note1test"))))

(provide 'nostr-media-nip-test)
;;; nostr-media-nip-test.el ends here
