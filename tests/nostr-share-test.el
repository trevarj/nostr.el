;;; nostr-share-test.el --- Share workflow tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch tests for copy and browse commands.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nostr-profile)
(require 'nostr-relays)
(require 'nostr-search)
(require 'nostr-share)
(require 'nostr-thread)
(require 'nostr-timeline)
(require 'nostr-ui)

(defmacro nostr-share-test-with-section (type id data &rest body)
  "Render one section of TYPE, ID, DATA, then run BODY at that section."
  (declare (indent 3))
  `(with-temp-buffer
     (nostr-ui-clear)
     (nostr-ui-with-section ,type ,id ,data "selected"
       (insert "body\n"))
     (goto-char (point-min))
     ,@body))

(defmacro nostr-share-test-with-stubs (&rest body)
  "Run BODY with share side effects captured."
  (declare (indent 0))
  `(let (copied browsed encoded)
     (ignore copied browsed encoded)
     (cl-letf (((symbol-function 'kill-new)
                (lambda (value) (setq copied value)))
               ((symbol-function 'browse-url)
                (lambda (url &rest _args) (setq browsed url)))
               ((symbol-function 'nostr-nip19-encode-sync)
                (lambda (entity value)
                  (push (list entity value) encoded)
                  `((ok . t) (value . ,(format "%s1%s" entity value))))))
       ,@body)))

(ert-deftest nostr-share-copies-and-browses-note ()
  "Note sections can be copied as note1 and opened in a web client."
  (nostr-share-test-with-stubs
    (let ((nostr-share-web-client-base-url "https://client.example/"))
      (nostr-share-test-with-section
          'note "event1" '((id . "event1") (pubkey . "author1"))
        (should (equal (nostr-share-copy-note-id) "note1event1"))
        (should (equal copied "note1event1"))
        (should (equal encoded '(("note" "event1"))))
        (setq encoded nil)
        (should (equal (nostr-share-browse-note) "https://client.example/note1event1"))
        (should (equal browsed "https://client.example/note1event1"))
        (should (equal encoded '(("note" "event1"))))))))

(ert-deftest nostr-share-copies-and-browses-profile ()
  "Profile sections can be copied as npub and opened in a web client."
  (nostr-share-test-with-stubs
    (let ((nostr-share-web-client-base-url "https://client.example"))
      (nostr-share-test-with-section
          'profile "profile1" '((pubkey . "pubkey1"))
        (should (equal (nostr-share-copy-profile-id) "npub1pubkey1"))
        (should (equal copied "npub1pubkey1"))
        (setq encoded nil)
        (should (equal (nostr-share-browse-profile)
                       "https://client.example/npub1pubkey1"))
        (should (equal browsed "https://client.example/npub1pubkey1"))
        (should (equal encoded '(("npub" "pubkey1"))))))))

(ert-deftest nostr-share-copies-raw-ids-and-relays ()
  "Raw copy works for note, profile, and relay sections."
  (nostr-share-test-with-stubs
    (nostr-share-test-with-section
        'note "event1" '((id . "event1") (pubkey . "author1"))
      (should (equal (nostr-share-copy-raw-id) "event1"))
      (should (equal copied "event1")))
    (nostr-share-test-with-section
        'profile "profile1" '((pubkey . "pubkey1"))
      (should (equal (nostr-share-copy-raw-id) "pubkey1"))
      (should (equal copied "pubkey1")))
    (nostr-share-test-with-section
        'relay "relay1" '((url . "wss://relay.example"))
      (should (equal (nostr-share-copy-relay-url) "wss://relay.example"))
      (should (equal copied "wss://relay.example")))))

(ert-deftest nostr-share-keymaps-are-bound-in-primary-buffers ()
  "Primary buffers expose consistent share key bindings."
  (dolist (map (list nostr-timeline-mode-map
                     nostr-thread-mode-map
                     nostr-profile-mode-map
                     nostr-search-mode-map))
    (should (eq (lookup-key map (kbd "w")) #'nostr-share-copy))
    (should (eq (lookup-key map (kbd "y")) #'nostr-share-copy-raw-id))
    (should (eq (lookup-key map (kbd "b")) #'nostr-share-browse))
    (should (eq (lookup-key map (kbd "m")) #'nostr-ui-toggle-note-media)))
  (should (eq (lookup-key nostr-relays-mode-map (kbd "w"))
              #'nostr-relays-copy-url)))

(provide 'nostr-share-test)
;;; nostr-share-test.el ends here
