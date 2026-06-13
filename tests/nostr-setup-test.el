;;; nostr-setup-test.el --- Tests for Nostr setup helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nostr-setup)

(ert-deftest nostr-setup-check-backend-uses-capabilities ()
  "Backend checks call the capabilities command and validate commands."
  (let (called-command called-payload)
    (cl-letf (((symbol-function 'nostr-backend-call-sync)
               (lambda (command payload)
                 (setq called-command command
                       called-payload payload)
                 (cons 0
                       '((ok . t)
                         (backend . "nostr-el-backend")
	                         (protocol_version . 1)
	                         (commands . ("capabilities"
                                      "generate-key"
	                                      "pubkey"
	                                      "sign-event"
                                      "verify-event"
                                      "nip19-decode"
                                      "nip19-encode"
                                      "blossom-auth")))))))
      (let ((response (nostr-setup-check-backend)))
        (should (equal called-command "capabilities"))
        (should (hash-table-p called-payload))
        (should (equal (alist-get 'backend response) "nostr-el-backend"))))))

(ert-deftest nostr-setup-check-backend-reports-missing-commands ()
  "Backend checks report missing required commands with actionable text."
  (cl-letf (((symbol-function 'nostr-backend-call-sync)
             (lambda (_command _payload)
               (cons 0
                     '((ok . t)
                       (backend . "nostr-el-backend")
                       (protocol_version . 1)
                       (commands . ("capabilities" "pubkey")))))))
    (should-error (nostr-setup-check-backend)
                  :type 'error)))

(ert-deftest nostr-setup-derive-pubkey-loads-secret-and-stores-current-account ()
  "Pubkey derivation loads the configured secret and stores `nostr-current-pubkey'."
  (let ((nostr-current-pubkey nil)
        (nostr-private-key-path (make-temp-file "nostr-test-key" nil ".gpg"))
        called)
    (unwind-protect
        (cl-letf (((symbol-function 'nostr-backend-load-secret)
                   (lambda () "nsec-test-secret"))
                  ((symbol-function 'nostr-backend-call-sync)
                   (lambda (command payload)
                     (setq called (list command payload))
                     (cons 0
                           '((ok . t)
                             (pubkey . "pubkey-1")
                             (npub . "npub1test"))))))
          (let ((response (nostr-setup-derive-pubkey)))
            (should (equal (car called) "pubkey"))
            (should (equal (alist-get 'secret_key (cadr called)) "nsec-test-secret"))
            (should (equal (alist-get 'pubkey response) "pubkey-1"))
            (should (equal nostr-current-pubkey "pubkey-1"))))
      (delete-file nostr-private-key-path))))

(ert-deftest nostr-setup-derive-pubkey-reports-missing-keyfile ()
  "Pubkey derivation explains when the configured key file is absent."
  (let ((nostr-private-key-path
         (expand-file-name "nostr-missing-key.gpg"
                           (make-temp-file "nostr-missing-key-dir" t))))
    (unwind-protect
        (should-error (nostr-setup-derive-pubkey)
                      :type 'error)
      (delete-directory (file-name-directory nostr-private-key-path) t))))

(ert-deftest nostr-setup-derive-pubkey-does-not-echo-secret-on-backend-failure ()
  "Backend failures for pubkey derivation do not expose the secret value."
  (let ((nostr-private-key-path (make-temp-file "nostr-test-key" nil ".gpg")))
    (unwind-protect
        (cl-letf (((symbol-function 'nostr-backend-load-secret)
                   (lambda () "nsec-secret-that-must-not-leak"))
                  ((symbol-function 'nostr-backend-call-sync)
                   (lambda (_command _payload)
                     (cons 1
                           '((ok . nil)
                             (error . ((code . "invalid-key")
                                       (message . "Invalid secret key"))))))))
          (let* ((err (should-error (nostr-setup-derive-pubkey)))
                 (message (error-message-string err)))
            (should (string-match-p "invalid-key" message))
            (should-not (string-match-p "nsec-secret-that-must-not-leak" message))))
      (delete-file nostr-private-key-path))))

(ert-deftest nostr-setup-import-private-key-validates-and-encrypts ()
  "Private key import validates the secret and stores only encrypted output."
  (let* ((directory (make-temp-file "nostr-import-key-dir" t))
         (nostr-private-key-path (expand-file-name "nostr-private.gpg" directory))
         (nostr-private-key-recipients '("user@example.com"))
         (nostr-current-pubkey nil)
         (secret "nsec-secret-that-must-not-be-written-plainly")
         called-payload encrypted-recipients encrypted-plain-text)
    (unwind-protect
        (cl-letf (((symbol-function 'nostr-backend-call-sync)
                   (lambda (command payload)
                     (setq called-payload (list command payload))
                     (cons 0
                           '((ok . t)
                             (pubkey . "pubkey-imported")
                             (npub . "npub1imported")))))
                  ((symbol-function 'epg-make-context)
                   (lambda (&optional _protocol) 'context))
                  ((symbol-function 'epg-encrypt-file)
                   (lambda (_context plain recipients cipher &optional _sign _always-trust)
                     (setq encrypted-recipients recipients)
                     (setq encrypted-plain-text
                           (with-temp-buffer
                             (insert-file-contents plain)
                             (buffer-string)))
                     (write-region "ciphertext only\n" nil cipher nil 'silent))))
          (let ((response (nostr-setup-import-private-key secret)))
            (should (equal (car called-payload) "pubkey"))
            (should (equal (alist-get 'secret_key (cadr called-payload)) secret))
            (should (equal encrypted-recipients '("user@example.com")))
            (should (equal encrypted-plain-text (concat secret "\n")))
            (should (equal (alist-get 'pubkey response) "pubkey-imported"))
            (should (equal nostr-current-pubkey "pubkey-imported"))
            (should (file-exists-p nostr-private-key-path))
            (with-temp-buffer
              (insert-file-contents-literally nostr-private-key-path)
              (should-not (string-match-p secret (buffer-string))))))
      (delete-directory directory t))))

(ert-deftest nostr-setup-import-private-key-refuses-overwrite-without-replace ()
  "Private key import does not overwrite an existing keyfile by default."
  (let* ((directory (make-temp-file "nostr-import-existing-key-dir" t))
         (nostr-private-key-path (expand-file-name "nostr-private.gpg" directory))
         backend-called encrypt-called)
    (unwind-protect
        (progn
          (let ((file-name-handler-alist nil))
            (write-region "existing ciphertext\n" nil nostr-private-key-path nil 'silent))
          (cl-letf (((symbol-function 'nostr-backend-call-sync)
                     (lambda (&rest _args)
                       (setq backend-called t)))
                    ((symbol-function 'epg-encrypt-file)
                     (lambda (&rest _args)
                       (setq encrypt-called t))))
            (should-error (nostr-setup-import-private-key "nsec-new-secret")
                          :type 'error)
            (should-not backend-called)
            (should-not encrypt-called)
            (with-temp-buffer
              (insert-file-contents-literally nostr-private-key-path)
              (should (equal (buffer-string) "existing ciphertext\n")))))
      (delete-directory directory t))))

(ert-deftest nostr-setup-generate-private-key-encrypts-new-secret ()
  "Private key generation stores backend-created keys in the encrypted keyfile."
  (let* ((directory (make-temp-file "nostr-generate-key-dir" t))
         (nostr-private-key-path (expand-file-name "nostr-private.gpg" directory))
         (nostr-current-pubkey nil)
         called-command encrypted-plain-text)
    (unwind-protect
        (cl-letf (((symbol-function 'nostr-backend-call-sync)
                   (lambda (command payload)
                     (setq called-command (list command payload))
                     (cons 0
                           '((ok . t)
                             (secret_key . "generated-secret-hex")
                             (nsec . "nsec1generated")
                             (pubkey . "generated-pubkey")
                             (npub . "npub1generated")))))
                  ((symbol-function 'epg-make-context)
                   (lambda (&optional _protocol) 'context))
                  ((symbol-function 'epg-encrypt-file)
                   (lambda (_context plain _recipients cipher &optional _sign _always-trust)
                     (setq encrypted-plain-text
                           (with-temp-buffer
                             (insert-file-contents plain)
                             (buffer-string)))
                     (write-region "generated ciphertext\n" nil cipher nil 'silent))))
          (let ((response (nostr-setup-generate-private-key)))
            (should (equal (car called-command) "generate-key"))
            (should (hash-table-p (cadr called-command)))
            (should (equal encrypted-plain-text "generated-secret-hex\n"))
            (should (equal (alist-get 'pubkey response) "generated-pubkey"))
            (should (equal nostr-current-pubkey "generated-pubkey"))
            (with-temp-buffer
              (insert-file-contents-literally nostr-private-key-path)
              (should-not (string-match-p "generated-secret-hex" (buffer-string))))))
      (delete-directory directory t))))

(ert-deftest nostr-backend-call-keeps-secret-out-of-process-argv ()
  "Async backend calls send secret payload over stdin, not process argv."
  (let ((nostr-backend-command "nostr-el-backend-test")
        captured-command
        captured-stdin
        eof-called)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq captured-command (plist-get plist :command))
                 'nostr-test-process))
              ((symbol-function 'process-send-string)
               (lambda (process string)
                 (should (eq process 'nostr-test-process))
                 (setq captured-stdin string)))
              ((symbol-function 'process-send-eof)
               (lambda (process)
                 (should (eq process 'nostr-test-process))
                 (setq eof-called t))))
      (should (eq (nostr-backend-call
                   "pubkey"
                   '((secret_key . "nsec-secret-stdin-only"))
                   #'ignore
                   #'ignore)
                  'nostr-test-process))
      (should (equal captured-command '("nostr-el-backend-test" "pubkey")))
      (should-not (string-match-p "nsec-secret-stdin-only"
                                  (prin1-to-string captured-command)))
      (should (string-match-p "nsec-secret-stdin-only" captured-stdin))
      (should eof-called))))

(ert-deftest nostr-backend-call-sync-keeps-secret-out-of-process-argv ()
  "Sync backend calls send secret payload over stdin, not process argv."
  (let ((nostr-backend-command "nostr-el-backend-test")
        captured-program
        captured-args
        captured-stdin)
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (start end program delete buffer display &rest args)
                 (setq captured-program program
                       captured-args args
                       captured-stdin (buffer-substring-no-properties start end))
                 (should delete)
                 (should (eq buffer t))
                 (should-not display)
                 (delete-region (point-min) (point-max))
                 (insert "{\"ok\":true,\"pubkey\":\"pubkey-sync\"}")
                 0)))
      (let ((result (nostr-backend-call-sync
                     "pubkey"
                     '((secret_key . "nsec-secret-sync-stdin-only")))))
        (should (zerop (car result)))
        (should (equal (alist-get 'pubkey (cdr result)) "pubkey-sync"))
        (should (equal captured-program "nostr-el-backend-test"))
        (should (equal captured-args '("pubkey")))
        (should-not (string-match-p "nsec-secret-sync-stdin-only"
                                    (prin1-to-string captured-args)))
        (should (string-match-p "nsec-secret-sync-stdin-only" captured-stdin))))))

(ert-deftest nostr-backend-json-encode-preserves-nostr-tag-arrays ()
  "Backend JSON encoding keeps Nostr tags as arrays, not JSON objects."
  (let* ((payload '((event . ((id . "event1")
                              (tags . (("nonce" "1" "2")
                                       ("p" "pubkey1")))))))
         (encoded (nostr-backend--json-encode payload)))
    (should (string-match-p
             "\"tags\":\\[\\[\"nonce\",\"1\",\"2\"\\],\\[\"p\",\"pubkey1\"\\]\\]"
             encoded))
    (should-not (string-match-p "\"tags\":{" encoded))))

(ert-deftest nostr-backend-json-encode-preserves-empty-objects ()
  "Backend JSON encoding keeps empty request objects as JSON objects."
  (should (equal (nostr-backend--json-encode (make-hash-table :test 'equal))
                 "{}")))

(ert-deftest nostr-setup-status-renders-buffer ()
  "Account status renders backend and account state into a setup buffer."
  (let ((nostr-current-pubkey nil)
        (nostr-private-key-path (make-temp-file "nostr-test-key" nil ".gpg")))
    (cl-letf (((symbol-function 'nostr-backend-load-secret)
               (lambda () "nsec-test-secret"))
              ((symbol-function 'nostr-backend-call-sync)
               (lambda (command _payload)
                 (pcase command
                   ("capabilities"
                    (cons 0
                          '((ok . t)
                            (backend . "nostr-el-backend")
	                            (protocol_version . 1)
	                            (commands . ("capabilities"
                                         "generate-key"
	                                         "pubkey"
	                                         "sign-event"
                                         "verify-event"
                                         "nip19-decode"
                                         "nip19-encode"
                                         "blossom-auth")))))
                   ("pubkey"
                    (cons 0
                          '((ok . t)
                            (pubkey . "pubkey-1")
                            (npub . "npub1test"))))
                   (_ (error "unexpected command: %s" command))))))
      (unwind-protect
          (progn
            (nostr-setup-status)
            (with-current-buffer nostr-setup-status-buffer-name
              (should (equal major-mode 'nostr-setup-mode))
              (should (eq (lookup-key nostr-setup-mode-map (kbd "?"))
                          #'nostr-setup-actions))
              (should (string-match-p "Backend status: OK" (buffer-string)))
              (should (string-match-p "Account status: OK" (buffer-string)))
              (should (string-match-p "Public key: pubkey-1" (buffer-string)))
              (should (string-match-p "Npub: npub1test" (buffer-string)))))
        (when-let* ((buffer (get-buffer nostr-setup-status-buffer-name)))
          (kill-buffer buffer))
        (delete-file nostr-private-key-path)))))

(provide 'nostr-setup-test)
;;; nostr-setup-test.el ends here
