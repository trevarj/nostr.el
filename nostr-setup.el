;;; nostr-setup.el --- Account and backend setup helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Setup commands for checking the Rust backend and deriving the local account
;; identity from the configured GPG-encrypted secret key file.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'nostr-backend)
(require 'nostr-ui)
(require 'transient)

(defcustom nostr-private-key-recipients nil
  "GPG recipients used when creating `nostr-private-key-path'.
When nil, `nostr-setup-import-private-key' creates a symmetrically encrypted
file and GPG prompts locally for its passphrase."
  :type '(repeat string)
  :group 'nostr)

(defvar nostr-current-pubkey nil
  "Current local account public key.")

(defconst nostr-setup-status-buffer-name "*Nostr Setup*"
  "Buffer name for Nostr account setup status.")

(defconst nostr-setup-required-backend-commands
  '("capabilities" "generate-key" "pubkey" "sign-event" "verify-event" "nip19-decode" "nip19-encode")
  "Backend commands required by nostr.el.")

(defvar nostr-setup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'nostr-setup-status-refresh)
    (define-key map (kbd "c") #'nostr-setup-check-backend)
    (define-key map (kbd "d") #'nostr-setup-derive-pubkey)
    (define-key map (kbd "i") #'nostr-setup-import-private-key)
    (define-key map (kbd "G") #'nostr-setup-generate-private-key)
    (define-key map (kbd "?") #'nostr-setup-actions)
    map)
  "Keymap for `nostr-setup-mode'.")

(define-derived-mode nostr-setup-mode special-mode "Nostr-Setup"
  "Mode for Nostr account and backend setup status buffers.")

(transient-define-prefix nostr-setup-actions ()
  "Actions for Nostr account and backend setup."
  [["Setup"
    ("g" "Refresh status" nostr-setup-status-refresh)
    ("c" "Check backend" nostr-setup-check-backend)
    ("d" "Derive pubkey" nostr-setup-derive-pubkey)
    ("i" "Import private key" nostr-setup-import-private-key)
    ("G" "Generate private key" nostr-setup-generate-private-key)]])

(defun nostr-setup--empty-payload ()
  "Return an empty JSON object payload for backend calls."
  (make-hash-table :test 'equal))

(defun nostr-setup--error-text (response stderr exit-status fallback)
  "Return actionable setup error text from RESPONSE, STDERR and EXIT-STATUS.
FALLBACK describes the operation that failed."
  (let* ((error-body (and (listp response) (alist-get 'error response)))
         (code (and (listp error-body) (alist-get 'code error-body)))
         (message (and (listp error-body) (alist-get 'message error-body)))
         (stderr-text (string-trim (or stderr ""))))
    (string-join
     (delq nil
           (list
            fallback
            (when code (format "Backend error code: %s" code))
            (when message (format "Backend message: %s" message))
            (when (and exit-status (not (zerop exit-status)))
              (format "Backend exit status: %s" exit-status))
            (unless (string-empty-p stderr-text)
              (format "Backend stderr: %s" stderr-text))
            (format "Check `nostr-backend-command' (%S) and `nostr-private-key-path' (%S)."
                    nostr-backend-command
                    (expand-file-name nostr-private-key-path))))
     "\n")))

(defun nostr-setup--call-sync (command payload fallback)
  "Call backend COMMAND with PAYLOAD or signal FALLBACK as an actionable error."
  (condition-case err
      (let* ((result (nostr-backend-call-sync command payload))
             (exit-status (car result))
             (response (cdr result)))
        (unless (and (zerop exit-status) (alist-get 'ok response))
          (error "%s" (nostr-setup--error-text response nil exit-status fallback)))
        response)
    (file-missing
     (error "%s\nCould not run backend command %S. Build the Rust backend or set `nostr-backend-command'."
            fallback nostr-backend-command))
    (json-parse-error
     (error "%s\nBackend returned invalid JSON. Check that %S points to `nostr-el-backend'."
            fallback nostr-backend-command))
    (error
     (if (string-prefix-p fallback (error-message-string err))
         (signal (car err) (cdr err))
       (error "%s\n%s" fallback (error-message-string err))))))

(defun nostr-setup-check-backend ()
  "Check that `nostr-backend-command' is available and supports required commands.
Return the backend capabilities alist.  Interactively, report a concise status
message."
  (interactive)
  (let* ((response (nostr-setup--call-sync
                    "capabilities"
                    (nostr-setup--empty-payload)
                    "Could not check Nostr backend capabilities."))
         (commands (alist-get 'commands response))
         (missing (cl-set-difference nostr-setup-required-backend-commands
                                     commands
                                     :test #'string=)))
    (when missing
      (error "Nostr backend is missing required commands: %s"
             (string-join missing ", ")))
    (when (called-interactively-p 'interactive)
      (message "Nostr backend OK: %s protocol %s"
               (or (alist-get 'backend response) nostr-backend-command)
               (or (alist-get 'protocol_version response) "unknown")))
    response))

(defun nostr-setup--load-secret ()
  "Load the configured secret key or signal an actionable setup error."
  (let ((path (expand-file-name nostr-private-key-path)))
    (unless (file-exists-p path)
      (error "Nostr private key file does not exist: %s\nCreate a GPG-encrypted nsec/hex key file or set `nostr-private-key-path'."
             path)))
  (condition-case err
      (nostr-backend-load-secret)
    (error
     (error "Could not decrypt Nostr private key file: %s\nUnlock GPG locally and check `nostr-private-key-path'."
            (error-message-string err)))))

(defun nostr-setup-derive-pubkey ()
  "Derive and store the current account pubkey using the configured backend.
The secret key is read with `nostr-backend-load-secret' and sent to the backend
over stdin, never argv.  Return the backend pubkey response."
  (interactive)
  (let* ((secret (nostr-setup--load-secret))
         (response (nostr-setup--call-sync
                    "pubkey"
                    `((secret_key . ,secret))
                    "Could not derive Nostr account pubkey."))
         (pubkey (alist-get 'pubkey response)))
    (unless (and (stringp pubkey) (not (string-empty-p pubkey)))
      (error "Backend returned no pubkey while deriving Nostr account status."))
    (setq nostr-current-pubkey pubkey)
    (when (called-interactively-p 'interactive)
      (message "Nostr account pubkey: %s" pubkey))
    response))

(defun nostr-setup--validate-secret (secret)
  "Validate SECRET with the backend and return the derived account response."
  (let* ((response (nostr-setup--call-sync
                    "pubkey"
                    `((secret_key . ,secret))
                    "Could not validate Nostr private key."))
         (pubkey (alist-get 'pubkey response)))
    (unless (and (stringp pubkey) (not (string-empty-p pubkey)))
      (error "Backend returned no pubkey while validating Nostr private key."))
    response))

(defun nostr-setup--encrypt-secret-file (secret output-path)
  "Encrypt SECRET into OUTPUT-PATH using GPG."
  (let* ((expanded-path (expand-file-name output-path))
         (directory (file-name-directory expanded-path))
         (plain-file (make-temp-file "nostr-secret-plain"))
         (cipher-file (make-temp-file "nostr-secret-cipher")))
    (unwind-protect
        (progn
          (make-directory directory t)
          ;; Keep the temporary plaintext readable only by the current user.
          (set-file-modes plain-file #o600)
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (concat (string-trim secret) "\n") nil plain-file nil 'silent))
          (epg-encrypt-file
           (epg-make-context 'OpenPGP)
           plain-file
           nostr-private-key-recipients
           cipher-file
           nil
           t)
          (rename-file cipher-file expanded-path t)
          (set-file-modes expanded-path #o600)
          expanded-path)
      (when (file-exists-p plain-file)
        (delete-file plain-file))
      (when (file-exists-p cipher-file)
        (delete-file cipher-file)))))

;;;###autoload
(defun nostr-setup-import-private-key (secret &optional replace)
  "Validate and store SECRET in `nostr-private-key-path' as a GPG file.
SECRET may be an nsec or hex private key.  Without REPLACE, refuse to overwrite
an existing keyfile.  Interactively, a prefix argument allows replacement."
  (interactive
   (list (read-passwd "Nostr private key (nsec or hex): ")
         current-prefix-arg))
  (let* ((path (expand-file-name nostr-private-key-path))
         account)
    (when (and (file-exists-p path) (not replace))
      (error "Nostr private key file already exists: %s" path))
    (setq account (nostr-setup--validate-secret secret))
    (nostr-setup--encrypt-secret-file secret path)
    (setq nostr-current-pubkey (alist-get 'pubkey account))
    (when (called-interactively-p 'interactive)
      (message "Stored encrypted Nostr private key for pubkey %s"
               nostr-current-pubkey))
    account))

;;;###autoload
(defun nostr-setup-generate-private-key (&optional replace)
  "Generate and store a new Nostr private key in `nostr-private-key-path'.
Without REPLACE, refuse to overwrite an existing keyfile.  Interactively, a
prefix argument allows replacement."
  (interactive "P")
  (let* ((path (expand-file-name nostr-private-key-path))
         (response nil)
         (secret nil))
    (when (and (file-exists-p path) (not replace))
      (error "Nostr private key file already exists: %s" path))
    (setq response (nostr-setup--call-sync
                    "generate-key"
                    (nostr-setup--empty-payload)
                    "Could not generate Nostr private key."))
    (setq secret (alist-get 'secret_key response))
    (unless (and (stringp secret) (not (string-empty-p secret)))
      (error "Backend returned no secret key while generating Nostr private key."))
    (nostr-setup--encrypt-secret-file secret path)
    (setq nostr-current-pubkey (alist-get 'pubkey response))
    (when (called-interactively-p 'interactive)
      (message "Generated encrypted Nostr private key for pubkey %s"
               nostr-current-pubkey))
    response))

(defun nostr-setup--status-lines ()
  "Return status lines for the configured backend and account."
  (let* ((key-path (expand-file-name nostr-private-key-path))
         (key-exists (file-exists-p key-path))
         (backend-status nil)
         (account-status nil)
         (capabilities nil)
         (account nil))
    (condition-case err
        (setq capabilities (nostr-setup-check-backend)
              backend-status "OK")
      (error
       (setq backend-status (error-message-string err))))
    (condition-case err
        (setq account (nostr-setup-derive-pubkey)
              account-status "OK")
      (error
       (setq account-status (error-message-string err))))
    (delq nil
          (list
           "Nostr setup status"
           ""
           (format "Backend command: %s" nostr-backend-command)
           (format "Backend status: %s" backend-status)
           (when capabilities
             (format "Backend protocol: %s"
                     (or (alist-get 'protocol_version capabilities) "unknown")))
           (when capabilities
             (format "Backend commands: %s"
                     (string-join (alist-get 'commands capabilities) ", ")))
           ""
           (format "Private key path: %s" key-path)
           (format "Private key file: %s" (if key-exists "present" "missing"))
           (format "Account status: %s" account-status)
           (when account
             (format "Public key: %s" (alist-get 'pubkey account)))
           (when account
             (format "Npub: %s" (or (alist-get 'npub account) "unavailable")))
           (when nostr-current-pubkey
             (format "Current session pubkey: %s" nostr-current-pubkey))))))

(defun nostr-setup-status-refresh ()
  "Refresh the current setup status buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (point-state (point)))
    (nostr-ui-clear)
    (nostr-ui-insert-status-header
     "Setup"
     "Account and backend"
     "Backend, private key, and current account status.")
    (insert (string-join (cdr (nostr-setup--status-lines)) "\n"))
    (insert "\n")
    (nostr-ui-insert-footer
     '("g refresh" "c backend" "d derive" "i import" "G generate" "? actions"))
    (goto-char (min point-state (point-max)))))

;;;###autoload
(defun nostr-setup-status (&optional message-only)
  "Display current Nostr backend and account status.
With prefix argument MESSAGE-ONLY, show a minibuffer summary instead of opening
the status buffer."
  (interactive "P")
  (let ((lines (nostr-setup--status-lines)))
    (if message-only
        (message "%s" (string-join (seq-take lines 4) " | "))
      (let ((buffer (get-buffer-create nostr-setup-status-buffer-name)))
        (with-current-buffer buffer
          (nostr-setup-mode)
          (nostr-setup-status-refresh)
          (goto-char (point-min)))
        (pop-to-buffer buffer)))))

(defalias 'nostr-check-backend #'nostr-setup-check-backend)
(defalias 'nostr-derive-pubkey #'nostr-setup-derive-pubkey)
(defalias 'nostr-import-private-key #'nostr-setup-import-private-key)
(defalias 'nostr-generate-private-key #'nostr-setup-generate-private-key)
(defalias 'nostr-account-status #'nostr-setup-status)

(provide 'nostr-setup)
;;; nostr-setup.el ends here
