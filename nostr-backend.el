;;; nostr-backend.el --- Rust backend process bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Async JSON protocol for the nostr-sdk CLI shim.

;;; Code:

(require 'cl-lib)
(require 'epg)
(require 'json)
(require 'subr-x)

(defgroup nostr nil
  "A public Nostr client in Emacs."
  :group 'applications)

(defcustom nostr-private-key-path "~/.nostr-private.gpg"
  "Path to the GPG-encrypted local Nostr secret key."
  :type 'file
  :group 'nostr)

(defcustom nostr-backend-command "nostr-el-backend"
  "Command used for Nostr signing and NIP helper operations."
  :type 'string
  :group 'nostr)

(defun nostr-backend-load-secret ()
  "Decrypt and return the configured local Nostr secret key."
  (string-trim
   (epg-decrypt-file
    (epg-make-context 'OpenPGP)
    (expand-file-name nostr-private-key-path)
    nil)))

(defun nostr-backend--json-read (string)
  "Parse STRING as backend JSON."
  (json-parse-string string :object-type 'alist :array-type 'list :false-object nil))

(defun nostr-backend--tags-to-vector (tags)
  "Return TAGS as JSON array data, preserving Nostr tag array shape."
  (vconcat
   (mapcar (lambda (tag)
             (if (vectorp tag)
                 tag
               (vconcat tag)))
           tags)))

(defun nostr-backend--event-to-json-data (event)
  "Return EVENT with Nostr tag lists prepared for `json-encode'."
  (mapcar (lambda (entry)
            (if (eq (car entry) 'tags)
                (cons (car entry) (nostr-backend--tags-to-vector (cdr entry)))
              entry))
          event))

(defun nostr-backend--payload-to-json-data (payload)
  "Return PAYLOAD with Nostr arrays prepared for `json-encode'."
  (if (hash-table-p payload)
      (let ((table (make-hash-table :test (hash-table-test payload))))
        (maphash
         (lambda (key value)
           (puthash
            key
            (pcase key
              ('tags (nostr-backend--tags-to-vector value))
              ('event (nostr-backend--event-to-json-data value))
              (_ value))
            table))
         payload)
        table)
    (mapcar (lambda (entry)
              (pcase (car entry)
                ('tags (cons (car entry) (nostr-backend--tags-to-vector (cdr entry))))
                ('event (cons (car entry) (nostr-backend--event-to-json-data (cdr entry))))
                (_ entry)))
            payload)))

(defun nostr-backend--json-encode (payload)
  "Encode backend PAYLOAD as JSON without corrupting Nostr tag arrays."
  (json-encode (nostr-backend--payload-to-json-data payload)))

(defun nostr-backend-call (command payload success error)
  "Call backend COMMAND with PAYLOAD.
SUCCESS receives the parsed response on ok.  ERROR receives parsed response,
stderr text, and exit status."
  (let* ((stdout (generate-new-buffer (format " *nostr-backend-%s-out*" command)))
         (stderr (generate-new-buffer (format " *nostr-backend-%s-err*" command)))
         (proc (make-process
                :name (format "nostr-backend-%s" command)
                :buffer stdout
                :command (list nostr-backend-command command)
                :stderr stderr
                :noquery t
                :connection-type 'pipe
                :sentinel
                (lambda (process _event)
                  (when (memq (process-status process) '(exit signal))
                    (let* ((exit-status (process-exit-status process))
                           (out (with-current-buffer stdout (buffer-string)))
                           (err (with-current-buffer stderr (buffer-string)))
                           (response (ignore-errors (nostr-backend--json-read out))))
                      (unwind-protect
                          (if (and (zerop exit-status)
                                   response
                                   (alist-get 'ok response))
                              (funcall success response)
                            (funcall error response err exit-status))
                        (kill-buffer stdout)
                        (kill-buffer stderr))))))))
    (process-send-string proc (nostr-backend--json-encode payload))
    (process-send-eof proc)
    proc))

(defun nostr-backend-call-sync (command payload)
  "Synchronously call backend COMMAND with PAYLOAD for tests and setup."
  (with-temp-buffer
    (let ((input (nostr-backend--json-encode payload)))
      (insert input)
      (let ((exit (call-process-region (point-min) (point-max)
                                       nostr-backend-command
                                       t t nil command)))
        (cons exit (nostr-backend--json-read (buffer-string)))))))

(defun nostr-backend-pubkey-sync ()
  "Return current account pubkey synchronously."
  (let* ((result (nostr-backend-call-sync
                  "pubkey"
                  `((secret_key . ,(nostr-backend-load-secret)))))
         (response (cdr result)))
    (unless (and (zerop (car result)) (alist-get 'ok response))
      (error "Could not derive Nostr pubkey"))
    (alist-get 'pubkey response)))

(defun nostr-backend-sign-event (kind tags content success error)
  "Sign KIND event with TAGS and CONTENT, then call SUCCESS or ERROR."
  (nostr-backend-call
   "sign-event"
   `((secret_key . ,(nostr-backend-load-secret))
     (kind . ,kind)
     (tags . ,tags)
     (content . ,content)
     (envelope . t))
   success
   error))

(defun nostr-backend-blossom-auth (server sha256 expiration success error)
  "Build a Blossom upload authorization event for SERVER and SHA256."
  (nostr-backend-call
   "blossom-auth"
   `((secret_key . ,(nostr-backend-load-secret))
     (server . ,server)
     (sha256 . ,sha256)
     (expiration . ,expiration))
   success
   error))

(provide 'nostr-backend)
;;; nostr-backend.el ends here
