;;; nostr-upload.el --- Shared Blossom upload helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Async Blossom uploads shared by compose and profile editing.

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'nostr-backend)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(defvar url-request-data)
(defvar url-request-extra-headers)
(defvar url-request-method)

(defcustom nostr-blossom-upload-server "https://blossom.primal.net"
  "Blossom server URL used for media attachment uploads."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'nostr)

(defcustom nostr-blossom-upload-auth-expiration-seconds 300
  "Seconds before generated Blossom upload authorization expires."
  :type 'integer
  :group 'nostr)

(defun nostr-upload--sha256-file (file)
  "Return SHA-256 hex digest for FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (buffer-string))))

(defun nostr-upload--upload-url ()
  "Return configured Blossom upload endpoint."
  (unless (and (stringp nostr-blossom-upload-server)
               (not (string-empty-p nostr-blossom-upload-server)))
    (user-error "Set `nostr-blossom-upload-server' before uploading media"))
  (concat (string-remove-suffix "/" nostr-blossom-upload-server) "/upload"))

(defun nostr-upload-parse-blossom-url (json-text)
  "Return uploaded media URL from Blossom JSON-TEXT."
  (let* ((parsed (json-parse-string json-text :object-type 'alist :array-type 'list
                                    :false-object nil))
         (url (or (alist-get 'url parsed)
                  (alist-get 'download_url parsed)
                  (alist-get 'downloadUrl parsed)))
         (nip94 (alist-get 'nip94_event parsed)))
    (or url
        (when-let* ((tags (alist-get 'tags nip94)))
          (cadr (seq-find (lambda (tag)
                            (and (listp tag) (equal (car tag) "url")))
                          tags)))
        (error "Blossom upload response did not include a URL"))))

(defun nostr-upload--http-unibyte (string)
  "Return STRING encoded as unibyte HTTP header/body text."
  (if (multibyte-string-p string)
      (encode-coding-string string 'us-ascii)
    string))

(defun nostr-upload-sanitize-error (message)
  "Return upload error MESSAGE without embedded binary request data."
  (let ((sanitized (nostr-backend-sanitize-diagnostic (or message ""))))
    (if (string-match "\\`Multibyte text in HTTP request:" sanitized)
        "Could not build binary upload request"
      (truncate-string-to-width sanitized 240 nil nil "..."))))

(defun nostr-upload-backend-error-message (response stderr fallback)
  "Return sanitized backend error text from RESPONSE, STDERR, or FALLBACK."
  (let* ((message (nostr-backend-sanitize-diagnostic
                   (or (alist-get 'message (alist-get 'error response))
                       fallback)))
         (stderr-text (nostr-upload-sanitize-error (or stderr ""))))
    (concat message
            (if (string-empty-p stderr-text) "" (concat ": " stderr-text)))))

(defun nostr-upload--read-file-bytes (file)
  "Return unibyte contents of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (encode-coding-string (buffer-string) 'no-conversion)))

;;;###autoload
(defun nostr-upload-file (file success error)
  "Upload FILE to the configured Blossom server.
SUCCESS receives FILE and uploaded URL.  ERROR receives FILE and a
human-readable message."
  (unless (file-readable-p file)
    (user-error "Attachment is not readable: %s" file))
  (let* ((upload-url (nostr-upload--upload-url))
         (server (string-remove-suffix "/" nostr-blossom-upload-server))
         (sha256 (nostr-upload--sha256-file file))
         (expiration (+ (truncate (float-time))
                        nostr-blossom-upload-auth-expiration-seconds)))
    (nostr-backend-blossom-auth
     server sha256 expiration
     (lambda (auth)
       (let ((url-request-method "PUT")
             (url-request-data (nostr-upload--read-file-bytes file))
             (url-request-extra-headers
              `((,(nostr-upload--http-unibyte "Authorization")
                 . ,(nostr-upload--http-unibyte
                     (alist-get 'authorization auth)))
                (,(nostr-upload--http-unibyte "Content-Type")
                 . ,(nostr-upload--http-unibyte
                     "application/octet-stream")))))
         (condition-case err
             (url-retrieve
              upload-url
              (lambda (status)
                (unwind-protect
                    (if-let* ((err (plist-get status :error)))
                        (funcall error file
                                 (format "Could not upload %s: %s"
                                         file
                                         (nostr-upload-sanitize-error
                                          (format "%S" err))))
                      (if (not (and url-http-response-status
                                    (>= url-http-response-status 200)
                                    (< url-http-response-status 300)))
                          (funcall error file
                                   (format "Could not upload %s: HTTP %s"
                                           file
                                           (or url-http-response-status "unknown")))
                        (condition-case err
                            (funcall success file
                                     (nostr-upload-parse-blossom-url
                                      (buffer-substring-no-properties
                                       (1+ url-http-end-of-headers)
                                       (point-max))))
                          (error (funcall error file
                                          (nostr-upload-sanitize-error
                                           (error-message-string err)))))))
                  (kill-buffer (current-buffer))))
              nil t)
           (error
            (funcall error file
                     (format "Could not upload %s: %s"
                             file
                             (nostr-upload-sanitize-error
                              (error-message-string err))))))))
     (lambda (response stderr _status)
       (funcall error file
                (format "Could not authorize %s: %s"
                        file
                        (nostr-upload-backend-error-message
                         response stderr "backend error")))))))

(provide 'nostr-upload)
;;; nostr-upload.el ends here
