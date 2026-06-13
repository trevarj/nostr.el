;;; nostr-nip.el --- NIP helper functions for nostr.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; NIP-05 verification and NIP-19 backend wrappers.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'nostr-backend)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defcustom nostr-nip05-timeout 10
  "Seconds to wait for NIP-05 HTTP responses."
  :type 'number
  :group 'nostr)

(defvar nostr-nip05-fetch-function nil
  "Optional test hook for NIP-05 fetches.
The function receives URL, SUCCESS and ERROR callbacks.  SUCCESS receives JSON
text.  ERROR receives a human-readable message.")

(defun nostr-nip05-parse-identifier (identifier)
  "Parse NIP-05 IDENTIFIER into an alist with name and domain."
  (unless (string-match "\\`\\([^@[:space:]]+\\)@\\([^@[:space:]]+\\)\\'" identifier)
    (error "Invalid NIP-05 identifier: %s" identifier))
  `((name . ,(match-string 1 identifier))
    (domain . ,(match-string 2 identifier))))

(defun nostr-nip05-url (identifier)
  "Return the verification URL for NIP-05 IDENTIFIER."
  (let-alist (nostr-nip05-parse-identifier identifier)
    (format "https://%s/.well-known/nostr.json?name=%s"
            .domain
            (url-hexify-string .name))))

(defun nostr-nip05--fetch (url success error)
  "Fetch NIP-05 URL and call SUCCESS or ERROR."
  (if nostr-nip05-fetch-function
      (funcall nostr-nip05-fetch-function url success error)
    (url-retrieve
     url
     (lambda (status)
       (unwind-protect
           (if-let* ((err (plist-get status :error)))
               (funcall error (format "Could not fetch %s: %S" url err))
             (if (not (equal url-http-response-status 200))
                 (funcall error (format "Could not fetch %s: HTTP %s"
                                        url url-http-response-status))
               (funcall success
                        (buffer-substring-no-properties
                         (1+ url-http-end-of-headers)
                         (point-max)))))
         (kill-buffer (current-buffer))))
     nil t)))

(defun nostr-nip05--response-pubkey (identifier json-text)
  "Return IDENTIFIER's pubkey from NIP-05 JSON-TEXT."
  (let* ((parsed (json-parse-string json-text :object-type 'alist :array-type 'list
                                    :false-object nil))
         (name (alist-get 'name (nostr-nip05-parse-identifier identifier)))
         (names (alist-get 'names parsed)))
    (alist-get (intern name) names nil nil #'eq)))

(defun nostr-nip05-verify (identifier pubkey success error)
  "Verify IDENTIFIER resolves to PUBKEY.
SUCCESS receives an alist with identifier, pubkey, verified, and url.  ERROR
receives a human-readable message."
  (let ((url (nostr-nip05-url identifier)))
    (nostr-nip05--fetch
     url
     (lambda (json-text)
       (condition-case err
           (let ((resolved (nostr-nip05--response-pubkey identifier json-text)))
             (funcall success
                      `((identifier . ,identifier)
                        (pubkey . ,pubkey)
                        (resolved-pubkey . ,resolved)
                        (verified . ,(equal resolved pubkey))
                        (url . ,url))))
         (error (funcall error (error-message-string err)))))
     error)))

(defun nostr-nip05-verify-sync (identifier pubkey)
  "Synchronously verify IDENTIFIER resolves to PUBKEY."
  (let (result failure)
    (nostr-nip05-verify identifier pubkey
                        (lambda (value) (setq result value))
                        (lambda (message) (setq failure message)))
    (when failure
      (error "%s" failure))
    result))

(defun nostr-nip19-decode (value success error)
  "Decode NIP-19 VALUE asynchronously using the backend."
  (nostr-backend-call "nip19-decode" `((value . ,value)) success error))

(defun nostr-nip19-decode-sync (value)
  "Decode NIP-19 VALUE synchronously using the backend."
  (let* ((result (nostr-backend-call-sync "nip19-decode" `((value . ,value))))
         (exit (car result))
         (response (cdr result)))
    (unless (and (zerop exit) (alist-get 'ok response))
      (error "Could not decode NIP-19 value"))
    response))

(defun nostr-nip19-encode (entity value success error)
  "Encode ENTITY and VALUE as a NIP-19 value asynchronously."
  (nostr-backend-call "nip19-encode"
                      `((entity . ,entity)
                        (value . ,value))
                      success error))

(defun nostr-nip19-encode-sync (entity value)
  "Encode ENTITY and VALUE as a NIP-19 value synchronously."
  (let* ((result (nostr-backend-call-sync "nip19-encode"
                                          `((entity . ,entity)
                                            (value . ,value))))
         (exit (car result))
         (response (cdr result)))
    (unless (and (zerop exit) (alist-get 'ok response))
      (error "Could not encode NIP-19 value"))
    response))

(provide 'nostr-nip)
;;; nostr-nip.el ends here
