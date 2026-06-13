;;; nostr-event.el --- Nostr event helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Pure helpers for normalized Nostr events and tags.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(defconst nostr-kind-metadata 0)
(defconst nostr-kind-text-note 1)
(defconst nostr-kind-contacts 3)
(defconst nostr-kind-repost 6)
(defconst nostr-kind-reaction 7)
(defconst nostr-kind-zap-receipt 9735)
(defconst nostr-kind-relay-list 10002)

(defun nostr-event-tag-name (tag)
  "Return TAG's name."
  (car-safe tag))

(defun nostr-event-tags-by-name (tags name)
  "Return every tag in TAGS whose first value is NAME."
  (seq-filter (lambda (tag) (equal (nostr-event-tag-name tag) name)) tags))

(defun nostr-event-first-tag-value (tags name)
  "Return the second value of the first tag in TAGS named NAME."
  (cadr (car (nostr-event-tags-by-name tags name))))

(defun nostr-event-e-tags-by-marker (tags)
  "Return a plist mapping e-tag markers in TAGS to event id lists."
  (let (result)
    (dolist (tag (nostr-event-tags-by-name tags "e"))
      (when-let* ((id (nth 1 tag))
                  (marker (nth 3 tag)))
        (setq result
              (plist-put result
                         (intern (concat ":" (downcase marker)))
                         (cons id (plist-get result
                                             (intern (concat ":" (downcase marker)))))))))
    result))

(defun nostr-event-root-id (event)
  "Return EVENT's NIP-10 root event id, when present."
  (let* ((tags (alist-get 'tags event))
         (markers (nostr-event-e-tags-by-marker tags)))
    (or (car (plist-get markers :root))
        (when-let* ((first-e (car (nostr-event-tags-by-name tags "e"))))
          (nth 1 first-e)))))

(defun nostr-event-reply-id (event)
  "Return EVENT's NIP-10 direct reply event id, when present."
  (let* ((tags (alist-get 'tags event))
         (markers (nostr-event-e-tags-by-marker tags)))
    (or (car (plist-get markers :reply))
        ;; NIP-10 deprecated positional scheme: with no :reply marker,
        ;; the last of >=2 plain (unmarked) e-tags is the reply (first is
        ;; root); a single e-tag is the root only, so reply stays nil.
        (let ((plain-e-tags
               (seq-filter (lambda (tag) (null (nth 3 tag)))
                           (nostr-event-tags-by-name tags "e"))))
          (when (>= (length plain-e-tags) 2)
            (nth 1 (car (last plain-e-tags))))))))

(defun nostr-event-quote-id (event)
  "Return EVENT's quoted event id, when present."
  (nostr-event-first-tag-value (alist-get 'tags event) "q"))

(defun nostr-event-reaction-event-id (event)
  "Return the event id targeted by reaction EVENT."
  (nostr-event-first-tag-value (alist-get 'tags event) "e"))

(defun nostr-event-repost-event-id (event)
  "Return the event id targeted by repost EVENT."
  (nostr-event-first-tag-value (alist-get 'tags event) "e"))

(defun nostr-event--number-string-p (value)
  "Return non-nil when VALUE is a non-negative integer string."
  (and (stringp value)
       (string-match-p "\\`[0-9]+\\'" value)))

(defun nostr-event-zap-target-event-id (event)
  "Return the note id targeted by zap receipt EVENT, when present."
  (or (nostr-event-first-tag-value (alist-get 'tags event) "e")
      (when-let* ((description (nostr-event-first-tag-value
                                (alist-get 'tags event)
                                "description"))
                  (request (ignore-errors
                             (json-parse-string description
                                                :object-type 'alist
                                                :array-type 'list
                                                :false-object nil))))
        (nostr-event-first-tag-value (alist-get 'tags request) "e"))))

(defun nostr-event-zap-amount-msats (event)
  "Return zap receipt EVENT amount in millisats, or nil when unavailable.
NIP-57 receipts carry the amount in the JSON zap request stored in the
description tag.  Some relays also include a direct amount tag on the receipt,
so that is accepted as a fallback."
  (let* ((tags (alist-get 'tags event))
         (direct (nostr-event-first-tag-value tags "amount"))
         (description (nostr-event-first-tag-value tags "description"))
         (request (and description
                       (ignore-errors
                         (json-parse-string description
                                            :object-type 'alist
                                            :array-type 'list
                                            :false-object nil))))
         (from-description (and request
                                (nostr-event-first-tag-value
                                 (alist-get 'tags request)
                                 "amount")))
         (amount (or from-description direct)))
    (when (nostr-event--number-string-p amount)
      (string-to-number amount))))

(defun nostr-event-mentioned-pubkeys (event)
  "Return public keys mentioned by EVENT's p-tags."
  (delq nil
        (mapcar #'cadr (nostr-event-tags-by-name (alist-get 'tags event) "p"))))

(defun nostr-event-build-reply-tags (reply-to primary-relay)
  "Build NIP-10 reply tags for REPLY-TO using PRIMARY-RELAY."
  (when reply-to
    (let* ((root-id (or (alist-get 'root-id reply-to)
                        (nostr-event-root-id reply-to)))
           (reply-id (alist-get 'id reply-to))
           (pubkey (alist-get 'pubkey reply-to))
           (tags nil))
      (if root-id
          (progn
            (push `("e" ,root-id ,primary-relay "root") tags)
            (push `("e" ,reply-id ,primary-relay "reply") tags))
        (push `("e" ,reply-id ,primary-relay "root") tags))
      (push `("p" ,pubkey) tags)
      (nreverse tags))))

(defun nostr-event-build-reaction-tags (react-to primary-relay)
  "Build NIP-25 reaction tags for REACT-TO using PRIMARY-RELAY."
  `(("e" ,(alist-get 'id react-to) ,(or (alist-get 'relay react-to) primary-relay))
    ("p" ,(alist-get 'pubkey react-to) ,(or (alist-get 'relay react-to) primary-relay))
    ("k" ,(number-to-string (or (alist-get 'kind react-to) nostr-kind-text-note)))))

(defun nostr-event-build-repost-tags (repost primary-relay)
  "Build NIP-18 repost tags for REPOST using PRIMARY-RELAY."
  `(("e" ,(alist-get 'id repost) ,(or (alist-get 'relay repost) primary-relay))
    ("p" ,(alist-get 'pubkey repost))))

(defconst nostr-event-media-url-regexp
  "\\bhttps?://[^][(){}<>[:space:]\"]+\\.\\(png\\|jpe?g\\|gif\\|webp\\|mp4\\)\\(?:?[^\n[:space:]]*\\)?"
  "Regexp matching media URLs that can be represented in note cards.")

(defconst nostr-event-nevent-regexp
  "\\(?:nostr:\\)?nevent1[023456789acdefghjklmnpqrstuvwxyz]+"
  "Regexp matching NIP-19 nevent identifiers in note content.")

(defun nostr-event-media-type (url)
  "Return media type symbol for URL."
  (cond
   ((and (stringp url)
         (string-match-p "\\.mp4\\(?:[?].*\\)?\\'" url))
    'video)
   ((and (stringp url)
         (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|webp\\)\\(?:[?].*\\)?\\'" url))
    'image)))

(defun nostr-event-media-items (content)
  "Return media item alists found in CONTENT.
Each item has `url' and `type' keys.  Video items are represented as external
media and are not downloaded for inline image display."
  (delq nil
        (mapcar (lambda (url)
                  (when-let* ((type (nostr-event-media-type url)))
                    `((url . ,url) (type . ,type))))
                (nostr-event-media-urls content))))

(defun nostr-event-media-urls (content)
  "Return media URLs found in CONTENT."
  (let (urls)
    (with-temp-buffer
      (insert (or content ""))
      (goto-char (point-min))
      (while (re-search-forward nostr-event-media-url-regexp nil t)
        (push (match-string-no-properties 0) urls)))
    (nreverse urls)))

(defun nostr-event-nevents (content)
  "Return NIP-19 nevent identifiers found in CONTENT."
  (let (values)
    (with-temp-buffer
      (insert (or content ""))
      (goto-char (point-min))
      (while (re-search-forward nostr-event-nevent-regexp nil t)
        (push (match-string-no-properties 0) values)))
    (delete-dups (nreverse values))))

(defun nostr-event-normalize (event relay)
  "Return EVENT alist with derived fields and RELAY."
  (let ((copy (copy-alist event)))
    (setf (alist-get 'relay copy nil nil #'eq) relay)
    (setf (alist-get 'root-id copy nil nil #'eq) (nostr-event-root-id copy))
    (setf (alist-get 'reply-id copy nil nil #'eq) (nostr-event-reply-id copy))
    (setf (alist-get 'quote-id copy nil nil #'eq) (nostr-event-quote-id copy))
    copy))

(provide 'nostr-event)
;;; nostr-event.el ends here
