;;; nostr-compose.el --- Nostr compose buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Compose and send public notes.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'nostr-backend)
(require 'nostr-db)
(require 'nostr-event)
(require 'nostr-relay)
(require 'transient)

(defvar-local nostr-compose-reply-to nil
  "Event being replied to.")

(defvar-local nostr-compose-extra-tags nil
  "Extra tags to add to the note being composed.")

(defvar-local nostr-compose-dirty nil
  "Whether compose content has changed since the buffer opened.")

(defvar-local nostr-compose--sent nil
  "Whether the current compose buffer has been successfully sent.")

(defvar nostr-actions-after-send-hook nil
  "Hook run with the signed event after a public action is sent.")

(defvar nostr-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'nostr-compose-send)
    (define-key map (kbd "C-c C-k") #'nostr-compose-cancel)
    (define-key map (kbd "?") #'nostr-compose-actions)
    map)
  "Keymap for `nostr-compose-mode'.")

(define-derived-mode nostr-compose-mode text-mode "Nostr-Compose"
  "Mode for writing a public Nostr note."
  (setq-local header-line-format '(:eval (nostr-compose--header-line)))
  (add-hook 'after-change-functions #'nostr-compose--after-change nil t)
  (add-hook 'kill-buffer-query-functions #'nostr-compose--confirm-kill nil t))

(transient-define-prefix nostr-compose-actions ()
  "Actions for the current Nostr compose buffer."
  [["Compose"
    ("C-c C-c" "Send" nostr-compose-send)
    ("C-c C-k" "Cancel" nostr-compose-cancel)]])

(defun nostr-compose--after-change (&rest _ignored)
  "Mark current compose buffer as dirty after content changes."
  (setq nostr-compose-dirty t))

(defun nostr-compose--content ()
  "Return compose buffer content, ignoring comment lines."
  (string-trim
   (mapconcat #'identity
              (seq-remove
               (lambda (line) (string-prefix-p ";;" (string-trim-left line)))
               (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
              "\n")))

(defun nostr-compose--target-label (event)
  "Return compact label for target EVENT."
  (or (alist-get 'author event)
      (alist-get 'pubkey event)
      (alist-get 'id event)
      "unknown"))

(defun nostr-compose--quote-event-id ()
  "Return quoted event id from `nostr-compose-extra-tags'."
  (cadr (car (nostr-event-tags-by-name nostr-compose-extra-tags "q"))))

(defun nostr-compose--quote-event ()
  "Return cached quoted event for compose context."
  (when-let* ((quote-id (nostr-compose--quote-event-id)))
    (car (nostr-db-select-thread quote-id))))

(defun nostr-compose--context-lines ()
  "Return context comment lines for the current compose buffer."
  (append
   (when nostr-compose-reply-to
     (list (format ";; Replying to %s"
                   (nostr-compose--target-label nostr-compose-reply-to))
           (format ";; %s"
                   (truncate-string-to-width
                    (or (alist-get 'content nostr-compose-reply-to) "")
                    100 nil nil t))))
   (when-let* ((quote-id (nostr-compose--quote-event-id)))
     (let ((quote (nostr-compose--quote-event)))
       (list (format ";; Quoting %s"
                     (if quote
                         (nostr-compose--target-label quote)
                       quote-id))
             (format ";; %s"
                     (truncate-string-to-width
                      (if quote
                          (or (alist-get 'content quote) "")
                        "(quote target is not cached)")
                      100 nil nil t)))))))

(defun nostr-compose--insert-context ()
  "Insert reply and quote context comments."
  (dolist (line (nostr-compose--context-lines))
    (insert line "\n"))
  (when (nostr-compose--context-lines)
    (insert "\n")))

(defun nostr-compose--header-line ()
  "Return compose header line text."
  (let* ((content (nostr-compose--content))
         (count (length content))
         (parts (delq nil
                      (list "Nostr Compose"
                            (when nostr-compose-reply-to
                              (format "reply: %s"
                                      (nostr-compose--target-label nostr-compose-reply-to)))
                            (when (nostr-compose--quote-event-id)
                              (format "quote: %s" (nostr-compose--quote-event-id)))
                            (format "%d chars" count)))))
    (string-join parts "  |  ")))

(defun nostr-compose--confirm-kill ()
  "Ask before killing a compose buffer with unsent content."
  (or nostr-compose--sent
      (not nostr-compose-dirty)
      (string-empty-p (nostr-compose--content))
      (yes-or-no-p "Discard unsent Nostr draft? ")))

(defun nostr-compose-open (&optional reply-to extra-tags)
  "Open a compose buffer, optionally replying to REPLY-TO."
  (interactive)
  (let ((buffer (generate-new-buffer "*Nostr Compose*")))
    (with-current-buffer buffer
      (nostr-compose-mode)
      (setq nostr-compose-reply-to reply-to)
      (setq nostr-compose-extra-tags extra-tags)
      (nostr-compose--insert-context)
      (setq nostr-compose-dirty nil))
    (switch-to-buffer buffer)))

(defun nostr-compose-cancel (&optional force)
  "Cancel the current compose buffer.
With FORCE, close without prompting."
  (interactive "P")
  (when (or force (nostr-compose--confirm-kill))
    (let ((nostr-compose--sent t))
      (kill-buffer (current-buffer)))
    (message "Nostr compose cancelled")))

(defun nostr-compose-send ()
  "Sign and send the current compose buffer."
  (interactive)
  (let* ((content (nostr-compose--content))
         (reply-to nostr-compose-reply-to)
         (tags (append (nostr-event-build-reply-tags reply-to (car nostr-relay-urls))
                       nostr-compose-extra-tags))
         (buffer (current-buffer)))
    (if (string-empty-p content)
        (user-error "Cannot send an empty Nostr note")
      (nostr-backend-sign-event
       nostr-kind-text-note
       tags
       content
       (lambda (response)
         (let* ((event (alist-get 'event response))
                (sent-count (nostr-relay-send-client-message
                             (alist-get 'client_message response))))
           (when event
             (nostr-db-store-event (nostr-event-normalize event nil))
             (run-hook-with-args 'nostr-actions-after-send-hook event))
           (message "Nostr note queued to %d relay%s"
                    sent-count
                    (if (= sent-count 1) "" "s")))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq nostr-compose--sent t))
           (kill-buffer buffer)))
       (lambda (response stderr _status)
         (message "Nostr signing failed: %s%s"
                  (or (alist-get 'message (alist-get 'error response)) "backend error")
                  (if (string-empty-p stderr) "" (concat ": " stderr))))))))

(provide 'nostr-compose)
;;; nostr-compose.el ends here
