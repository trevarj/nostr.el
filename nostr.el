;;; nostr.el --- Public Nostr client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr
;; Version: 0.2
;; Package-Requires: ((emacs "30.1")
;;                    (transient "0.3")
;;                    (emacsql "3.1.1")
;;                    (websocket "1.13")
;;                    (plz "0.9.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A public-social Nostr client for Emacs.  Emacs owns relay networking,
;; storage, and UI.  A small Rust CLI handles signing and protocol helpers.

;;; Code:

(require 'cl-lib)
(require 'nostr-backend)
(require 'nostr-actions)
(require 'nostr-db)
(require 'nostr-dispatch)
(require 'nostr-event)
(require 'nostr-media)
(require 'nostr-nip)
(require 'nostr-notifications)
(require 'nostr-profile)
(require 'nostr-relay)
(require 'nostr-relays)
(require 'nostr-search)
(require 'nostr-setup)
(require 'nostr-share)
(require 'nostr-timeline)
(require 'nostr-ui)

(defcustom nostr-db-path (expand-file-name "nostr-db.sqlite" user-emacs-directory)
  "Path to the Nostr SQLite cache."
  :type 'file
  :group 'nostr)

(defcustom nostr-debug-logging nil
  "Whether to emit Nostr debug messages."
  :type 'boolean
  :group 'nostr)

(defconst nostr-buffer-name "*Nostr*"
  "Main Nostr buffer name.")

(defvar nostr-current-pubkey nil
  "Current local account public key.")

(defvar nostr--refresh-timer nil
  "Pending debounced refresh timer.")

(defvar nostr--opening-account nil
  "Non-nil while `nostr-open' is deriving the account asynchronously.")

(defvar nostr--open-generation 0
  "Generation counter used to ignore stale async `nostr-open' callbacks.")

(defcustom nostr-refresh-debounce-seconds 0.4
  "Seconds to debounce UI refresh after relay events."
  :type 'number
  :group 'nostr)

(defun nostr-debug-message (fmt &rest args)
  "Emit debug message FMT with ARGS when logging is enabled."
  (when nostr-debug-logging
    (apply #'message (concat "[nostr] " fmt) args)))

(defun nostr--schedule-refresh (&rest _ignored)
  "Debounce refresh of visible Nostr buffers."
  (when (timerp nostr--refresh-timer)
    (cancel-timer nostr--refresh-timer))
  (setq nostr--refresh-timer
        (run-at-time nostr-refresh-debounce-seconds nil #'nostr-refresh-visible-buffers)))

(defun nostr-refresh-visible-buffers ()
  "Refresh live Nostr buffers."
  (setq nostr--refresh-timer nil)
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (pcase major-mode
          ('nostr-timeline-mode (nostr-timeline-refresh))
          ('nostr-thread-mode
           (when (fboundp 'nostr-thread-refresh)
             (nostr-thread-refresh)))
          ('nostr-profile-mode (nostr-profile-refresh))
          ('nostr-search-mode (nostr-search-refresh))
          ('nostr-notifications-mode (nostr-notifications-refresh))
          ('nostr-relays-mode (nostr-relays-refresh)))))))

(defun nostr--render-opening-buffer (buffer detail)
  "Render BUFFER as the main Nostr opening screen with DETAIL."
  (with-current-buffer buffer
    (nostr-timeline-mode)
    (setq-local nostr-timeline-current-pubkey nostr-current-pubkey)
    (let ((inhibit-read-only t))
      (nostr-ui-clear)
      (nostr-ui-insert-status-header "Feed" "Nostr" detail)
      (nostr-ui-insert-primary-nav nostr-ui-primary-nav-items 'feed)
      (nostr-ui-insert-empty-state
       "Preparing account."
       "Nostr is deriving the configured account pubkey in the background.")
      (nostr-ui-insert-footer '("S setup" "? actions"))
      (nostr-ui-finish-refresh nil))))

(defun nostr--finish-open-after-pubkey (buffer generation)
  "Finish opening BUFFER once `nostr-current-pubkey' is available."
  (when (= generation nostr--open-generation)
    (setq nostr--opening-account nil)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (nostr-timeline-mode)
        (setq-local nostr-timeline-current-pubkey nostr-current-pubkey)
        (nostr-timeline-refresh)))
    (nostr-relay-connect-all-deferred nostr-current-pubkey)))

(defun nostr--start-account-open (buffer)
  "Start asynchronous account setup for `nostr-open' and refresh BUFFER later."
  (unless nostr--opening-account
    (let ((generation nostr--open-generation))
      (setq nostr--opening-account t)
      (nostr-setup-derive-pubkey-async
       (lambda (_response)
         (run-at-time 0 nil #'nostr--finish-open-after-pubkey buffer generation))
       (lambda (message)
         (run-at-time
          0 nil
          (lambda ()
            (when (= generation nostr--open-generation)
              (setq nostr--opening-account nil)
              (when (buffer-live-p buffer)
                (nostr--render-opening-buffer
                 buffer
                 (format "Account setup failed: %s" message)))))))))))

;;;###autoload
(defun nostr-open ()
  "Open the main Nostr client buffer."
  (interactive)
  (unless nostr-db--connection
    (nostr-db-open nostr-db-path))
  (cl-incf nostr--open-generation)
  (add-hook 'nostr-relay-event-hook #'nostr--schedule-refresh)
  (let ((buffer (get-buffer-create nostr-buffer-name)))
    (if nostr-current-pubkey
        (with-current-buffer buffer
          (nostr-timeline-mode)
          (setq-local nostr-timeline-current-pubkey nostr-current-pubkey)
          (nostr-timeline-refresh))
      (nostr--render-opening-buffer buffer "Deriving configured account pubkey."))
    (switch-to-buffer buffer)
    (if nostr-current-pubkey
        (nostr-relay-connect-all-deferred nostr-current-pubkey)
      (nostr--start-account-open buffer))))

;;;###autoload
(defun nostr-close ()
  "Close relay connections and the database."
  (interactive)
  (when (timerp nostr--refresh-timer)
    (cancel-timer nostr--refresh-timer)
    (setq nostr--refresh-timer nil))
  (setq nostr--opening-account nil)
  (remove-hook 'nostr-relay-event-hook #'nostr--schedule-refresh)
  (cl-incf nostr--open-generation)
  (nostr-relay-disconnect-all)
  (nostr-db-close))

;;;###autoload
(defun nostr-reset-db ()
  "Reset the Nostr cache database."
  (interactive)
  (nostr-db-reset nostr-db-path))

(defalias 'nostr-create-note #'nostr-compose-open)
(defalias 'nostr-refresh #'nostr-refresh-visible-buffers)
(defalias 'nostr-open-profile #'nostr-profile-open)
(defalias 'nostr-search #'nostr-search-open)
(defalias 'nostr-open-notifications #'nostr-notifications-open)
(defalias 'nostr-open-relays #'nostr-relays-open)
(defalias 'nostr-open-id #'nostr-open-identifier)
(defalias 'nostr-check-backend #'nostr-setup-check-backend)
(defalias 'nostr-account-status #'nostr-setup-status)

(provide 'nostr)
;;; nostr.el ends here
