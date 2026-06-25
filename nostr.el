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
(require 'nostr-daemon)
(require 'nostr-db)
(require 'nostr-discover)
(require 'nostr-dispatch)
(require 'nostr-event)
(require 'nostr-media)
(require 'nostr-nip)
(require 'nostr-notifications)
(require 'nostr-profile)
(require 'nostr-reactions)
(require 'nostr-relay)
(require 'nostr-relays)
(require 'nostr-search)
(require 'nostr-setup)
(require 'nostr-share)
(require 'nostr-timeline)
(require 'nostr-upload)
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

(defvar nostr--refresh-pending-since nil
  "`float-time' when the currently pending refresh was first scheduled.")

(defvar-local nostr--refresh-dirty nil
  "Non-nil when this buffer skipped a refresh because it was not visible.
The buffer is repainted the next time it becomes visible.")

(defvar nostr--opening-account nil
  "Non-nil while `nostr-open' is deriving the account asynchronously.")

(defvar nostr--open-generation 0
  "Generation counter used to ignore stale async `nostr-open' callbacks.")

(defcustom nostr-refresh-debounce-seconds 0.8
  "Seconds to debounce UI refresh after relay events."
  :type 'number
  :group 'nostr)

(defcustom nostr-refresh-max-wait-seconds 3.0
  "Maximum seconds a pending UI refresh may be deferred by a steady event stream.
Once a refresh has been pending this long, the next event lets it fire instead
of pushing it out again, so a continuous stream cannot starve the refresh."
  :type 'number
  :group 'nostr)

(defcustom nostr-sync-refresh-interval 8.0
  "Long-interval partial-progress fallback during the initial sync.
While `nostr-relay-syncing-p' is non-nil, `nostr--schedule-refresh' does not
rebuild on every arriving event: the EOSE burst would just thrash the buffer.
The authoritative render happens once at sync-finished
(`nostr-relay-sync-finished-hook'); this interval is only the safety valve so a
slow or never-EOSE relay still paints partial progress periodically.  Verified
events are drained from a background ingestion queue regardless, so a slower
render here loses no data: `nostr-relay--clear-syncing' flushes the queue first."
  :type 'number
  :group 'nostr)

(defun nostr-debug-message (fmt &rest args)
  "Emit debug message FMT with ARGS when logging is enabled."
  (when nostr-debug-logging
    (apply #'message (concat "[nostr] " fmt) args)))

(defun nostr--syncing-p ()
  "Return non-nil while relays are still delivering the initial sync burst."
  (and (fboundp 'nostr-relay-syncing-p) (nostr-relay-syncing-p)))

(defun nostr--schedule-refresh (&rest _ignored)
  "Schedule a refresh of visible Nostr buffers, throttled during initial sync.
While syncing, do not rebuild per event: arm one wall-clock fallback timer at
`nostr-sync-refresh-interval' so a slow/never-EOSE relay still paints partial
progress; the authoritative render runs at sync-finished.  In steady state,
debounce by `nostr-refresh-debounce-seconds' of *idle* time (so a refresh never
interrupts typing/scrolling) but never defer a pending refresh longer than
`nostr-refresh-max-wait-seconds'."
  (if (nostr--syncing-p)
      ;; Initial-sync burst: leave any pending timer alone so it fires once per
      ;; fallback interval; only arm a new one when none is pending.
      (unless (timerp nostr--refresh-timer)
        (setq nostr--refresh-pending-since (float-time))
        (setq nostr--refresh-timer
              (run-at-time nostr-sync-refresh-interval nil
                           #'nostr-refresh-visible-buffers)))
    ;; Steady state: debounce on idle time so redisplay/input interleave, but
    ;; coalesce so a steady stream still refreshes (max-wait backstop).
    (let ((now (float-time)))
      (unless (timerp nostr--refresh-timer)
        (setq nostr--refresh-pending-since now))
      (unless (and nostr--refresh-pending-since
                   (>= (- now nostr--refresh-pending-since)
                       nostr-refresh-max-wait-seconds))
        (when (timerp nostr--refresh-timer)
          (cancel-timer nostr--refresh-timer))
        (setq nostr--refresh-timer
              (run-with-idle-timer nostr-refresh-debounce-seconds nil
                                   #'nostr-refresh-visible-buffers))))))

(defun nostr--buffer-refresh-function ()
  "Return the refresh command for the current buffer's Nostr mode, or nil."
  (pcase major-mode
    ('nostr-timeline-mode #'nostr-timeline-refresh)
    ('nostr-thread-mode (and (fboundp 'nostr-thread-refresh) #'nostr-thread-refresh))
    ('nostr-profile-mode #'nostr-profile-refresh)
    ('nostr-profile-list-mode #'nostr-profile-list-refresh)
    ('nostr-search-mode #'nostr-search-refresh)
    ('nostr-notifications-mode #'nostr-notifications-refresh)
    ('nostr-relays-mode #'nostr-relays-refresh)
    ('nostr-reactions-mode #'nostr-reactions-refresh)
    ('nostr-setup-mode #'nostr-setup-status-refresh)))

(defun nostr-visible-note-ids ()
  "Return rendered note ids from visible Nostr buffers."
  (delete-dups
   (apply #'append
          (delq nil
                (mapcar
                 (lambda (window)
                   (let ((buffer (window-buffer window)))
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (when (and (nostr--buffer-refresh-function)
                                    (fboundp 'nostr-ui-rendered-note-ids))
                           (nostr-ui-rendered-note-ids))))))
                 (window-list nil 'no-minibuf))))))

(defvar nostr--refreshing nil
  "Non-nil while `nostr-refresh-visible-buffers' is running.
Guards against re-entrant refreshes (e.g. a window-change handler firing during
a refresh), which would otherwise recurse through redisplay.")

(defun nostr-refresh-visible-buffers ()
  "Refresh Nostr buffers shown in a visible window.
Buffers with no visible window are marked dirty and repainted the next time
they become visible (see `nostr--refresh-on-window-change'), so off-screen
buffers do no rendering work during a sync burst."
  (setq nostr--refresh-timer nil
        nostr--refresh-pending-since nil)
  ;; Never refresh re-entrantly: refreshing mutates buffers and window points,
  ;; which can fire `window-buffer-change-functions' and call us again.
  (unless nostr--refreshing
    (let ((nostr--refreshing t))
      (dolist (buffer (buffer-list))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when-let* ((refresh (nostr--buffer-refresh-function)))
              (if (get-buffer-window buffer 'visible)
                  (progn
                    (setq nostr--refresh-dirty nil)
                    (funcall refresh))
                (setq nostr--refresh-dirty t)))))))))

(defun nostr--refresh-on-window-change (&rest _ignored)
  "Schedule a repaint when a visible Nostr buffer skipped a refresh while hidden.
Runs from `window-buffer-change-functions' during redisplay, so it must stay
cheap: it only inspects windows and schedules a deferred refresh, never
refreshing synchronously (which would re-enter redisplay and recurse)."
  (when (and (not nostr--refreshing)
             (not (timerp nostr--refresh-timer))
             (catch 'found
               (dolist (window (window-list nil 'no-minibuf))
                 (let ((buffer (window-buffer window)))
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (when (and (bound-and-true-p nostr--refresh-dirty)
                                  (nostr--buffer-refresh-function))
                         (throw 'found t))))))
               nil))
    (setq nostr--refresh-pending-since (float-time))
    (setq nostr--refresh-timer
          (run-at-time 0 nil #'nostr-refresh-visible-buffers))))

(defun nostr--render-opening-buffer (buffer detail)
  "Render BUFFER as the main Nostr opening screen with DETAIL."
  (with-current-buffer buffer
    (nostr-timeline-mode)
    (setq-local nostr-timeline-current-pubkey nostr-current-pubkey)
    (let ((inhibit-read-only t))
      (nostr-ui-clear)
      (nostr-ui-insert-status-header "Feed" nil detail)
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
  (add-hook 'nostr-actions-after-send-hook #'nostr--schedule-refresh)
  ;; Repaint on EOSE (a final render once the sync burst settles) and when a
  ;; previously-hidden Nostr buffer becomes visible again.
  (add-hook 'nostr-relay-sync-finished-hook #'nostr--schedule-refresh)
  (add-hook 'nostr-discover-finished-hook #'nostr--schedule-refresh)
  (add-hook 'window-buffer-change-functions #'nostr--refresh-on-window-change)
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
  (setq nostr--opening-account nil
        nostr--refresh-pending-since nil)
  (remove-hook 'nostr-relay-event-hook #'nostr--schedule-refresh)
  (remove-hook 'nostr-actions-after-send-hook #'nostr--schedule-refresh)
  (remove-hook 'nostr-relay-sync-finished-hook #'nostr--schedule-refresh)
  (remove-hook 'nostr-discover-finished-hook #'nostr--schedule-refresh)
  (remove-hook 'window-buffer-change-functions #'nostr--refresh-on-window-change)
  (cl-incf nostr--open-generation)
  ;; Flush any verified events still queued for background ingestion before
  ;; closing the DB, so nothing is dropped and no drain fires after teardown.
  (when (fboundp 'nostr-relay--flush-ingestion)
    (nostr-relay--flush-ingestion))
  (nostr-relay-disconnect-all)
  (nostr-db-close))

;;;###autoload
(defun nostr-reset-db ()
  "Reset the Nostr cache database."
  (interactive)
  (nostr-db-reset nostr-db-path)
  ;; Drop already-verified ids so a fresh cache re-stores events.
  (nostr-relay--reset-seen-events))

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
