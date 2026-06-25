;;; nostr-daemon.el --- Long-running relay daemon driver -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Driver for the Rust `relay-daemon' subcommand of the nostr-el backend.
;;
;; The daemon owns every relay websocket: it connects, subscribes on our
;; behalf, verifies signatures, and writes the SQLite cache directly. Emacs no
;; longer parses EVENT frames or verifies events. This module only:
;;
;;   * launches and tears down the daemon process,
;;   * forwards subscribe/close/relay control commands over stdin, and
;;   * reads the daemon's newline-delimited stdout notifications, refreshing
;;     the UI *from the database* (debounced) when new events land.
;;
;; The frontend therefore reflects relay traffic purely by re-reading the
;; cache, matching the project's "Emacs refreshes from DB" model.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'nostr-backend)

(defcustom nostr-daemon-refresh-debounce 0.3
  "Seconds to coalesce database refreshes after daemon `stored' events.
A short debounce keeps a burst of incoming events from triggering one redisplay
per event during initial sync."
  :type 'number
  :group 'nostr)

(defvar nostr-daemon--process nil
  "The running relay-daemon process, or nil.")

(defvar nostr-daemon--stdout-acc ""
  "Accumulator for partial stdout lines from the daemon.")

(defvar nostr-daemon--refresh-timer nil
  "Pending debounced refresh timer, or nil.")

(defvar nostr-daemon-event-hook nil
  "Hook run for every daemon stdout notification.
Each function receives the parsed alist (with an `event' key naming the kind:
`stored', `eose', `relay-status', `notice', `closed', `error', or `ready').")

(defvar nostr-daemon-refresh-hook nil
  "Hook run, debounced, after the daemon stores new events.
Handlers should re-read the cache and refresh whatever views they own.")

(defun nostr-daemon-running-p ()
  "Return non-nil when the daemon process is live."
  (and nostr-daemon--process
       (process-live-p nostr-daemon--process)))

;;;; Process lifecycle

(defun nostr-daemon-start (db-path relays &optional current-pubkey)
  "Start the relay daemon writing to DB-PATH, connecting to RELAYS.
CURRENT-PUBKEY, when non-nil, lets the daemon derive notification rows.  A
daemon already running is reused as-is; call `nostr-daemon-stop' first to
restart with different bootstrap configuration."
  (unless (nostr-daemon-running-p)
    (setq nostr-daemon--stdout-acc "")
    (let* ((stderr (generate-new-buffer " *nostr-daemon-err*"))
           (proc (make-process
                  :name "nostr-relay-daemon"
                  :buffer (generate-new-buffer " *nostr-daemon-out*")
                  :command (list nostr-backend-command "relay-daemon")
                  :stderr stderr
                  :noquery t
                  :connection-type 'pipe
                  :filter #'nostr-daemon--filter
                  :sentinel #'nostr-daemon--sentinel)))
      (setq nostr-daemon--process proc)
      ;; First stdin line is the bootstrap config; subsequent lines are commands.
      (nostr-daemon--send
       `((db_path . ,(expand-file-name db-path))
         (relays . ,(or (vconcat relays) []))
         (current_pubkey . ,(or current-pubkey :null))))
      proc)))

(defun nostr-daemon-stop ()
  "Stop the relay daemon if running."
  (when (nostr-daemon-running-p)
    ;; Ask for a clean shutdown; the sentinel clears state when it exits.
    (ignore-errors (nostr-daemon--send '((op . "shutdown"))))
    (ignore-errors (process-send-eof nostr-daemon--process)))
  (when nostr-daemon--refresh-timer
    (cancel-timer nostr-daemon--refresh-timer)
    (setq nostr-daemon--refresh-timer nil)))

(defun nostr-daemon--sentinel (process _event)
  "Reset daemon state when PROCESS exits."
  (when (memq (process-status process) '(exit signal))
    (when (eq process nostr-daemon--process)
      (setq nostr-daemon--process nil
            nostr-daemon--stdout-acc ""))))

;;;; Control commands

(defun nostr-daemon--send (command)
  "Encode COMMAND alist as one JSON line and write it to the daemon."
  (when (nostr-daemon-running-p)
    (process-send-string
     nostr-daemon--process
     (concat (nostr-backend--json-encode command) "\n"))))

(defun nostr-daemon-subscribe (id filters)
  "Open (or replace) subscription ID with FILTERS on every relay.
FILTERS is a list of nostr filter alists (array values must be vectors)."
  (nostr-daemon--send `((op . "subscribe")
                        (id . ,id)
                        (filters . ,(or (vconcat filters) [])))))

(defun nostr-daemon-close (id)
  "Close subscription ID on every relay."
  (nostr-daemon--send `((op . "close") (id . ,id))))

(defun nostr-daemon-add-relay (url)
  "Connect the daemon to relay URL."
  (nostr-daemon--send `((op . "add-relay") (url . ,url))))

(defun nostr-daemon-remove-relay (url)
  "Disconnect the daemon from relay URL."
  (nostr-daemon--send `((op . "remove-relay") (url . ,url))))

(defun nostr-daemon-set-pubkey (pubkey)
  "Tell the daemon to derive notifications for PUBKEY (or nil to disable)."
  (nostr-daemon--send `((op . "set-pubkey") (pubkey . ,(or pubkey :null)))))

;;;; stdout parsing

(defun nostr-daemon--filter (process chunk)
  "Process daemon CHUNK from PROCESS, dispatching complete JSON lines."
  (when (eq process nostr-daemon--process)
    (setq nostr-daemon--stdout-acc (concat nostr-daemon--stdout-acc chunk))
    ;; Process every complete newline-terminated line, keeping the remainder.
    (let ((start 0) newline)
      (while (setq newline (string-search "\n" nostr-daemon--stdout-acc start))
        (let ((line (substring nostr-daemon--stdout-acc start newline)))
          (setq start (1+ newline))
          (unless (string-empty-p (string-trim line))
            (nostr-daemon--handle-line line))))
      (setq nostr-daemon--stdout-acc (substring nostr-daemon--stdout-acc start)))))

(defun nostr-daemon--handle-line (line)
  "Parse one notification LINE and dispatch it."
  (when-let* ((notification (ignore-errors (nostr-backend--json-read line))))
    (run-hook-with-args 'nostr-daemon-event-hook notification)
    ;; A freshly stored event means the cache changed; coalesce refreshes so a
    ;; sync burst yields one redisplay, not one per event.
    (when (equal (alist-get 'event notification) "stored")
      (nostr-daemon--schedule-refresh))))

(defun nostr-daemon--schedule-refresh ()
  "Arm the debounced database-refresh timer if not already pending."
  (unless (timerp nostr-daemon--refresh-timer)
    (setq nostr-daemon--refresh-timer
          (run-at-time nostr-daemon-refresh-debounce nil
                       #'nostr-daemon--run-refresh))))

(defun nostr-daemon--run-refresh ()
  "Run the database-refresh hook."
  (setq nostr-daemon--refresh-timer nil)
  (run-hooks 'nostr-daemon-refresh-hook))

(provide 'nostr-daemon)
;;; nostr-daemon.el ends here
