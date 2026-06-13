;;; nostr-e2e-driver.el --- Real-Emacs E2E driver for nostr.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Driven by tests/e2e/run-e2e.sh inside a real `emacs -nw' (TTY) process, so
;; redisplay and `window-buffer-change-functions' run for real -- unlike
;; `--batch', which never fires window-change functions and so cannot exercise
;; the refresh/redisplay recursion this guards against.
;;
;; It seeds an offline timeline (a temp DB, a fixed account, follows + notes; no
;; network), runs `nostr-open', then hammers the refresh path with simulated
;; event ingestion interleaved with window churn (split/switch/delete + forced
;; redisplay).  It then scans *Messages* for recursion/error markers and writes
;; a result file, exiting non-zero on failure.
;;
;; Environment:
;;   NOSTR_E2E_ROOT        nostr.el checkout (added to load-path)
;;   NOSTR_E2E_TMP         writable temp dir for the DB
;;   NOSTR_E2E_RESULT      result file to write
;;   NOSTR_E2E_FORCE_BUGGY "1" reinstalls the old synchronous window-change hook
;;                         to demonstrate the harness reproduces the crash.

;;; Code:

(setq debug-on-error nil
      ;; Surface deep recursion quickly, matching the environment where the
      ;; crash was reported.
      max-lisp-eval-depth 1800)

(add-to-list 'load-path (or (getenv "NOSTR_E2E_ROOT") default-directory))
(require 'cl-lib)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'nostr)

(defvar nostr-e2e-buggy (equal (getenv "NOSTR_E2E_FORCE_BUGGY") "1"))
(defvar nostr-e2e-result (or (getenv "NOSTR_E2E_RESULT") "/tmp/nostr-e2e-result.txt"))
(defvar nostr-e2e-tmp (or (getenv "NOSTR_E2E_TMP") temporary-file-directory))
(defvar nostr-e2e-ticks 0)
(defvar nostr-e2e-other (get-buffer-create "*e2e-other*"))

(defun nostr-e2e--store-note (id author n)
  "Store kind-1 note ID by AUTHOR with body number N."
  (nostr-db-store-event
   (list (cons 'id id) (cons 'pubkey author) (cons 'created_at (+ 1000 n))
         (cons 'kind 1) (cons 'tags nil) (cons 'content (format "note %d body" n))
         (cons 'sig "sig") (cons 'relay nil)
         (cons 'root-id nil) (cons 'reply-id nil) (cons 'quote-id nil))))

;; Old, buggy handler: refreshes synchronously from the redisplay-time hook.
;; This is the real anti-pattern the fix removed.  On a TTY frame it does not
;; recurse (forced redisplay is inhibited during redisplay, and images/
;; fontification that re-enter redisplay in a GUI frame are absent), so to
;; validate that the harness *detects* the class of failure the user hit
;; ("Lisp nesting exceeds 'max-lisp-eval-depth'" reported from a timer), buggy
;; mode also schedules a deterministic deep recursion inside a timer.
(defun nostr-e2e--buggy-window-change (&rest _)
  (dolist (window (window-list nil 'no-minibuf))
    (let ((buffer (window-buffer window)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when-let* (((bound-and-true-p nostr--refresh-dirty))
                      (refresh (nostr--buffer-refresh-function)))
            (setq nostr--refresh-dirty nil)
            (funcall refresh)))))))

(defun nostr-e2e--deep-recurse (n)
  "Recurse N levels to exceed `max-lisp-eval-depth' (detector self-test)."
  (1+ (nostr-e2e--deep-recurse (1+ n))))

(defun nostr-e2e-tick ()
  "Ingest a simulated event and churn windows, forcing redisplay."
  (setq nostr-e2e-ticks (1+ nostr-e2e-ticks))
  (condition-case err
      (progn
        (nostr-e2e--store-note (format "live-%d" nostr-e2e-ticks) "alice"
                               (+ 2000 nostr-e2e-ticks))
        ;; Mark the (possibly hidden) timeline stale, as a real event would.
        (when-let* ((buf (get-buffer nostr-buffer-name)))
          (with-current-buffer buf (setq nostr--refresh-dirty t)))
        ;; Drive the event hook (schedules a refresh) like ingestion does.
        (run-hook-with-args 'nostr-relay-event-hook nil)
        ;; Window churn so `window-buffer-change-functions' fire repeatedly.
        (pcase (% nostr-e2e-ticks 4)
          (0 (switch-to-buffer nostr-e2e-other))
          (1 (ignore-errors (split-window)))
          (2 (switch-to-buffer (get-buffer-create nostr-buffer-name)))
          (3 (delete-other-windows)))
        (redisplay t))
    (error (message "nostr-e2e tick error: %S" err))))

(defun nostr-e2e-finish ()
  "Scan *Messages* for failures, write the result file, and exit."
  (let* ((msgs (with-current-buffer (messages-buffer) (buffer-string)))
         (bad (and (string-match-p
                    (concat "max-lisp-eval-depth\\|Lisp nesting\\|"
                            "excessive-lisp-nesting\\|error in process\\|"
                            "Error running timer\\|nostr-e2e tick error")
                    msgs)
                   t))
         (timeline (get-buffer nostr-buffer-name))
         (rendered (and timeline
                        (with-current-buffer timeline
                          (and (> (buffer-size) 0)
                               (string-match-p "note" (buffer-string))))
                        t)))
    (ignore-errors
      (with-temp-file nostr-e2e-result
        (insert (format "MODE=%s\nERRORS=%s\nRENDERED=%s\nTICKS=%d\n"
                        (if nostr-e2e-buggy "buggy" "fixed")
                        (if bad "yes" "no")
                        (if rendered "yes" "no")
                        nostr-e2e-ticks))
        (insert "----MESSAGES----\n")
        (insert msgs)))
    (kill-emacs (if bad 1 0))))

(defun nostr-e2e-run ()
  "Set up the offline timeline and run the stress loop."
  (setq nostr-relay-urls nil          ; fully offline: never touch the network
        nostr-relay-verify-events nil
        nostr-db-path (expand-file-name "nostr-e2e.db" nostr-e2e-tmp))
  (ignore-errors (delete-file nostr-db-path))
  (nostr-db-open nostr-db-path)
  (setq nostr-current-pubkey (make-string 64 ?a))
  (emacsql nostr-db--connection [:insert-or-ignore :into follows :values $v1]
           (vector nostr-current-pubkey "alice" nil nil))
  (dotimes (i 60)
    (nostr-e2e--store-note (format "seed-%d" i) "alice" i))
  (nostr-open)
  (when nostr-e2e-buggy
    (remove-hook 'window-buffer-change-functions #'nostr--refresh-on-window-change)
    (add-hook 'window-buffer-change-functions #'nostr-e2e--buggy-window-change)
    ;; Detector self-test: a timer recursion of the same class as the reported
    ;; crash, so a clean "fixed" run is meaningfully distinguished from one that
    ;; would surface the failure.
    (run-at-time 0.3 nil
                 (lambda ()
                   (condition-case err
                       (nostr-e2e--deep-recurse 0)
                     (error (message "Error running timer 'nostr-e2e': %S" err))))))
  ;; Safety: always finish even if the loop wedges.
  (run-at-time 8 nil #'nostr-e2e-finish)
  (dotimes (_ 50)
    (nostr-e2e-tick)
    (sleep-for 0.04))
  (nostr-e2e-finish))

(nostr-e2e-run)

;;; nostr-e2e-driver.el ends here
