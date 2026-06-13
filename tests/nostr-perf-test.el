;;; nostr-perf-test.el --- Tests for nostr.el sync/refresh performance -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp, nostr

;;; Commentary:

;; Tests for the responsiveness work: refresh throttling/visibility gating
;; (Stage 1) and batched database loads (Stage 2).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'nostr)

;;; Stage 1 --- syncing flag lifecycle

(defmacro nostr-perf-test--with-clean-sync (&rest body)
  "Run BODY with isolated relay syncing state."
  (declare (indent 0))
  `(let ((nostr-relay--syncing-subs (make-hash-table :test #'equal))
         (nostr-relay--sync-timeout-timer nil)
         (nostr-relay-sync-finished-hook nil))
     (unwind-protect
         (progn ,@body)
       (when (timerp nostr-relay--sync-timeout-timer)
         (cancel-timer nostr-relay--sync-timeout-timer)))))

(ert-deftest nostr-feed-subscription-sets-and-clears-syncing ()
  "A personal feed subscription marks syncing until its EOSE arrives."
  (nostr-perf-test--with-clean-sync
    (should-not (nostr-relay-syncing-p))
    (nostr-relay--note-feed-subscription "wss://r" "personal-abc")
    (should (nostr-relay-syncing-p))
    (should (timerp nostr-relay--sync-timeout-timer))
    (nostr-relay--note-feed-eose "wss://r" "personal-abc")
    (should-not (nostr-relay-syncing-p))
    (should-not (timerp nostr-relay--sync-timeout-timer))))

(ert-deftest nostr-non-feed-subscription-does-not-sync ()
  "Non-feed subscriptions (e.g. profile metadata) do not mark syncing."
  (nostr-perf-test--with-clean-sync
    (nostr-relay--note-feed-subscription "wss://r" "profile-abc")
    (should-not (nostr-relay-syncing-p))
    (should-not (timerp nostr-relay--sync-timeout-timer))))

(ert-deftest nostr-sync-finished-hook-runs-once-when-all-eose ()
  "The finished hook runs exactly once, only after every feed sub EOSEs."
  (nostr-perf-test--with-clean-sync
    (let ((calls 0))
      (add-hook 'nostr-relay-sync-finished-hook (lambda () (cl-incf calls)))
      (nostr-relay--note-feed-subscription "wss://r" "personal-abc")
      (nostr-relay--note-feed-subscription "wss://r" "follows-def")
      (nostr-relay--note-feed-eose "wss://r" "personal-abc")
      (should (nostr-relay-syncing-p))
      (should (= calls 0))
      (nostr-relay--note-feed-eose "wss://r" "follows-def")
      (should-not (nostr-relay-syncing-p))
      (should (= calls 1)))))

(ert-deftest nostr-clear-syncing-safety-path-runs-hook ()
  "Force-clearing syncing (the EOSE-never-arrives safety path) runs the hook."
  (nostr-perf-test--with-clean-sync
    (let ((calls 0))
      (add-hook 'nostr-relay-sync-finished-hook (lambda () (cl-incf calls)))
      (nostr-relay--note-feed-subscription "wss://r" "personal-abc")
      (nostr-relay--clear-syncing)
      (should-not (nostr-relay-syncing-p))
      (should (= calls 1)))))

(ert-deftest nostr-disconnect-all-clears-syncing-without-hook ()
  "Disconnecting resets syncing state but does not fire the finished hook."
  (nostr-perf-test--with-clean-sync
    (let ((calls 0)
          (nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-relay--connecting (make-hash-table :test #'equal))
          (nostr-relay--subscriptions (make-hash-table :test #'equal))
          (nostr-relay--profile-requests (make-hash-table :test #'equal))
          (nostr-relay--profile-request-counts (make-hash-table :test #'equal))
          (nostr-relay--profile-request-subscriptions (make-hash-table :test #'equal))
          (nostr-relay--event-metadata-requests (make-hash-table :test #'equal))
          (nostr-relay--event-id-requests (make-hash-table :test #'equal))
          (nostr-relay--connect-queue nil)
          (nostr-relay--connect-timer nil)
          (nostr-relay--activity-timer nil))
      (add-hook 'nostr-relay-sync-finished-hook (lambda () (cl-incf calls)))
      (nostr-relay--note-feed-subscription "wss://r" "personal-abc")
      (should (nostr-relay-syncing-p))
      (nostr-relay-disconnect-all)
      (should-not (nostr-relay-syncing-p))
      (should (= calls 0)))))

;;; Stage 1 --- refresh scheduling

(defmacro nostr-perf-test--with-clean-refresh (&rest body)
  "Run BODY with isolated refresh-scheduler state."
  (declare (indent 0))
  `(let ((nostr-relay--syncing-subs (make-hash-table :test #'equal))
         (nostr--refresh-timer nil)
         (nostr--refresh-pending-since nil))
     (unwind-protect
         (progn ,@body)
       (when (timerp nostr--refresh-timer)
         (cancel-timer nostr--refresh-timer)))))

(ert-deftest nostr-schedule-refresh-throttles-during-sync ()
  "While syncing, repeated events do not reschedule the pending refresh."
  (nostr-perf-test--with-clean-refresh
    (puthash "k" t nostr-relay--syncing-subs)
    (should (nostr-relay-syncing-p))
    (nostr--schedule-refresh)
    (let ((first nostr--refresh-timer))
      (should (timerp first))
      (nostr--schedule-refresh)
      (nostr--schedule-refresh)
      (should (eq nostr--refresh-timer first)))))

(ert-deftest nostr-schedule-refresh-debounces-in-steady-state ()
  "In steady state, a new event reschedules (debounces) the refresh."
  (nostr-perf-test--with-clean-refresh
    (should-not (nostr-relay-syncing-p))
    (nostr--schedule-refresh)
    (let ((first nostr--refresh-timer))
      (should (timerp first))
      (nostr--schedule-refresh)
      (should (timerp nostr--refresh-timer))
      (should-not (eq nostr--refresh-timer first)))))

;;; Stage 1 --- visibility gating

(ert-deftest nostr-refresh-skips-invisible-buffers ()
  "Buffers with no visible window are marked dirty, not refreshed."
  (let ((refreshed 0)
        (visible nil))
    (cl-letf (((symbol-function 'nostr-timeline-refresh)
               (lambda () (cl-incf refreshed)))
              ((symbol-function 'get-buffer-window)
               (lambda (&rest _) visible)))
      (with-temp-buffer
        (nostr-timeline-mode)
        (setq visible nil)
        (nostr-refresh-visible-buffers)
        (should (= refreshed 0))
        (should nostr--refresh-dirty)
        (setq visible t)
        (nostr-refresh-visible-buffers)
        (should (= refreshed 1))
        (should-not nostr--refresh-dirty)))))

(ert-deftest nostr-window-change-repaints-dirty-buffer ()
  "A dirty buffer that becomes visible is repainted once."
  (let ((refreshed 0))
    (with-temp-buffer
      (nostr-timeline-mode)
      (setq nostr--refresh-dirty t)
      (let ((buf (current-buffer)))
        (cl-letf (((symbol-function 'nostr-timeline-refresh)
                   (lambda () (cl-incf refreshed)))
                  ((symbol-function 'window-list)
                   (lambda (&rest _) (list 'fake-window)))
                  ((symbol-function 'window-buffer)
                   (lambda (&rest _) buf)))
          (nostr--refresh-on-window-change)
          (should (= refreshed 1))
          (should-not nostr--refresh-dirty))))))

(provide 'nostr-perf-test)
;;; nostr-perf-test.el ends here
