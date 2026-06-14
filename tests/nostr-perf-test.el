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

(ert-deftest nostr-startup-since-falls-back-to-recent-window ()
  "Cold-cache startup filters still use a bounded recent since value."
  (let ((nostr-relay-startup-window-seconds 100)
        (now 1000.0))
    (cl-letf (((symbol-function 'float-time) (lambda () now))
              ((symbol-function 'nostr-db-oldest-latest-event-time)
               (lambda (&rest _) nil)))
      (should (= (nostr-relay--since-for-pubkeys '("alice" "bob")) 900)))))

(ert-deftest nostr-personal-filters-are-bounded ()
  "Startup personal subscriptions use bounded, kind-limited filters."
  (let ((nostr-relay-personal-limit 11)
        (nostr-relay-mentions-limit 7))
    (cl-letf (((symbol-function 'nostr-relay--since-for-pubkey)
               (lambda (&rest _) 123)))
      (let ((filters (nostr-relay--personal-filters "alice")))
        (should (= (length filters) 2))
        (should (equal (alist-get "since" (car filters) nil nil #'equal) 123))
        (should (equal (alist-get "limit" (car filters) nil nil #'equal) 11))
        (should (member nostr-kind-mute-list
                        (alist-get "kinds" (car filters) nil nil #'equal)))
        (should-not (member nostr-kind-zap-receipt
                            (alist-get "kinds" (car filters) nil nil #'equal)))
        (should (equal (alist-get "since" (cadr filters) nil nil #'equal) 123))
        (should (equal (alist-get "limit" (cadr filters) nil nil #'equal) 7))
        (should (member nostr-kind-reaction
                        (alist-get "kinds" (cadr filters) nil nil #'equal)))))))

(ert-deftest nostr-contact-filter-includes-mute-list ()
  "Contact refresh also keeps the current account's mute list fresh."
  (let ((filter (nostr-relay--contacts-filter "alice")))
    (should (member nostr-kind-contacts
                    (alist-get "kinds" filter nil nil #'equal)))
    (should (member nostr-kind-mute-list
                    (alist-get "kinds" filter nil nil #'equal)))
    (should (member nostr-kind-relay-list
                    (alist-get "kinds" filter nil nil #'equal)))))

(ert-deftest nostr-follows-filters-split-content-from-metadata ()
  "The follows feed avoids mixing metadata/zaps into the main content stream."
  (cl-letf (((symbol-function 'nostr-relay--since-for-pubkeys)
             (lambda (&rest _) 456)))
    (let ((content (nostr-relay--feed-filter '("alice" "bob")))
          (metadata (nostr-relay--follow-metadata-filter '("alice" "bob"))))
      (should (equal (alist-get "since" content nil nil #'equal) 456))
      (should-not (member nostr-kind-metadata
                          (alist-get "kinds" content nil nil #'equal)))
      (should-not (member nostr-kind-zap-receipt
                          (alist-get "kinds" content nil nil #'equal)))
      (should (equal (alist-get "kinds" metadata nil nil #'equal)
                     (list nostr-kind-metadata)))
      (should (equal (alist-get "limit" metadata nil nil #'equal)
                     nostr-relay-follow-metadata-limit)))))

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

(ert-deftest nostr-open-refreshes-after-local-actions ()
  "Opening Nostr wires locally sent actions into the refresh scheduler."
  (let ((nostr-actions-after-send-hook nil)
        (nostr-relay-event-hook nil)
        (nostr-relay-sync-finished-hook nil)
        (nostr-discover-finished-hook nil)
        (window-buffer-change-functions nil)
        (nostr-current-pubkey "me")
        (nostr-db--connection t))
    (cl-letf (((symbol-function 'nostr-timeline-refresh) #'ignore)
              ((symbol-function 'nostr-relay-connect-all-deferred) #'ignore)
              ((symbol-function 'nostr-relay-disconnect-all) #'ignore)
              ((symbol-function 'nostr-db-close) #'ignore)
              ((symbol-function 'switch-to-buffer) #'ignore))
      (nostr-open)
      (should (memq #'nostr--schedule-refresh nostr-actions-after-send-hook))
      (nostr-close)
      (should-not (memq #'nostr--schedule-refresh nostr-actions-after-send-hook)))))

;;; Stage 1 --- conservative global relay subscriptions

(ert-deftest nostr-global-filter-is-small-and-recent ()
  "The Global relay filter uses a narrow time window and small limit."
  (let ((nostr-relay-global-limit 7)
        (nostr-relay-global-window-seconds 60)
        (now 1000.0))
    (cl-letf (((symbol-function 'float-time) (lambda () now)))
      (should (equal (nostr-relay--global-filter)
                     `(("kinds" . (,nostr-kind-text-note))
                       ("since" . 940)
                       ("limit" . 7)))))))

(ert-deftest nostr-feed-refresh-does-not-query-global ()
  "Refreshing Feed closes any Global stream but does not issue a Global REQ."
  (let ((global-requests 0)
        (global-closes 0))
    (cl-letf (((symbol-function 'nostr-relay-subscribe-global)
               (lambda (&optional _force) (cl-incf global-requests)))
              ((symbol-function 'nostr-relay-close-global)
               (lambda () (cl-incf global-closes)))
              ((symbol-function 'nostr-db-select-account-feed)
               (lambda (&rest _) nil))
              ((symbol-function 'nostr-db-select-missing-repost-targets)
               (lambda (&rest _) nil))
              ((symbol-function 'nostr-relay-fetch-event-metadata)
               (lambda (&rest _) nil))
              ((symbol-function 'nostr-relay-fetch-events-by-id)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (nostr-timeline-mode)
        (setq-local nostr-timeline-current-pubkey "alice"
                    nostr-timeline-feed-kind 'feed)
        (nostr-timeline-refresh)
        (should (= global-requests 0))
        (should (= global-closes 1))))))

(ert-deftest nostr-global-refresh-queries-global-once ()
  "Refreshing Global issues one rate-limited Global relay request."
  (let ((global-requests 0))
    (cl-letf (((symbol-function 'nostr-relay-subscribe-global)
               (lambda (&optional _force) (cl-incf global-requests)))
              ((symbol-function 'nostr-db-select-global-feed)
               (lambda (&rest _) nil))
              ((symbol-function 'nostr-relay-fetch-event-metadata)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (nostr-timeline-mode)
        (setq-local nostr-timeline-current-pubkey "alice"
                    nostr-timeline-feed-kind 'global)
        (nostr-timeline-refresh)
        (should (= global-requests 1))))))

(ert-deftest nostr-global-subscribe-is-rate-limited ()
  "Repeated Global refreshes do not repeatedly query relays."
  (let ((nostr-relay--connections (make-hash-table :test #'equal))
        (nostr-relay--subscriptions (make-hash-table :test #'equal))
        (nostr-relay--global-last-request-time nil)
        (nostr-relay-global-refresh-interval 60)
        (sent 0)
        (now 1000.0))
    (puthash "wss://relay" 'fake nostr-relay--connections)
    (cl-letf (((symbol-function 'float-time) (lambda () now))
              ((symbol-function 'nostr-relay--send)
               (lambda (&rest _) (cl-incf sent))))
      (should (= (nostr-relay-subscribe-global) 1))
      (should (= sent 1))
      (setq now 1010.0)
      (should-not (nostr-relay-subscribe-global))
      (should (= sent 1))
      (setq now 1061.0)
      (should (= (nostr-relay-subscribe-global) 1))
      (should (= sent 2)))))

(ert-deftest nostr-visible-metadata-backfill-is-capped ()
  "Timeline refreshes backfill metadata for only the first visible slice."
  (let ((nostr-timeline-metadata-backfill-limit 2)
        profile-pubkeys
        metadata-ids)
    (cl-letf (((symbol-function 'nostr-relay-fetch-profile)
               (lambda (pubkey &rest _) (push pubkey profile-pubkeys)))
              ((symbol-function 'nostr-relay-fetch-event-metadata)
               (lambda (ids &rest _) (setq metadata-ids ids))))
      (nostr-timeline--backfill-visible-metadata
       '(((id . "1") (pubkey . "a"))
         ((id . "2") (pubkey . "b"))
         ((id . "3") (pubkey . "c"))))
      (should (equal (nreverse profile-pubkeys) '("a" "b")))
      (should (equal metadata-ids '("1" "2"))))))

(ert-deftest nostr-verify-finished-defers-queue-drain ()
  "Verification sentinels do not recursively start the next backend process."
  (let ((nostr-relay--verify-inflight 1)
        (nostr-relay--verify-queue (list (cons "wss://relay" '((id . "event")))))
        (nostr-relay--verify-drain-timer nil)
        (started 0))
    (unwind-protect
        (cl-letf (((symbol-function 'nostr-relay--start-verification)
                   (lambda (&rest _) (cl-incf started))))
          (nostr-relay--verify-finished)
          (should (= nostr-relay--verify-inflight 0))
          (should (= started 0))
          (should (timerp nostr-relay--verify-drain-timer))
          (cancel-timer nostr-relay--verify-drain-timer)
          (setq nostr-relay--verify-drain-timer nil)
          (nostr-relay--drain-verify-queue)
          (should (= started 1))
          (should-not nostr-relay--verify-queue))
      (when (timerp nostr-relay--verify-drain-timer)
        (cancel-timer nostr-relay--verify-drain-timer)))))

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

(ert-deftest nostr-window-change-schedules-deferred-refresh ()
  "The window-change handler defers a refresh; it never refreshes synchronously.
Refreshing synchronously inside the redisplay-time hook re-enters redisplay and
recurses, so the handler must only schedule a timer."
  (let ((nostr--refresh-timer nil)
        (nostr--refresh-pending-since nil)
        (nostr--refreshing nil)
        (refreshed 0))
    (unwind-protect
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
              ;; Deferred: a timer is scheduled, nothing refreshed synchronously.
              (should (timerp nostr--refresh-timer))
              (should (= refreshed 0)))))
      (when (timerp nostr--refresh-timer)
        (cancel-timer nostr--refresh-timer)))))

(ert-deftest nostr-refresh-visible-buffers-not-reentrant ()
  "A refresh requested while one is already running is a no-op (no recursion)."
  (let ((nostr--refreshing t)
        (nostr--refresh-timer nil)
        (nostr--refresh-pending-since nil)
        (refreshed 0))
    (cl-letf (((symbol-function 'nostr-timeline-refresh)
               (lambda () (cl-incf refreshed)))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) t)))
      (with-temp-buffer
        (nostr-timeline-mode)
        (nostr-refresh-visible-buffers)
        (should (= refreshed 0))))))

;;; Stage 2 --- batched database loads

(defmacro nostr-perf-test--with-db (&rest body)
  "Run BODY with an isolated in-memory Nostr database."
  (declare (indent 0))
  `(let ((nostr-db--connection (emacsql-sqlite-open nil)))
     (unwind-protect
         (progn (nostr-db-init) ,@body)
       (emacsql-close nostr-db--connection))))

(defun nostr-perf-test--insert-event (id pubkey &optional root-id reply-id)
  "Insert a minimal kind-1 event ID by PUBKEY with ROOT-ID/REPLY-ID."
  (emacsql nostr-db--connection
           [:insert-or-replace :into events
            [id pubkey created_at kind tags content sig relay root_id reply_id quote_id]
            :values [$s1 $s2 100 1 nil "" "sig" nil $s3 $s4 nil]]
           id pubkey root-id reply-id))

(ert-deftest nostr-db-event-counts-batch-counts-each-interaction ()
  "Batch counts match the seeded reactions/reposts/zaps/replies per id."
  (nostr-perf-test--with-db
    (nostr-perf-test--insert-event "n1" "author")
    (nostr-perf-test--insert-event "n2" "author")
    ;; n1: 2 reactions, 1 repost, 3 zaps (100+200+300 msats), 2 replies
    (dolist (r '(("react-1" "n1" "p" "+" 10) ("react-2" "n1" "q" "+" 11)))
      (emacsql nostr-db--connection [:insert :into reactions :values $v1] (vconcat r)))
    (emacsql nostr-db--connection [:insert :into reposts :values $v1]
             (vector "rp-1" "n1" "p" 12))
    (dolist (z '(("z1" "n1" "p" 100 13) ("z2" "n1" "q" 200 14) ("z3" "n1" "r" 300 15)))
      (emacsql nostr-db--connection [:insert :into zaps :values $v1] (vconcat z)))
    ;; two replies to n1 (one direct reply sets root_id = reply_id = n1)
    (nostr-perf-test--insert-event "reply-a" "x" "n1" "n1")
    (nostr-perf-test--insert-event "reply-b" "y" "n1" "other")
    (let* ((batch (nostr-db-event-counts-batch '("n1" "n2" "missing")))
           (n1 (cdr (assoc "n1" batch)))
           (n2 (cdr (assoc "n2" batch)))
           (missing (cdr (assoc "missing" batch))))
      (should (= 2 (alist-get 'reactions n1)))
      (should (= 1 (alist-get 'reposts n1)))
      (should (= 3 (alist-get 'zaps n1)))
      (should (= 600 (alist-get 'zap-msats n1)))
      (should (= 2 (alist-get 'replies n1)))
      ;; n2 and a missing id are all zeroes
      (dolist (key '(reactions reposts replies zaps zap-msats))
        (should (= 0 (alist-get key n2)))
        (should (= 0 (alist-get key missing)))))))

(ert-deftest nostr-db-event-counts-delegates-to-batch ()
  "The single-id function matches the batch result for the same id."
  (nostr-perf-test--with-db
    (nostr-perf-test--insert-event "n1" "author")
    (emacsql nostr-db--connection [:insert :into reactions :values $v1]
             (vector "react-1" "n1" "p" "+" 10))
    (should (equal (nostr-db-event-counts "n1")
                   (cdr (car (nostr-db-event-counts-batch '("n1"))))))
    (should (= 1 (alist-get 'reactions (nostr-db-event-counts "n1"))))))

(ert-deftest nostr-db-select-profiles-batch-matches-single ()
  "Batch profile load returns the same rows as per-pubkey selection."
  (nostr-perf-test--with-db
    (nostr-db-store-profile-event
     '((id . "m1") (pubkey . "alice") (kind . 0)
       (content . "{\"name\":\"Alice\",\"nip05\":\"alice@example.test\"}")
       (created_at . 100)))
    (nostr-db-store-profile-event
     '((id . "m2") (pubkey . "bob") (kind . 0)
       (content . "{\"name\":\"Bob\"}") (created_at . 100)))
    (let ((batch (nostr-db-select-profiles-batch '("alice" "bob" "missing"))))
      (should (equal (cdr (assoc "alice" batch)) (nostr-db-select-profile "alice")))
      (should (equal (cdr (assoc "bob" batch)) (nostr-db-select-profile "bob")))
      (should-not (assoc "missing" batch)))))

(ert-deftest nostr-ui-prime-caches-avoids-per-note-queries ()
  "After priming, per-note count and profile lookups hit the cache, not the DB."
  (nostr-perf-test--with-db
    (nostr-perf-test--insert-event "n1" "alice")
    (emacsql nostr-db--connection [:insert :into reactions :values $v1]
             (vector "r1" "n1" "p" "+" 10))
    (nostr-db-store-profile-event
     '((id . "m1") (pubkey . "alice") (kind . 0)
       (content . "{\"name\":\"Alice\",\"nip05\":\"alice@example.test\"}")
       (created_at . 100)))
    (with-temp-buffer
      (let ((event '((id . "n1") (pubkey . "alice")))
            (count-calls 0)
            (profile-calls 0))
        (nostr-ui-prime-caches (list event))
        (cl-letf (((symbol-function 'nostr-db-event-counts)
                   (lambda (&rest _) (cl-incf count-calls) (nostr-db--zero-counts)))
                  ((symbol-function 'nostr-db-select-profile)
                   (lambda (&rest _) (cl-incf profile-calls) nil)))
          ;; Values resolve from the primed caches...
          (should (= 1 (nostr-ui--event-count event 'reactions)))
          (should (equal "alice@example.test" (nostr-ui--event-nip05 event)))
          ;; ...without any per-note database query.
          (should (= 0 count-calls))
          (should (= 0 profile-calls)))))))

;;; Stage 3 --- avatar image descriptor cache

(ert-deftest nostr-ui-cached-image-decodes-once ()
  "An avatar image is decoded once and reused for repeated renders/sizes."
  (let ((nostr-ui--image-cache (make-hash-table :test #'equal))
        (decodes 0)
        (file (make-temp-file "nostr-img" nil ".png")))
    (unwind-protect
        (cl-letf (((symbol-function 'create-image)
                   (lambda (&rest _) (cl-incf decodes) (list 'image))))
          (let ((first (nostr-ui--cached-image file 32)))
            (should (= 1 decodes))
            ;; Same file + size reuses the cached descriptor (no new decode).
            (should (eq first (nostr-ui--cached-image file 32)))
            (should (= 1 decodes))
            ;; A different size is a distinct entry.
            (nostr-ui--cached-image file 64)
            (should (= 2 decodes))))
      (delete-file file))))

(provide 'nostr-perf-test)
;;; nostr-perf-test.el ends here
