;;; nostr-live-relay-smoke-test.el --- Opt-in public relay smoke test -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests are skipped by default.  Set NOSTR_LIVE_RELAY_TEST=1 to exercise
;; a real public relay through the Rust relay daemon used by nostr.el.

;;; Code:

(require 'ert)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'json)
(require 'websocket)
(require 'nostr-backend)
(require 'nostr-db)
(require 'nostr-relay)
(require 'nostr-search)

(defmacro nostr-live-relay-smoke-test-with-db (&rest body)
  "Run BODY with an isolated file-backed Nostr database."
  (declare (indent 0))
  `(let ((path (make-temp-file "nostr-live-smoke" nil ".sqlite")))
     (let ((nostr-db-path path)
           (nostr-db--connection (emacsql-sqlite-open path)))
       (unwind-protect
           (progn
             (nostr-db-init)
             ,@body)
         (when (fboundp 'nostr-relay-disconnect-all)
           (nostr-relay-disconnect-all))
         (emacsql-close nostr-db--connection)
         (when (file-exists-p path)
           (delete-file path))))))

(defun nostr-live-relay-smoke-test-wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds pass."
  (let ((deadline (+ (float-time) (or timeout 10)))
        result)
    (while (and (not result) (< (float-time) deadline))
      (accept-process-output nil 0.1)
      (setq result (funcall predicate)))
    result))

(ert-deftest nostr-live-relay-smoke-receives-and-stores-public-event ()
  "Connect to a public relay through the daemon and store one metadata event."
  (skip-unless (getenv "NOSTR_LIVE_RELAY_TEST"))
  (skip-unless (executable-find nostr-backend-command))
  (nostr-live-relay-smoke-test-with-db
    (let* ((url (or (getenv "NOSTR_LIVE_RELAY_URL")
                    "wss://relay.damus.io"))
           (sub-id "nostr-el-live-smoke")
           (nostr-relay--connections (make-hash-table :test #'equal))
           (nostr-relay--subscriptions (make-hash-table :test #'equal))
           (nostr-relay--pending-subscriptions (make-hash-table :test #'equal))
           stored-pubkey)
      (unwind-protect
          (progn
            (nostr-relay-open url nil)
            (nostr-relay-subscribe
             url
             sub-id
             `((("kinds" . (,nostr-kind-metadata))
                ("limit" . 10))))
            (should
             (nostr-live-relay-smoke-test-wait-until
              (lambda ()
                (setq stored-pubkey
                      (caar (emacsql nostr-db--connection
                                     [:select [pubkey]
                                      :from profiles
                                      :limit 1])))))))
        (nostr-relay-close-subscription-all sub-id))
      (should stored-pubkey)
      (should (nostr-db-select-profile stored-pubkey)))))

(ert-deftest nostr-live-relay-smoke-publishes-ephemeral-event ()
  "Generate a temporary key, publish one note, and receive a relay OK."
  (skip-unless (getenv "NOSTR_LIVE_RELAY_PUBLISH_TEST"))
  (skip-unless (executable-find nostr-backend-command))
  (nostr-live-relay-smoke-test-with-db
    (let* ((url (or (getenv "NOSTR_LIVE_RELAY_URL")
                    "wss://relay.damus.io"))
           (key-response (cdr (nostr-backend-call-sync
                               "generate-key"
                               (make-hash-table :test 'equal))))
           (secret (alist-get 'secret_key key-response))
           (content (format "nostr.el live publish smoke %s" (float-time)))
           (signed (cdr (nostr-backend-call-sync
                         "sign-event"
                         `((secret_key . ,secret)
                           (kind . ,nostr-kind-text-note)
                           (tags . (("client" "nostr.el live smoke")))
                           (content . ,content)
                           (envelope . t)))))
           (event (alist-get 'event signed))
           (client-message (alist-get 'client_message signed))
           (event-id (alist-get 'id event))
           client accepted-message rejected-message error-message)
      (unwind-protect
          (progn
            (setq client
                  (websocket-open
                   url
                   :on-open
                   (lambda (ws)
                     (websocket-send-text ws client-message))
                   :on-message
                   (lambda (ws frame)
                     (pcase (json-parse-string (websocket-frame-payload frame)
                                               :object-type 'alist
                                               :array-type 'list
                                               :false-object nil)
                       (`("OK" ,ok-event-id ,accepted ,message)
                        (when (equal ok-event-id event-id)
                          (if accepted
                              (setq accepted-message message)
                            (setq rejected-message message))
                          (websocket-close ws)))))
                   :on-error
                   (lambda (_ws type err)
                     (setq error-message (format "%s %s" type err))))))
            (should
             (nostr-live-relay-smoke-test-wait-until
              (lambda ()
                (or accepted-message rejected-message error-message)))))
        (when (and client (websocket-openp client))
          (websocket-close client))
      (should-not error-message)
      (should-not rejected-message)
      (should accepted-message)
      (should event-id))))

(ert-deftest nostr-live-relay-smoke-searches-profile-name ()
  "Search a real indexed relay for ODELL and populate local note results."
  (skip-unless (getenv "NOSTR_LIVE_RELAY_SEARCH_TEST"))
  (nostr-live-relay-smoke-test-with-db
    (let* ((url (or (getenv "NOSTR_LIVE_SEARCH_RELAY_URL")
                    "wss://relay.primal.net"))
           (nostr-relay-urls nil)
           (nostr-relay-search-urls '("wss://cache2.primal.net/v1"))
           (nostr-relay-search-author-urls (list url))
           (nostr-relay--connections (make-hash-table :test #'equal))
           (nostr-relay--connecting (make-hash-table :test #'equal))
           (nostr-relay--subscriptions (make-hash-table :test #'equal))
           (nostr-relay--search-profile-queries (make-hash-table :test #'equal))
           (nostr-relay--search-profile-author-requests (make-hash-table :test #'equal))
           (nostr-relay--pending-subscriptions (make-hash-table :test #'equal))
           ;; This smoke focuses on search subscription behavior.  Signature
           ;; verification coverage lives in the generic live receive test.
           (nostr-relay-verify-events nil)
           (nostr-relay-open-timeout-seconds 5)
           (nostr-default-feed-limit 25))
      (unwind-protect
          (progn
            (nostr-relay-search "ODELL" 25)
            (should
             (nostr-live-relay-smoke-test-wait-until
              (lambda ()
                (nostr-search--select-local "ODELL" 5))
              20))
            (should (nostr-search--select-local "@ODELL" 5)))
        (nostr-relay-disconnect-all)))))

(provide 'nostr-live-relay-smoke-test)
;;; nostr-live-relay-smoke-test.el ends here
