;;; nostr-live-relay-smoke-test.el --- Opt-in public relay smoke test -*- lexical-binding: t; -*-

;;; Commentary:

;; This test is skipped by default.  Set NOSTR_LIVE_RELAY_TEST=1 to exercise a
;; real public relay through the same frame handler used by nostr.el.

;;; Code:

(require 'ert)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'json)
(require 'websocket)
(require 'nostr-backend)
(require 'nostr-db)
(require 'nostr-relay)

(defmacro nostr-live-relay-smoke-test-with-db (&rest body)
  "Run BODY with an isolated in-memory Nostr database."
  (declare (indent 0))
  `(let ((nostr-db--connection (emacsql-sqlite-open nil)))
     (unwind-protect
         (progn
           (nostr-db-init)
           ,@body)
       (emacsql-close nostr-db--connection))))

(defun nostr-live-relay-smoke-test-wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds pass."
  (let ((deadline (+ (float-time) (or timeout 10)))
        result)
    (while (and (not result) (< (float-time) deadline))
      (accept-process-output nil 0.1)
      (setq result (funcall predicate)))
    result))

(ert-deftest nostr-live-relay-smoke-receives-and-stores-public-event ()
  "Connect to a public relay, receive one event, verify it, and store it."
  (skip-unless (getenv "NOSTR_LIVE_RELAY_TEST"))
  (skip-unless (executable-find nostr-backend-command))
  (nostr-live-relay-smoke-test-with-db
    (let* ((url (or (getenv "NOSTR_LIVE_RELAY_URL")
                    "wss://relay.damus.io"))
           (sub-id "nostr-el-live-smoke")
           (nostr-relay--connections (make-hash-table :test #'equal))
           (nostr-relay--subscriptions (make-hash-table :test #'equal))
           (nostr-relay-verify-events t)
           client stored-pubkey error-message last-rejection)
      (unwind-protect
          (progn
            (setq client
                  (websocket-open
                   url
                   :on-open
                   (lambda (ws)
                     (websocket-send-text
                      ws
                       (json-encode
                       (vector "REQ"
                               sub-id
                               `(("kinds" . (,nostr-kind-metadata))
                                 ("limit" . 10))))))
                   :on-message
                   (lambda (ws frame)
                     (let ((payload (websocket-frame-payload frame)))
                       (pcase (json-parse-string payload
                                                 :object-type 'alist
                                                 :array-type 'list
                                                 :false-object nil)
                         (`("EVENT" ,_sub-id ,event)
                          (unless stored-pubkey
                            (if (nostr-relay-handle-frame url payload)
                                (progn
                                  (setq stored-pubkey (alist-get 'pubkey event))
                                  (websocket-send-text
                                   ws
                                   (json-encode (vector "CLOSE" sub-id)))
                                  (websocket-close ws))
                              (setq last-rejection
                                    (emacsql nostr-db--connection
                                             [:select [state message]
                                                      :from relay_status
                                                      :where (= url $s1)]
                                             url)))))
                         (`("EOSE" ,_sub-id)
                          (unless stored-pubkey
                            (setq error-message
                                  (format "no verified metadata event before EOSE: %S"
                                          last-rejection)))))))
                   :on-error
                   (lambda (_ws type err)
                     (setq error-message (format "%s %s" type err))))))
            (should
             (nostr-live-relay-smoke-test-wait-until
              (lambda ()
                (or (and stored-pubkey
                         (nostr-db-select-profile stored-pubkey))
                    error-message)))))
        (when (and client (websocket-openp client))
          (websocket-close client))
      (should-not error-message)
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

(provide 'nostr-live-relay-smoke-test)
;;; nostr-live-relay-smoke-test.el ends here
