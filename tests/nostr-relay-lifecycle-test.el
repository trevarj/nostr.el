;;; nostr-relay-lifecycle-test.el --- Local relay lifecycle tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests `nostr-relay-open' against a local websocket server.

;;; Code:

(require 'ert)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'json)
(require 'websocket)
(require 'nostr-db)
(require 'nostr-relay)

(defmacro nostr-relay-lifecycle-test-with-db (&rest body)
  "Run BODY with an isolated in-memory Nostr database."
  (declare (indent 0))
  `(let ((nostr-db--connection (emacsql-sqlite-open nil)))
     (unwind-protect
         (progn
           (nostr-db-init)
           ,@body)
       (emacsql-close nostr-db--connection))))

(defun nostr-relay-lifecycle-test-wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds pass."
  (let ((deadline (+ (float-time) (or timeout 3)))
        result)
    (while (and (not result) (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (setq result (funcall predicate)))
    result))

(ert-deftest nostr-relay-open-talks-to-local-websocket-relay ()
  "Opening a relay sends REQ, handles EVENT, and records EOSE status."
  (nostr-relay-lifecycle-test-with-db
    (let (server server-ws client received)
      (unwind-protect
          (let* ((nostr-relay--connections (make-hash-table :test #'equal))
                 (nostr-relay--subscriptions (make-hash-table :test #'equal))
                 (nostr-relay-verify-events nil)
                 (url nil))
            (setq server
                  (websocket-server
                   0
                   :host 'local
                   :on-open (lambda (ws) (setq server-ws ws))
                   :on-message (lambda (_ws frame)
                                 (push (websocket-frame-payload frame) received))
                   :on-close (lambda (_ws) nil)))
            (setq url (format "ws://127.0.0.1:%s" (process-contact server :service)))
            (setq client (nostr-relay-open url "me"))
            (should
             (nostr-relay-lifecycle-test-wait-until
              (lambda ()
                (and server-ws received
                     (string-match-p "\\[\"REQ\"" (car received))))))
            (websocket-send-text
             server-ws
             (json-encode
              (list "EVENT" "personal-test"
                    '((id . "local-note")
                      (pubkey . "me")
                      (created_at . 100)
                      (kind . 1)
                      (tags . [])
                      (content . "hello from local relay")
                      (sig . "sig")))))
            (websocket-send-text server-ws (json-encode (list "EOSE" "personal-test")))
            (should
             (nostr-relay-lifecycle-test-wait-until
              (lambda ()
                (and (nostr-db-event-pubkey "local-note")
                     (equal (emacsql nostr-db--connection
                                     [:select [state message]
                                              :from relay_status
                                              :where (= url $s1)]
                                     url)
                            '(("eose" "personal-test")))))))
            (should (equal (alist-get 'content (car (nostr-db-select-thread "local-note")))
                           "hello from local relay")))
        (when (and client (websocket-openp client))
          (websocket-close client))
        (when server
          (websocket-server-close server))))))

(provide 'nostr-relay-lifecycle-test)
;;; nostr-relay-lifecycle-test.el ends here
