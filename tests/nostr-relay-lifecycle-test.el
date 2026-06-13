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
                 (nostr-relay--connecting (make-hash-table :test #'equal))
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
            ;; `nostr-relay-open' probes reachability first and connects
            ;; asynchronously, so the websocket appears in the connection table
            ;; once the probe and handshake complete rather than being returned.
            (nostr-relay-open url "me")
            (should
             (nostr-relay-lifecycle-test-wait-until
              (lambda ()
                (and server-ws received
                     (string-match-p "\\[\"REQ\"" (car received))))))
            (setq client (gethash url nostr-relay--connections))
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

(ert-deftest nostr-relay-open-does-not-block-on-slow-connect ()
  "`nostr-relay-open' returns promptly when a relay's TCP connect is slow.

`websocket-open' sends the upgrade handshake with `process-send-string' while
the `:nowait' socket is still in the `connect' state, which blocks Emacs in C
until the connection completes or the OS connect timeout elapses.
`nostr-relay-open' must therefore probe reachability with a socket it never
writes to: opening a relay whose host black-holes the connection (192.0.2.1,
RFC 5737 TEST-NET-1) must return immediately, leave the URL marked as
connecting, and never reach `websocket-open'."
  (nostr-relay-lifecycle-test-with-db
    (let ((nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-relay--connecting (make-hash-table :test #'equal))
          (nostr-relay--subscriptions (make-hash-table :test #'equal))
          (nostr-relay-open-timeout-seconds 30)
          (url "ws://192.0.2.1:80")
          probe)
      ;; Skip when the sandbox forbids creating a non-loopback socket.
      (condition-case _err
          (setq probe (make-network-process
                       :name "nostr-relay-blackhole-probe"
                       :host "192.0.2.1" :service 80 :nowait t))
        (error (ert-skip "Cannot create non-loopback network process here")))
      (when probe (delete-process probe))
      (unwind-protect
          (let ((start (float-time)))
            (nostr-relay-open url "me")
            (let ((elapsed (- (float-time) start)))
              ;; A real connect to 192.0.2.1 takes tens of seconds; the
              ;; non-blocking probe makes `nostr-relay-open' return effectively
              ;; immediately.  Allow generous slack for slow CI.
              (should (< elapsed 5))
              ;; The probe is in flight and no websocket was created.
              (should (gethash url nostr-relay--connecting))
              (should-not (gethash url nostr-relay--connections))))
        (nostr-relay-disconnect-all)))))

(provide 'nostr-relay-lifecycle-test)
;;; nostr-relay-lifecycle-test.el ends here
