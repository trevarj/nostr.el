;;; nostr-relay-lifecycle-test.el --- Relay daemon lifecycle tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests `nostr-relay-open' daemon commands and local bookkeeping.

;;; Code:

(require 'ert)
(require 'seq)
(require 'emacsql)
(require 'emacsql-sqlite)
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

(ert-deftest nostr-relay-open-sends-daemon-subscriptions ()
  "Opening a relay registers it with the daemon and primes live subscriptions."
  (nostr-relay-lifecycle-test-with-db
    (let ((nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-relay--connecting (make-hash-table :test #'equal))
          (nostr-relay--subscriptions (make-hash-table :test #'equal))
          (nostr-relay--pending-subscriptions (make-hash-table :test #'equal))
          (nostr-relay--syncing-subs (make-hash-table :test #'equal))
          (nostr-relay--daemon-pubkey "me")
          added
          subscriptions)
      (cl-letf (((symbol-function 'nostr-daemon-running-p) (lambda () t))
                ((symbol-function 'nostr-daemon-add-relay)
                 (lambda (url) (push url added)))
                ((symbol-function 'nostr-daemon-subscribe)
                 (lambda (id filters relays)
                   (push (list id filters relays) subscriptions))))
        (should (nostr-relay-open "wss://relay.example" "me"))
        (should (equal added '("wss://relay.example")))
        (should (gethash "wss://relay.example" nostr-relay--connections))
        (should (seq-some (lambda (request)
                            (string-prefix-p "personal-" (car request)))
                          subscriptions))))))

(ert-deftest nostr-relay-open-does-not-block-on-daemon-connect ()
  "`nostr-relay-open' only sends daemon commands and returns promptly."
  (nostr-relay-lifecycle-test-with-db
    (let ((nostr-relay--connections (make-hash-table :test #'equal))
          (nostr-relay--connecting (make-hash-table :test #'equal))
          (nostr-relay--subscriptions (make-hash-table :test #'equal))
          (nostr-relay--pending-subscriptions (make-hash-table :test #'equal))
          (nostr-relay--syncing-subs (make-hash-table :test #'equal))
          (nostr-relay--daemon-pubkey "me")
          (url "wss://relay.example"))
      (cl-letf (((symbol-function 'nostr-daemon-running-p) (lambda () t))
                ((symbol-function 'nostr-daemon-add-relay) #'ignore)
                ((symbol-function 'nostr-daemon-subscribe) #'ignore))
        (let ((start (float-time)))
          (nostr-relay-open url "me")
          (should (< (- (float-time) start) 1))
          (should (gethash url nostr-relay--connections))
          (should-not (gethash url nostr-relay--connecting)))))))

(provide 'nostr-relay-lifecycle-test)
;;; nostr-relay-lifecycle-test.el ends here
