;;; nostr-test.el --- Test suite for nostr.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests

;;; Code:

(require 'ert)
(require 'emacsql)
(require 'emacsql-sqlite)
(load-file "./nostr.el")

(defmacro with-temp-nostr-db (&rest body)
  "Run BODY with an in-memory database for testing."
  `(let* ((nostr--db (emacsql-sqlite-open nil)))
     (unwind-protect
         (progn
           (nostr--init-db)
           ,@body)
       (emacsql-close nostr--db))))

(ert-deftest nostr--store-event-upsert-test ()
  "Test that `nostr--store-event` upserts events based on id."
  (with-temp-nostr-db
   (let ((event1 '((id . "e1") (pubkey . "pk1") (created_at . 1000)
                   (kind . 1) (tags . []) (content . "first") (sig . "s1") (relay . "r1")))
         (event2 '((id . "e1") (pubkey . "pk1") (created_at . 2000)
                   (kind . 1) (tags . []) (content . "updated") (sig . "s2") (relay . "r2"))))
     ;; Store first event
     (nostr--store-event "r1" event1)
     ;; Verify inserted
     (should (equal
              (emacsql nostr--db [:select content :from events :where (= id $s1)] "e1")
              '(("first"))))
     ;; Store second event with same id but updated content and created_at
     (nostr--store-event "r2" event2)
     ;; Verify it replaced (upserted)
     (should (equal
              (emacsql nostr--db [:select content :from events :where (= id $s1)] "e1")
              '(("updated")))))))

(ert-deftest nostr--handle-frame-event ()
  "Handle a full text-note EVENT frame and store it."
  (with-temp-nostr-db
   (let ((frame (json-encode '["EVENT" "sub123"
                               ((id . "e123") (pubkey . "p1") (created_at . 1000)
                                (kind . 1) (tags . []) (content . "hello") (sig . "s") (relay . "r"))])))
     (nostr--handle-frame "r" frame)
     (should (equal
              (emacsql nostr--db [:select [id content] :from events :where (= id "e123")])
              '(("e123" "hello")))))))

(ert-deftest nostr--handle-frame-notice ()
  "Handle NOTICE frame."
  (let ((msg "Relay under maintenance"))
    (should (equal (nostr--handle-frame "r" (json-encode `["NOTICE" ,msg]))
                   t))))

(ert-deftest nostr--handle-frame-closed ()
  "Handle CLOSED frame."
  (let ((nostr--subscriptions (make-hash-table :test 'equal)))
    (puthash "sub-2" t nostr--subscriptions)
    (let ((frame (json-encode '["CLOSED" "sub-2" "bad filter"])))
      (nostr--handle-frame "r" frame)
      (should-not (gethash "sub-2" nostr--subscriptions)))))

(ert-deftest nostr--handle-frame-ok ()
  "Handle OK frame with accepted event."
  (should (equal (nostr--handle-frame "r" (json-encode ["OK" "e789" t "saved"]))
                 t)))

(ert-deftest nostr--handle-event-metadata ()
  "Test handling of kind 0 (metadata) event."
  (with-temp-nostr-db
   (let ((event '((id . "e1")
                  (pubkey . "pubkey1")
                  (created_at . 1000)
                  (kind . 0)
                  (tags . [])
                  (content . "{\"name\":\"Alice\",\"about\":\"Dev\",\"picture\":\"http://img\"}")
                  (sig . "sig")
                  (relay . "r"))))
     (nostr--handle-event "r" "sub" event)
     (should (equal (emacsql nostr--db
                             [:select [name about picture]
                              :from users
                              :where (= pubkey "pubkey1")])
                    '(("Alice" "Dev" "http://img")))))))

(ert-deftest nostr--handle-event-contacts ()
  "Test handling of kind 3 (contact list) event."
  (with-temp-nostr-db
   (let ((event '((id . "e2")
                  (pubkey . "pubkey2")
                  (created_at . 1010)
                  (kind . 3)
                  (tags . (("p" "contact1") ("p" "contact2")))
                  (content . "")
                  (sig . "sig")
                  (relay . "r"))))
     (nostr--handle-event "r" "sub" event)
     (let ((contacts (emacsql nostr--db [:select [pubkey] :from follows])))
       (should (equal contacts '(("contact1") ("contact2"))))))))

(ert-deftest nostr--handle-event-text-note ()
  "Test handling of kind 1 (text note) event."
  (with-temp-nostr-db
   (let ((event '((id . "note123")
                  (pubkey . "noteuser")
                  (created_at . 1020)
                  (kind . 1)
                  (tags . [])
                  (content . "Hello world")
                  (sig . "sig")
                  (relay . "r"))))
     (nostr--handle-event "r" "sub" event)
     (should (equal (emacsql nostr--db
                             [:select *
                              :from events
                              :where (= id "note123")])
                    (list (mapcar #'cdr event)))))))

(ert-deftest nostr--handle-event-unknown-kind ()
  "Test handling of unknown kind event."
  (with-temp-nostr-db
   (let ((event '((id . "xyz")
                  (pubkey . "testuser")
                  (created_at . 1030)
                  (kind . 999)
                  (tags . [])
                  (content . "misc event")
                  (sig . "sig")
                  (relay . "r"))))
     (nostr--handle-event "r" "sub" event)
     (should (equal (emacsql nostr--db
                             [:select *
                              :from events
                              :where (= id "xyz")])
                    nil)))))

(defun nostr--random-test-data ()
  "Generate some random test data into `nostr--db'."
  (setq nostr--db nil)
  (nostr--open-db nil)
  (nostr--init-db)

  ;; Insert authors
  (dolist (user '(("1" "Bob"   "About Bob"   "https://pic.com/1")
                  ("2" "Alice" "About Alice" "https://pic.com/2")
                  ("3" "Cody"  "About Cody"  "https://pic.com/3")))
    (apply #'emacsql nostr--db
           [:insert :into users :values [$s1 $s2 $s3 $s4]]
           user))

  ;; Random message pool
  (setq content-pool '("Hello world!" "Test post" "Just checking in" "What’s up?"
                       "Random message here" "Another day, another post" "Coffee time ☕"
                       "Working on Emacs stuff" "Nostr is neat" "How’s everyone doing?"))

  ;; Generate 15 events with random content, randomized timestamps, and rotating authors
  (dotimes (i 15)
    (let* ((id (format "e%d" i))
           (pubkey (nth (mod i 3) '("1" "2" "3")))
           (created-at (+ 1700000000 (random 10000)))
           (content (nth (random (length content-pool)) content-pool)))
      (emacsql nostr--db
               [:insert :into events :values [$s1 $s2 $s3 $s4 $s5 $s6 $s7 $s8]]
               id pubkey created-at 1 '() content "sig" "relay"))))

;; (nostr--random-test-data)
;; (nostr--fetch-user-posts 1700008716)


(provide 'nostr-test)
;;; nostr-test.el ends here
