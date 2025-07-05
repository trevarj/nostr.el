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
                   (kind . 1) (tags . ()) (content . "first") (sig . "s1") (relay . "r1")))
         (event2 '((id . "e1") (pubkey . "pk1") (created_at . 2000)
                   (kind . 1) (tags . ()) (content . "updated") (sig . "s2") (relay . "r2"))))
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
     (let ((contacts (emacsql nostr--db [:select [contact] :from follows :where (= pubkey "pubkey2")])))
       (should (equal contacts '(("contact1") ("contact2"))))))))

(ert-deftest nostr--handle-event-text-note ()
  "Test handling of kind 1 (text note) event."
  (with-temp-nostr-db
   (let ((event '((id . "note123")
                  (pubkey . "noteuser")
                  (created_at . 1020)
                  (kind . 1)
                  (tags . (("e" "root456" "" "root")
                           ("e" "reply789" "" "reply")))
                  (content . "Hello world")
                  (sig . "sig")
                  (relay . "r")
                  (reply_id . "reply789")
                  (reply_count . 0)
                  (root_id . "root456"))))
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

(ert-deftest nostr--fetch-text-notes ()
  "Test that text notes are returned with proper joins and filters."
  (with-temp-nostr-db
   (let ((root-event '((id . "root123")
                       (pubkey . "root_pubkey")
                       (created_at . 1020)
                       (kind . 1)
                       (tags . nil)
                       (content . "Root content")
                       (sig . "sig")
                       (relay . "r")))
         (reply-event '((id . "reply123")
                        (pubkey . "reply_pubkey")
                        (created_at . 1020)
                        (kind . 1)
                        (tags . (("e" "root123" "" "root")))
                        (content . "Reply content")
                        (sig . "sig")
                        (relay . "r"))))
     (nostr--handle-event "r" "" root-event)
     (nostr--handle-event "r" "" reply-event)
     (let ((all (nostr--fetch-text-notes nil 10 nil))
           (roots (nostr--fetch-text-notes nil 10 t)))
       (should (= (length all) 2))
       (message "%s" roots)
       (should (= (length roots) 1))
       (should (= (nth 8 (car roots)) 1))))))

(ert-deftest nostr--build-note-tags-top-level ()
  "Replying to a root level note (no tags)."
  (let* ((note '((id . "abc123") (pubkey . "bobpubkey") (tags . nil)
                 (root-id . nil) (reply-id . nil)))
         (tags (nostr--build-note-tags note)))
    (should (equal tags
                   '(("e" "abc123" "" "root")
                     ("e" "abc123" "" "reply")
                     ("p" "bobpubkey"))))))

(ert-deftest nostr--build-note-tags-threaded ()
  "Replying to a note that has a root e-tag."
  (let* ((note '((id . "reply2")
                 (pubkey . "carolpubkey")
                 (tags . (("e" "root1" "" "root")
                          ("e" "reply1" "" "reply")
                          ("p" "alicepubkey")))
                 (root-id . "root1")
                 (reply-id . "reply1")))
         (tags (nostr--build-note-tags note)))
    (should (equal tags
                   '(("e" "root1" "" "root")
                     ("e" "reply2" "" "reply")
                     ("p" "carolpubkey"))))))

(ert-deftest nostr--build-note-tags-no-root-but-e-tag ()
  "Replying to a note that has e-tags but no 'root' marker."
  (let* ((note '((id . "ghi789")
                 (pubkey . "pubkey")
                 (created_at . 1020)
                 (kind . 1)
                 (tags . (("e" "" "aaa111") ("p" "someone")))
                 (content . "Root content")
                 (sig . "sig")
                 (relay . "r")))
         (tags (nostr--build-note-tags note)))
    (should (equal tags
                   '(("e" "ghi789" "" "root")
                     ("e" "ghi789" "" "reply")
                     ("p" "pubkey"))))))

(ert-deftest nostr--build-note-tags-nil-input ()
  "Should return nil when input is nil (new root note)."
  (should (equal (nostr--build-note-tags nil) nil)))

(ert-deftest nostr--parse-e-tags-by-marker-test ()
  "Test parsing of e-tags by marker."
  (let* ((tags '(("e" "id1" "relay.example.com" "root")
                 ("e" "id2" "relay.example.com" "reply")
                 ("e" "id3" "relay.example.com")
                 ("e" "id4")
                 ("p" "pubkey"))) ;; Non-e tag, should be ignored
         (expected '(:root ("id1") :reply ("id2")))
         (result (nostr--parse-e-tags-by-marker tags)))
    (should (equal (plist-get result :root) (plist-get expected :root)))
    (should (equal (plist-get result :reply) (plist-get expected :reply)))
    (should (equal (length result) 4))))

(provide 'nostr-test)
;;; nostr-test.el ends here
