;;; bech32-test.el --- Tests for Bech32 package      -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'ert)
(require 'bech32)

;;; Test helpers

(defun bech32--string-equal-ignore-case (a b)
  "Compare two strings A and B case-insensitively."
  (string= (downcase a) (downcase b)))

;;; Core tests

(ert-deftest bech32--hex-string-decode-test ()
  (should (equal (bech32--hex-string-decode "7e")
                 '(126)))
  (should (equal (bech32--hex-string-decode "7e7e")
                 '(126 126)))
  (should (equal (bech32--hex-string-decode "00ff")
                 '(0 255))))

(ert-deftest bech32--convert-bits-test ()
  ;; 0xFF = 11111111 => 5-bit chunks: 11111 11100 => 31 28
  (should (equal (bech32--convert-bits '(255) 8 5)
                 '(31 28)))
  ;; 1 2 3 4 = 00000001 00000010 00000011 00000100
  ;;           00000 00100 00001 00000 00110 00001 00000
  ;;             0     4     1     0     6     1     0
  ;; => big-endian packed 5-bit groups
  (should (equal (bech32--convert-bits '(1 2 3 4) 8 5)
                 '(0 4 1 0 6 1 0)))
  ;; reverse
  (should (equal (bech32--convert-bits '(0 4 1 0 6 1 0) 5 8)
                 '(1 2 3 4 0))))

;;; Test vectors

(defconst bech32--test-npub
  '(:hrp "npub"
         :bech "npub10elfcs4fr0l0r8af98jlmgdh9c8tcxjvz9qkw038js35mp4dma8qzvjptg"
         :hex "7e7e9c42a91bfef19fa929e5fda1b72e0ebc1a4c1141673e2794234d86addf4e"))

(defconst bech32--test-nsec
  '(:hrp "nsec"
         :bech "nsec1vl029mgpspedva04g90vltkh6fvh240zqtv9k0t9af8935ke9laqsnlfe5"
         :hex "67dea2ed018072d675f5415ecfaed7d2597555e202d85b3d65ea4e58d2d92ffa"))

;;; Encoding tests

(ert-deftest bech32-encode-npub-test ()
  (let* ((hrp (plist-get bech32--test-npub :hrp))
         (hex (plist-get bech32--test-npub :hex))
         (expected (plist-get bech32--test-npub :bech)))
    (should (bech32--string-equal-ignore-case
             (bech32-encode hrp hex)
             expected))))

(ert-deftest bech32-encode-nsec-test ()
  (let* ((hrp (plist-get bech32--test-nsec :hrp))
         (hex (plist-get bech32--test-nsec :hex))
         (expected (plist-get bech32--test-nsec :bech)))
    (should (bech32--string-equal-ignore-case
             (bech32-encode hrp hex)
             expected))))

(ert-deftest bech32-decode-npub-test ()
  (let ((bech (plist-get bech32--test-npub :bech))
        (expected-hrp (plist-get bech32--test-npub :hrp))
        (expected-data (bech32--convert-bits
                        (bech32--hex-string-decode (plist-get bech32--test-npub :hex))
                        8 5)))
    (pcase (bech32-decode bech)
      (`(,hrp ,data t)
       (should (equal hrp expected-hrp))
       (should (equal data expected-data))))))

(ert-deftest bech32-data-to-hex-test ()
  (let ((bech (plist-get bech32--test-npub :bech))
        (expected-hex (plist-get bech32--test-npub :hex)))
    (pcase (bech32-decode bech)
      (`(,_ ,data t)
       (should (equal (bech32-data-to-hex data)
                      expected-hex))))))

(provide 'bech32-test)
;;; bech32-test.el ends here
