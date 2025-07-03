;;; bech32.el --- An Emacs Lisp implementation of Bech32  -*- lexical-binding: t; -*-

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

;; Experimental
;; From the spec: https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#user-content-Specification

;;; Code:

(defconst bech32--charset (string-to-list "qpzry9x8gf2tvdw0s3jn54khce6mua7l"))

(defun bech32--hex-string-decode (hex-string)
  "Decodes a HEX-STRING to raw byte list."
  (let ((chars (string-to-list hex-string)))
    (seq-map
     (lambda (two-chs) (string-to-number (concat two-chs) 16))
     (seq-partition chars 2))))

;; def convertbits(data, frombits, tobits, pad=True):
;;     """General power-of-2 base conversion."""
;;     acc = 0
;;     bits = 0
;;     ret = []
;;     maxv = (1 << tobits) - 1
;;     max_acc = (1 << (frombits + tobits - 1)) - 1
;;     for value in data:
;;         if value < 0 or (value >> frombits):
;;             return None
;;         acc = ((acc << frombits) | value) & max_acc
;;         bits += frombits
;;         while bits >= tobits:
;;             bits -= tobits
;;             ret.append((acc >> bits) & maxv)
;;     if pad:
;;         if bits:
;;             ret.append((acc << (tobits - bits)) & maxv)
;;     elif bits >= frombits or ((acc << (tobits - bits)) & maxv):
;;         return None
;;     return ret

(defun bech32--convert-bits (data from-bits to-bits)
  "Convert DATA list from FROM-BITS bit integers to TO-BITS integers."
  (let ((out '())
        (acc 0)
        (bits 0)
        (max-val (1- (ash 1 to-bits)))
        (max-acc (1- (ash 1 (1- (+ from-bits to-bits))))))
    (dolist (val data)
      (setq acc (logand (logior (ash acc from-bits) val)
                        max-acc)
            bits (+ bits from-bits))
      (while (>= bits to-bits)
        (setq bits (- bits to-bits))
        (push (logand (ash acc (- bits)) max-val) out)))
    (when (> bits 0)
      (push (logand (ash acc (- to-bits bits)) max-val) out))
    (nreverse out)))

;; def bech32_polymod(values):
;;   GEN = [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]
;;   chk = 1
;;   for v in values:
;;     b = (chk >> 25)
;;     chk = (chk & 0x1ffffff) << 5 ^ v
;;     for i in range(5):
;;       chk ^= GEN[i] if ((b >> i) & 1) else 0
;;   return chk

(defun bech32--polymod (values)
  "Computes bech32 checksum for VALUES."
  (let ((gen '(#x3b6a57b2 #x26508e6d #x1ea119fa #x3d4233dd #x2a1462b3)))
    (seq-reduce
     (lambda (chk v)
       (let ((b (ash chk -25))
             (chk (logxor
                   (ash (logand chk #x1ffffff) 5)
                   v)))
         (seq-reduce
          (lambda (chk i)
            (logxor chk
                    (if (not (eq (logand (ash b (- i)) 1) 0))
                        (nth i gen)
                      0)))
          '(0 1 2 3 4)
          chk)))
     values
     1)))

;; def bech32_hrp_expand(s):
;;   return [ord(x) >> 5 for x in s] + [0] + [ord(x) & 31 for x in s]

(defun bech32--hrp-expand (s)
  "Expand human readable part string S."
  (let ((chars (string-to-list s)))
    (append
     (seq-map (lambda (ch) (ash ch -5)) chars)
     [0]
     (seq-map (lambda (ch) (logand ch 31)) chars))))

;; def bech32_verify_checksum(hrp, data):
;;   return bech32_polymod(bech32_hrp_expand(hrp) + data) == 1

(defun bech32--verify-checksum (hrp data)
  "Verify the checksum on the HRP and DATA parts of the address string."
  (eq 1 (bech32--polymod (append (bech32--hrp-expand hrp) data))))

;; def bech32_create_checksum(hrp, data):
;;   values = bech32_hrp_expand(hrp) + data
;;   polymod = bech32_polymod(values + [0,0,0,0,0,0]) ^ 1
;;   return [(polymod >> 5 * (5 - i)) & 31 for i in range(6)]

(defun bech32--create-checksum (hrp data)
  "Creates a checksum from human-readable part HRP and non-checksum values of DATA.
Returns a string."
  (let* ((values (append (bech32--hrp-expand hrp) data))
         (polymod (logxor
                   (bech32--polymod (append values '(0 0 0 0 0 0)))
                   1)))
    (seq-map
     (lambda (i)
       (logand (ash polymod (- (* 5 (- 5 i))))
               31))
     '(0 1 2 3 4 5))))

;; def bech32_encode(hrp, data, spec):
;;     """Compute a Bech32 string given HRP and data values."""
;;     combined = data + bech32_create_checksum(hrp, data, spec)
;;     return hrp + '1' + ''.join([CHARSET[d] for d in combined])

(defun bech32-encode (hrp data)
  "Compute a Bech32 string given HRP and DATA as hex string."
  (let* ((data (bech32--convert-bits (bech32--hex-string-decode data) 8 5))
         (combined (append data (bech32--create-checksum hrp data))))
    (concat
     (append (string-to-list hrp)
             '(?1)
             (seq-map (lambda (d) (nth d bech32--charset)) combined)))))

;; def bech32_decode(bech):
;;     """Validate a Bech32/Bech32m string, and determine HRP and data."""
;;     if ((any(ord(x) < 33 or ord(x) > 126 for x in bech)) or
;;             (bech.lower() != bech and bech.upper() != bech)):
;;         return (None, None, None)
;;     bech = bech.lower()
;;     pos = bech.rfind('1')
;;     if pos < 1 or pos + 7 > len(bech) or len(bech) > 90:
;;         return (None, None, None)
;;     if not all(x in CHARSET for x in bech[pos+1:]):
;;         return (None, None, None)
;;     hrp = bech[:pos]
;;     data = [CHARSET.find(x) for x in bech[pos+1:]]
;;     spec = bech32_verify_checksum(hrp, data)
;;     if spec is None:
;;         return (None, None, None)
;;     return (hrp, data[:-6], spec)

(defun bech32-decode (addr)
  "Validate Bech32 ADDR and determine HRP and data."
  (let ((bech (string-to-list (downcase addr))))
    (unless (or (seq-some (lambda (ch) (or (< ch 33) (> ch 126))) (string-to-list addr))
                (and (not (equal (downcase addr) addr))
                     (not (equal (upcase addr) addr))))
      (if-let* ((pos (seq-position bech ?1))
                (len (length bech))
                (_ (or (> pos 1)
                       (> (+ pos 7) len)
                       (> len 90)))
                (suffix (seq-subseq bech (1+ pos)))
                (_ (seq-every-p (lambda (ch) (memq ch bech32--charset))
                                suffix))
                (hrp (concat (seq-subseq bech 0 pos)))
                (data (seq-map
                       (lambda (ch) (seq-position bech32--charset ch)) suffix))
                (spec (bech32--verify-checksum hrp data)))
          (list hrp (seq-subseq data 0 -6) spec)))))

(provide 'bech32)
;;; bech32.el ends here
