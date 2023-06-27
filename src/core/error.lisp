
; ISC License (ISC)
;
; Copyright 2023 James Adam Armstrong
;
; Permission to use, copy, modify, and/or distribute this software for any
; purpose with or without fee is hereby granted, provided that the above copyright
; notice and this permission notice appear in all copies.
;
; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
; REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
; FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
; INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
; OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
; PERFORMANCE OF THIS SOFTWARE.

(uiop:define-package :abyss/error
	(:use :cl)
	(:mix :abyss/types)
	(:export
		:exn-type-p
		:+tid-sym-not-found+ :make-sym-not-found
		:+tid-invalid-comb+ :make-invalid-comb
		:+tid-improper-list+ :make-improper-list
		:+tid-match-param+ :make-match-param
		:+tid-match-repeat+ :make-match-repeat
		:+tid-match-cons+ :make-match-cons
		:+tid-match-null+ :make-match-null
		:+tid-bad-cont+ :make-bad-cont
		:+tid-bad-handler+ :make-bad-handler
		:+tid-type-exn+ :make-type-exn
		:+tid-div-zero+ :make-div-zero
		:+tid-bounds-exn+ :make-bounds-exn
		:+tid-export-exn+ :make-export-exn
	)
)
(in-package :abyss/error)

(declaim (ftype (function (t abyss/types::type-id) t) exn-type-p))

(defun exn-type-p (x tid)
	(and (record-p x) (eq tid (record-subtype x)))
)

(defmacro def-exn-id (var name)
	`(defvar ,var (make-type-id (make-glyph ,name) #'exn-type-p))
)

(declaim (type abyss/types::type-id
	+tid-sym-not-found+ +tid-invalid-comb+ +tid-improper-list+
	+tid-match-param+ +tid-match-repeat+ +tid-match-null+ +tid-match-cons+
	+tid-bad-cont+ +tid-bad-handler+
	+tid-type-exn+ +tid-div-zero+ +tid-bounds-exn+ +tid-export-exn+
))

(def-exn-id +tid-sym-not-found+ "sym-not-found")
(def-exn-id +tid-invalid-comb+ "invalid-combiner")
(def-exn-id +tid-improper-list+ "improper-list")
(def-exn-id +tid-match-param+ "match-param")
(def-exn-id +tid-match-repeat+ "match-repeat")
(def-exn-id +tid-match-null+ "match-null")
(def-exn-id +tid-match-cons+ "match-cons")
(def-exn-id +tid-bad-cont+ "bad-continuation")
(def-exn-id +tid-bad-handler+ "bad-handler-case")
(def-exn-id +tid-type-exn+ "type-exn")
(def-exn-id +tid-div-zero+ "div-by-zero")
(def-exn-id +tid-bounds-exn+ "bounds-exn")
(def-exn-id +tid-export-exn+ "repeat-export")

(defmacro init-field (table field val)
	`(setf (gethash ,field ,table) ,val)
)

(defvar +sym+ (make-glyph "symbol"))
(defvar +obj+ (make-glyph "object"))
(defvar +res+ (make-glyph "result"))
(defvar +expect+ (make-glyph "expected"))
(defvar +val+ (make-glyph "value"))
(defvar +min+ (make-glyph "min"))
(defvar +max+ (make-glyph "max"))
(defvar +old+ (make-glyph "old"))
(defvar +new+ (make-glyph "new"))

(defun make-sym-not-found (sym obj)
	(let* ((rec (make-record +tid-sym-not-found+)) (table (record-obj rec)))
		(init-field table +sym+ sym)
		(init-field table +obj+ obj)
		rec
	)
)

(defun make-invalid-comb (obj)
	(let* ((rec (make-record +tid-invalid-comb+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		rec
	)
)

(defun make-improper-list (tail result)
	(let* ((rec (make-record +tid-improper-list+)) (table (record-obj rec)))
		(init-field table +obj+ tail)
		(init-field table +res+ result)
		rec
	)
)

(defun make-match-param (obj)
	(let* ((rec (make-record +tid-match-param+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		rec
	)
)

(defun make-match-cons (obj)
	(let* ((rec (make-record +tid-match-cons+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		rec
	)
)

(defun make-match-null (obj)
	(let* ((rec (make-record +tid-match-null+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		rec
	)
)

(defun make-match-repeat (obj)
	(let* ((rec (make-record +tid-match-repeat+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		rec
	)
)

(defun make-bad-cont (obj)
	(let* ((rec (make-record +tid-bad-cont+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		rec
	)
)

(defun make-bad-handler (obj)
	(let* ((rec (make-record +tid-bad-handler+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		rec
	)
)

(defun make-type-exn (obj expect)
	(let* ((rec (make-record +tid-type-exn+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		(init-field table +expect+ expect)
		rec
	)
)

(defun make-div-zero (n)
	(let* ((rec (make-record +tid-div-zero+)) (table (record-obj rec)))
		(init-field table +val+ n)
		rec
	)
)

(defun make-bounds-exn (n lower upper)
	(let* ((rec (make-record +tid-bounds-exn+)) (table (record-obj rec)))
		(init-field table +val+ n)
		(init-field table +min+ lower)
		(init-field table +max+ upper)
		rec
	)
)

(defun make-export-exn (sym old new)
	(let* ((rec (make-record +tid-export-exn+)) (table (record-obj rec)))
		(init-field table +sym+ sym)
		(init-field table +old+ old)
		(init-field table +new+ new)
		rec
	)
)
