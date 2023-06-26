
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
	(:import-from :abyss/types
		:make-record :record-p :record-obj :record-subtype
		:make-type-id :make-glyph :make-record
	)
	(:export
		:+tid-sym-not-found+ :sym-not-found-p :make-sym-not-found
		:+tid-invalid-comb+ :invalid-comb-p :make-invalid-comb
		:+tid-improper-list+ :improper-list-p :make-improper-list
		:+tid-match-param+ :match-param-p :make-match-param
		:+tid-match-repeat+ :match-repeat-p :make-match-repeat
		:+tid-match-cons+ :match-cons-p :make-match-cons
		:+tid-match-null+ :match-null-p :make-match-null
		:+tid-bad-cont+ :bad-cont-p :make-bad-cont
		:+tid-bad-handler+ :bad-handler-p :make-bad-handler
		:+tid-type-exn+ :type-exn-p :make-type-exn
		:+tid-div-zero+ :div-zero-p :make-div-zero
		:+tid-bounds-exn+ :bounds-exn-p :make-bounds-exn
	)
)
(in-package :abyss/error)

(declaim (type abyss/types::type-id
	+tid-sym-not-found+ +tid-invalid-comb+ +tid-improper-list+
	+tid-match-param+ +tid-match-repeat+ +tid-match-null+ +tid-match-cons+
	+tid-bad-cont+ +tid-bad-handler+
	+tid-type-exn+ +tid-div-zero+ +tid-bounds-exn+
))

(defvar +tid-sym-not-found+ (make-type-id "sym-not-found"))
(defvar +tid-invalid-comb+ (make-type-id "invalid-combiner"))
(defvar +tid-improper-list+ (make-type-id "improper-list"))
(defvar +tid-match-param+ (make-type-id "match-param"))
(defvar +tid-match-repeat+ (make-type-id "match-repeat"))
(defvar +tid-match-null+ (make-type-id "match-null"))
(defvar +tid-match-cons+ (make-type-id "match-cons"))
(defvar +tid-bad-cont+ (make-type-id "bad-continuation"))
(defvar +tid-bad-handler+ (make-type-id "bad-handler-case"))
(defvar +tid-type-exn+ (make-type-id "type-exn"))
(defvar +tid-div-zero+ (make-type-id "div-by-zero"))
(defvar +tid-bounds-exn+ (make-type-id "bounds-exn"))

(defmacro exn-pred (x tid)
	`(and (record-p ,x) (eq ,tid (record-subtype ,x)))
)

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

(defun make-sym-not-found (sym obj)
	(let* ((rec (make-record +tid-sym-not-found+)) (table (record-obj rec)))
		(init-field table +sym+ sym)
		(init-field table +obj+ obj)
		rec
	)
)

(defun sym-not-found-p (x)
	(exn-pred x +tid-sym-not-found+)
)

(defun make-invalid-comb (obj)
	(let* ((rec (make-record +tid-invalid-comb+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		rec
	)
)

(defun invalid-comb-p (x)
	(exn-pred x +tid-invalid-comb+)
)

(defun make-improper-list (tail result)
	(let* ((rec (make-record +tid-improper-list+)) (table (record-obj rec)))
		(init-field table +obj+ tail)
		(init-field table +res+ result)
		rec
	)
)

(defun improper-list-p (x)
	(exn-pred x +tid-improper-list+)
)

(defun make-match-param (obj)
	(let* ((rec (make-record +tid-match-param+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		rec
	)
)

(defun match-param-p (x)
	(exn-pred x +tid-match-param+)
)

(defun make-match-cons (obj)
	(let* ((rec (make-record +tid-match-cons+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		rec
	)
)

(defun match-cons-p (x)
	(exn-pred x +tid-match-cons+)
)

(defun make-match-null (obj)
	(let* ((rec (make-record +tid-match-null+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		rec
	)
)

(defun match-null-p (x)
	(exn-pred x +tid-match-null+)
)

(defun make-match-repeat (obj)
	(let* ((rec (make-record +tid-match-repeat+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		rec
	)
)

(defun match-repeat-p (x)
	(exn-pred x +tid-match-repeat+)
)

(defun make-bad-cont (obj)
	(let* ((rec (make-record +tid-bad-cont+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		rec
	)
)

(defun bad-cont-p (x)
	(exn-pred x +tid-bad-cont+)
)

(defun make-bad-handler (obj)
	(let* ((rec (make-record +tid-bad-handler+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		rec
	)
)

(defun bad-handler-p (x)
	(exn-pred x +tid-bad-handler+)
)

(defun make-type-exn (obj expect)
	(let* ((rec (make-record +tid-type-exn+)) (table (record-obj rec)))
		(init-field table +obj+ obj)
		(init-field table +expect+ expect)
		rec
	)
)

(defun type-exn-p (x)
	(exn-pred x +tid-type-exn+)
)

(defun make-div-zero (n)
	(let* ((rec (make-record +tid-div-zero+)) (table (record-obj rec)))
		(init-field table +val+ n)
		rec
	)
)

(defun div-zero-p (x)
	(exn-pred x +tid-div-zero+)
)

(defun make-bounds-exn (n lower upper)
	(let* ((rec (make-record +tid-bounds-exn+)) (table (record-obj rec)))
		(init-field table +val+ n)
		(init-field table +min+ lower)
		(init-field table +max+ upper)
		rec
	)
)

(defun bounds-exn-p (x)
	(exn-pred x +tid-bounds-exn+)
)
