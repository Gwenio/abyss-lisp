
; ISC License (ISC)
;
; Copyright 2022-2023 James Adam Armstrong
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

(uiop:define-package :abyss/record
	(:use :cl)
	(:mix :abyss/types :abyss/error)
	(:import-from :abyss/context
		:push-frame :normal-pass :throw-exn
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/helpers
		:bind-params :type-pred-body :destructure :destructure/val
	)
	(:export
		:record-p-impl :record-impl :record-set-impl
		:record-subtype-impl
		:make-type-exn-impl :make-bounds-exn-impl
		:sym-not-found-p-impl :invalid-comb-p-impl :improper-list-p-impl
		:match-param-p-impl :match-cons-p-impl :match-null-p-impl
		:match-repeat-p-impl :bad-cont-p-impl :bad-handler-p-impl
		:type-exn-p-impl :div-zero-p-impl :bounds-exn-p-impl :export-exn-p-impl
	)
)
(in-package :abyss/record)

(declaim (ftype (function (cons) t) record-p-impl))

(defun record-p-impl (args)
	(type-pred-body args x (record-p x))
)

(declaim (ftype
	(function (abyss/types::type-id t)
		(function ((cons abyss/types::environment t)) t))
	record-subtype-construct))

(defun record-subtype-construct (tid bindings)
	(lambda (args)
		(let ((rec (make-record tid)))
			(funcall
				(destructure/val (record-obj rec) bindings rec)
				(cdr args))
		)
	)
)

(declaim (ftype (function (t abyss/types::type-id) t) record-subtype-p))

(defun record-subtype-p (x tid)
	(and (record-p x)
		(eq tid (record-subtype x)))
)

(declaim (ftype (function ((cons abyss/types::environment t)) t)
	record-subtype-impl))

(defun record-subtype-impl (args)
	(bind-params args (nil name bindings)
		(if (glyph-p name)
			(let ((tid (abyss/types::make-type-id name #'record-subtype-p)))
				(normal-pass (list
					(make-app (record-subtype-construct tid bindings))
					tid))
			)
			(throw-exn (make-type-exn name +tid-symbol+))
		)
	)
)

(declaim (ftype (function ((cons abyss/types::environment t)) t)
	record-impl))

(defun record-impl (args)
	(bind-params args (env bindings x)
		(let ((rec (make-record +tid-record+)))
			(push-frame (destructure/val (record-obj rec) bindings rec))
			(evaluate x env)
		)
	)
)

(declaim (ftype
	(function (abyss/types::environment t t)
		(function (abyss/types::record) t))
	record-set-aux))

(defun record-set-aux (env bindings x)
	(lambda (rec)
		(push-frame (destructure (record-obj rec) bindings))
		(evaluate x env)
	)
)

(declaim (ftype (function ((cons abyss/types::environment t)) t)
	record-set-impl))

(defun record-set-impl (args)
	(bind-params args (env rec bindings x)
		(push-frame (record-set-aux env bindings x))
		(evaluate rec env)
	)
)

(defun make-type-exn-impl (args)
	(bind-params args (nil obj expect)
		(normal-pass (make-type-exn obj expect))
	)
)

(defun make-bounds-exn-impl (args)
	(bind-params args (nil n lower upper)
		(normal-pass (make-bounds-exn n lower upper))
	)
)
