
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

(uiop:define-package :abyss/helpers
	(:use :cl)
	(:import-from :abyss/types
		:+true+ :+false+ :+inert+ :+tid-boole+
	)
	(:import-from :abyss/error
		:make-improper-list :make-arg-pair :make-arg-null :make-type-exn
		:make-arg-repeat :make-bad-param
	)
	(:import-from :abyss/context
		:normal-pass :throw-exn :recover-exn
	)
	(:export :bad-tail :bind-params :boole-branch :type-pred-body
		:destructure :destructure/val
	)
)
(in-package :abyss/helpers)

(declaim (ftype (function (t) (function (t) t)) bad-tail))

(defun bad-tail (tail)
	(lambda (result)
		(recover-exn (make-improper-list tail result))
	)
)

(defmacro bind-params (args (env &rest params) &body body)
	(setf body `(progn ,@body))
	(labels (
		(impl (x y)
			(cond
				((null y)
					`(if (null ,x)
						,body
						(throw-exn (make-arg-null ,x))
					)
				)
				((consp y)
					(setf body (impl `(cdr ,x) (cdr y)))
					`(if (consp ,x)
						,(impl `(car ,x) (car y))
						(throw-exn (make-arg-pair ,x))
					)
				)
				((eq t y) body)
				((symbolp y)
					`(let ((,y ,x)) ,body)
				)
				(t (error "Bad parameter binding."))
			)
		))
		(if env
			`(let (,(list env `(car ,args)))
				,(impl `(cdr ,args) params)
			)
			(impl `(cdr ,args) params)
		)
	)
)

(defmacro boole-branch (check t-branch f-branch)
	`(cond
		((eq ,check +true+) ,t-branch)
		((eq ,check +false+) ,f-branch)
		(t (throw-exn (make-type-exn ,check +tid-boole+)))
	)
)

(defmacro type-pred-body (args x pred)
	`(bind-params ,args (nil ,x)
		(normal-pass (if ,pred
			+true+
			+false+
		))
	)
)

(defmacro destructure-body (table binding ret)
	`(let ((pending nil) (used (make-hash-table :test 'eq)))
		(labels (
			(impl (x)
				(macrolet (
				(advance ()
					'(if pending
						(let ((next (pop pending)))
							(setf ,binding (first next))
							(impl (cdr next))
						)
						(normal-pass ,ret)
					)
				))
				(typecase ,binding
					(cons
						(if (consp x)
							(progn
								(push (cons (cdr ,binding) (cdr x)) pending)
								(setf ,binding (first ,binding))
								(impl (first x))
							)
							(throw-exn (make-arg-pair x))
						)
					)
					(null
						(if (null x)
							(advance)
							(throw-exn (make-arg-null x))
						)
					)
					(abyss/types::glyph
						(if (gethash ,binding used)
							(throw-exn (make-arg-repeat x))
							(progn
								(setf (gethash ,binding ,table) x)
								(setf (gethash ,binding used) t)
								(advance)
							)
						)
					)
					(abyss/types::ignore-type
						(advance)
					)
					(t (throw-exn (make-bad-param ,binding)))
				))
			))
			#'impl
		)
	)
)

(declaim (ftype (function (hash-table t) function) destructure))

(defun destructure (table binding)
	(destructure-body table binding +inert+)
)

(declaim (ftype (function (hash-table t t) function) destructure/val))

(defun destructure/val (table binding ret)
	(destructure-body table binding ret)
)
