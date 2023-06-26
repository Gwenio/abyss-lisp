
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

(uiop:define-package :abyss/lists
	(:use :cl)
	(:import-from :abyss/types
		:+tid-integer+ :+tid-cons+
	)
	(:import-from :abyss/error
		:make-type-exn :make-improper-list :make-bounds-exn
	)
	(:import-from :abyss/context
		:normal-pass :throw-exn :recover-exn
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/helpers
		:bad-tail :bind-params :type-pred-body
	)
	(:export :null-p-impl :cons-p-impl :list-p-impl
		:cons-impl :list-impl :list*-impl :list-len-impl
		:first-impl :second-impl :third-impl :nth-impl
		:tail-impl :nth-tail-impl :last-impl :last-n-impl
	)
)
(in-package :abyss/lists)

(defun null-p-impl (args)
	(type-pred-body args x (null x))
)

(defun cons-p-impl (args)
	(type-pred-body args x (consp x))
)

(defun list-p-impl (args)
	(type-pred-body args x (listp x))
)

(defun cons-impl (args)
	(bind-params args (nil x y)
		(normal-pass (cons x y))
	)
)

(defun list-impl (args)
	(normal-pass (cdr args))
)

(defun list*-impl (args)
	(bind-params args (nil x . y)
		(typecase y
			(cons (let ((acc (cons x (car y))))
				(loop for z = (cdr y) then (cdr z)
					for prev = acc then (cdr prev)
					while (consp z)
					do (setf (cdr prev) (cons (cdr prev) (car z)))
					finally (if (null z)
						(return (normal-pass acc))
						(return (recover-exn (make-improper-list z acc)))
					)
				)
			))
			(null (normal-pass x))
			(t (recover-exn (make-improper-list y x)))
		)
	)
)

(defun first-impl (args)
	(bind-params args (nil x)
		(if (consp x)
			(normal-pass (car x))
			(throw-exn (make-type-exn x +tid-cons+))
		)
	)
)

(defun second-impl (args)
	(bind-params args (nil (t . x))
		(if (consp x)
			(normal-pass (car x))
			(throw-exn (make-type-exn x +tid-cons+))
		)
	)
)

(defun third-impl (args)
	(bind-params args (nil (t t . x))
		(if (consp x)
			(normal-pass (car x))
			(throw-exn (make-type-exn x +tid-cons+))
		)
	)
)

(defun nth-impl (args)
	(bind-params args (nil n x)
		(if (integerp n)
			(if (<= 0 n most-positive-fixnum)
				(loop for count below n
					if (consp x)
					do (pop x)
					else
					do (return (throw-exn (make-type-exn x +tid-cons+)))
					end
					finally (return (normal-pass (car x)))
				)
				(throw-exn (make-bounds-exn n 0 most-positive-fixnum))
			)
			(throw-exn (make-type-exn n +tid-integer+))
		)
	)
)

(defun tail-impl (args)
	(bind-params args (nil x)
		(if (consp x)
			(normal-pass (cdr x))
			(throw-exn (make-type-exn x +tid-cons+))
		)
	)
)

(defun nth-tail-impl (args)
	(bind-params args (nil n x)
		(if (integerp n)
			(if (<= 0 n most-positive-fixnum)
				(loop for count below n
					if (consp x)
					do (pop x)
					else
					do (return (throw-exn (make-type-exn x +tid-cons+)))
					end
					finally (return (normal-pass (cdr x)))
				)
				(throw-exn (make-bounds-exn n 0 most-positive-fixnum))
			)
			(throw-exn (make-type-exn n +tid-integer+))
		)
	)
)

(defun last-impl (args)
	(bind-params args (nil x)
		(if (consp x)
			(normal-pass (last x))
			(normal-pass x)
		)
	)
)

(defun last-n-impl (args)
	(bind-params args (nil n x)
		(if (integerp n)
			(if (<= 0 n most-positive-fixnum)
				(if (consp x)
					(normal-pass (last x n))
					(normal-pass x)
				)
				(throw-exn (make-bounds-exn n 0 most-positive-fixnum))
			)
			(throw-exn (make-type-exn n +tid-integer+))
		)
	)
)

(defun list-len-impl (args)
	(bind-params args (nil x)
		; lists are unlikely to exceed what fits in a fixnum
		; especially with SBCL limiting heap size to like 1GB
		(loop for count fixnum from 0 upto most-positive-fixnum
			do (if (consp x)
				(pop x)
				(return (normal-pass count))
			)
			finally (return (throw-exn
				(make-bounds-exn count nil most-positive-fixnum)))
		)
	)
)

; (defun map-impl (args))

; (defun append-impl (args))

; (defun filter-impl (args))

; (defun assoc-impl (args))

; (defun member-p-impl (args))

; (defun reduce-impl (args))
