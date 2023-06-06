
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

(uiop:define-package :abyss/numbers
	(:use :cl)
	(:import-from :abyss/types
		:+true+ :+false+ :boole-type-p
	)
	(:import-from :abyss/error
		:make-type-exn :make-div-by-zero :make-improper-list
	)
	(:import-from :abyss/context
		:normal-pass :push-frame :throw-exn :recover-exn
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/helpers
		:bad-tail :bind-params :boole-branch :type-pred-body
	)
	(:export
		:int-p-impl :rat-p-impl :num-p-impl
		:add-impl :sub-impl :mul-impl :div-impl
		:min-impl :max-impl :mod-impl :rem-impl
		:abs-impl :num-impl :den-impl
		:=impl :<impl :>impl :<=impl :>=impl
	)
)
(in-package :abyss/numbers)

(defun int-p-impl (args)
	(type-pred-body args x (integerp x))
)

(defun rat-p-impl (args)
	(type-pred-body args x (rationalp x))
)

(defun num-p-impl (args)
	(type-pred-body args x (realp x))
)

(defun add-impl (args)
	(bind-params args (nil . tail)
		(labels ((aux (n)
			(typecase tail
				(cons (let ((x (pop tail)))
					(if (realp x)
						(aux (+ n x))
						(throw-exn (make-type-exn x 'number))
					)
				))
				(null (normal-pass n))
				(t (funcall (bad-tail tail) n))
			)))
			(aux 0)
		)
	)
)

(defun sub-impl (args)
	(bind-params args (nil head . tail)
		(if (realp head)
			(if tail
				(labels ((aux (n)
					(let ((x (pop tail)))
						(if (realp x)
							(typecase tail
								(cons (aux (- n x)))
								(null (normal-pass (- n x)))
								(t (funcall (bad-tail tail) n))
							)
							(throw-exn (make-type-exn x 'number))
						)
					)))
					(aux head)
				)
				(normal-pass (- head))
			)
			(throw-exn (make-type-exn head 'number))
		)
	)
)

(defun mul-impl (args)
	(bind-params args (nil . tail)
		(labels ((aux (n)
			(typecase tail
				(cons (let ((x (pop tail)))
					(if (realp x)
						(aux (* n x))
						(throw-exn (make-type-exn x 'number))
					)
				))
				(null (normal-pass n))
				(t (funcall (bad-tail tail) n))
			)))
			(aux 1)
		)
	)
)

(defun div-impl (args)
	(bind-params args (nil head . tail)
		(if (realp head)
			(if tail
				(labels ((aux (n)
					(let ((x (pop tail)))
						(if (realp x)
							(if (zerop x)
								(typecase tail
									(cons
										(push-frame #'aux)
										(recover-exn (make-div-by-zero n))
									)
									(null (recover-exn (make-div-by-zero n)))
									(t
										(push-frame (bad-tail tail))
										(recover-exn (make-div-by-zero n))
									)
								)
								(typecase tail
									(cons (aux (/ n x)))
									(null (normal-pass (/ n x)))
									(t (funcall (bad-tail tail) n))
								)
							)
							(throw-exn (make-type-exn x 'number))
						)
					)))
					(aux head)
				)
				(normal-pass (/ head))
			)
			(throw-exn (make-type-exn head 'number))
		)
	)
)

(defun min-impl (args)
	(bind-params args (nil head . tail)
		(if (realp head)
			(labels ((aux (n)
				(typecase tail
					(cons (let ((x (pop tail)))
						(if (realp x)
							(aux (min n x))
							(throw-exn (make-type-exn x 'number))
						)
					))
					(null (normal-pass n))
					(t (funcall (bad-tail tail) n))
				)))
				(aux head)
			)
			(throw-exn (make-type-exn head 'number))
		)
	)
)

(defun max-impl (args)
	(bind-params args (nil head . tail)
		(if (realp head)
			(labels ((aux (n)
				(typecase tail
					(cons (let ((x (pop tail)))
						(if (realp x)
							(aux (max n x))
							(throw-exn (make-type-exn x 'number))
						)
					))
					(null (normal-pass n))
					(t (funcall (bad-tail tail) n))
				)))
				(aux head)
			)
			(throw-exn (make-type-exn head 'number))
		)
	)
)

(defun mod-impl (args)
	(bind-params args (nil n divisor)
		(if (realp n)
			(if (realp divisor)
				(normal-pass (mod n divisor))
				(throw-exn (make-type-exn divisor 'number))
			)
			(throw-exn (make-type-exn n 'number))
		)
	)
)

(defun rem-impl (args)
	(bind-params args (nil n divisor)
		(if (realp n)
			(if (realp divisor)
				(normal-pass (rem n divisor))
				(throw-exn (make-type-exn divisor 'number))
			)
			(throw-exn (make-type-exn n 'number))
		)
	)
)

(defun abs-impl (args)
	(bind-params args (nil n)
		(if (realp n)
			(normal-pass (abs n))
			(throw-exn (make-type-exn n 'number))
		)
	)
)

(defun num-impl (args)
	(bind-params args (nil n)
		(if (rationalp n)
			(normal-pass (numerator n))
			(throw-exn (make-type-exn n 'rational))
		)
	)
)

(defun den-impl (args)
	(bind-params args (nil n)
		(if (rationalp n)
			(normal-pass (denominator n))
			(throw-exn (make-type-exn n 'rational))
		)
	)
)

(defmacro num-comp-body (pred args)
	`(bind-params ,args (nil head . tail)
		(if (realp head)
			(labels ((aux (n)
				(typecase tail
					(cons
						(let ((x (pop tail)))
							(if (realp x)
								(if (,pred n x)
									(aux x)
									(normal-pass +false+)
								)
								(throw-exn (make-type-exn head 'number))
							)
						)
					)
					(null
						(normal-pass +true+)
					)
					(t (funcall (bad-tail tail) n))
				)
				))
				(aux head)
			)
			(throw-exn (make-type-exn head 'number))
		)
	)
)

(defun =impl (args)
	(num-comp-body = args)
)

(defun <impl (args)
	(num-comp-body < args)
)

(defun >impl (args)
	(num-comp-body > args)
)

(defun <=impl (args)
	(num-comp-body <= args)
)

(defun >=impl (args)
	(num-comp-body >= args)
)
