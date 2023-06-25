
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
		:+true+ :+false+ :boole-type-p :+tid-integer+ :+tid-ratio+
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

(defvar +tid-number+ (list +tid-integer+ +tid-ratio+))
(defvar +tid-rational+ (list +tid-integer+ +tid-ratio+))

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
			(declare (type rational n))
			(typecase tail
				(cons (let ((x (pop tail)))
					(if (rationalp x)
						(aux (+ n x))
						(throw-exn (make-type-exn x +tid-number+))
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
		(if (rationalp head)
			(if tail
				(labels ((aux (n)
					(declare (type rational n))
					(let ((x (pop tail)))
						(if (rationalp x)
							(typecase tail
								(cons (aux (- n x)))
								(null (normal-pass (- n x)))
								(t (funcall (bad-tail tail) n))
							)
							(throw-exn (make-type-exn x +tid-number+))
						)
					)))
					(aux head)
				)
				(normal-pass (- head))
			)
			(throw-exn (make-type-exn head +tid-number+))
		)
	)
)

(defun mul-impl (args)
	(bind-params args (nil . tail)
		(labels ((aux (n)
			(declare (type rational n))
			(typecase tail
				(cons (let ((x (pop tail)))
					(if (rationalp x)
						(aux (* n x))
						(throw-exn (make-type-exn x +tid-number+))
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
		(if (rationalp head)
			(if tail
				(labels ((aux (n)
					(declare (type rational n))
					(let ((x (pop tail)))
						(if (rationalp x)
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
							(throw-exn (make-type-exn x +tid-number+))
						)
					)))
					(aux head)
				)
				(normal-pass (/ head))
			)
			(throw-exn (make-type-exn head +tid-number+))
		)
	)
)

(defun min-impl (args)
	(bind-params args (nil head . tail)
		(if (rationalp head)
			(labels ((aux (n)
				(declare (type rational n))
				(typecase tail
					(cons (let ((x (pop tail)))
						(if (rationalp x)
							(aux (min n x))
							(throw-exn (make-type-exn x +tid-number+))
						)
					))
					(null (normal-pass n))
					(t (funcall (bad-tail tail) n))
				)))
				(aux head)
			)
			(throw-exn (make-type-exn head +tid-number+))
		)
	)
)

(defun max-impl (args)
	(bind-params args (nil head . tail)
		(if (rationalp head)
			(labels ((aux (n)
				(declare (type rational n))
				(typecase tail
					(cons (let ((x (pop tail)))
						(if (rationalp x)
							(aux (max n x))
							(throw-exn (make-type-exn x +tid-number+))
						)
					))
					(null (normal-pass n))
					(t (funcall (bad-tail tail) n))
				)))
				(aux head)
			)
			(throw-exn (make-type-exn head +tid-number+))
		)
	)
)

(defun mod-impl (args)
	(bind-params args (nil n divisor)
		(if (rationalp n)
			(if (rationalp divisor)
				(normal-pass (mod n divisor))
				(throw-exn (make-type-exn divisor +tid-number+))
			)
			(throw-exn (make-type-exn n +tid-number+))
		)
	)
)

(defun rem-impl (args)
	(bind-params args (nil n divisor)
		(if (rationalp n)
			(if (rationalp divisor)
				(normal-pass (rem n divisor))
				(throw-exn (make-type-exn divisor +tid-number+))
			)
			(throw-exn (make-type-exn n +tid-number+))
		)
	)
)

(defun abs-impl (args)
	(bind-params args (nil n)
		(if (rationalp n)
			(normal-pass (abs n))
			(throw-exn (make-type-exn n +tid-number+))
		)
	)
)

(defun num-impl (args)
	(bind-params args (nil n)
		(if (rationalp n)
			(normal-pass (numerator n))
			(throw-exn (make-type-exn n +tid-rational+))
		)
	)
)

(defun den-impl (args)
	(bind-params args (nil n)
		(if (rationalp n)
			(normal-pass (denominator n))
			(throw-exn (make-type-exn n +tid-rational+))
		)
	)
)

(defmacro num-comp-body (pred args)
	`(bind-params ,args (nil head . tail)
		(if (rationalp head)
			(labels ((aux (n)
				(declare (type rational n))
				(typecase tail
					(cons
						(let ((x (pop tail)))
							(if (rationalp x)
								(if (,pred n x)
									(aux x)
									(normal-pass +false+)
								)
								(throw-exn (make-type-exn head +tid-number+))
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
			(throw-exn (make-type-exn head +tid-number+))
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
