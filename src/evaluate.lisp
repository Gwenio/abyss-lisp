
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

(uiop:define-package :abyss/evaluate
	(:use :cl)
	(:import-from :abyss/environment
		:env-lookup
	)
	(:import-from :abyss/types
		:app-comb :applicative-p
	)
	(:import-from :abyss/error
		:make-invalid-comb :make-sym-not-found
	)
	(:import-from :abyss/context
		:normal-pass :push-frame :throw-exn :recover-exn
	)
	(:export :evaluate)
)
(in-package :abyss/evaluate)

(declaim (ftype (function (t abyss/environment::environment) t) evaluate))

(declaim (ftype (function (abyss/environment::environment t) function)
	precombine))

(defun evaluate (x env)
	"The main evaluation function."
	(typecase x
		(keyword
			(multiple-value-bind (found v) (env-lookup env x)
				(if found
					(normal-pass v)
					(recover-exn (make-sym-not-found x env))
				)
			)
		)
		(cons
			(push-frame (precombine env (cons env (cdr x))))
			(evaluate (car x) env)
		)
		(t (normal-pass x))
	)
)

(declaim (ftype (function
	(abyss/types::applicative abyss/environment::environment cons) t)
	combine))

(defun precombine (env args)
	"Initial dispatch on combiner type."
	(lambda (combiner)
		(typecase combiner
			(abyss/types::applicative
				(combine combiner env args)
			)
			(function
				(funcall combiner args)
			)
			; applicatives can only hold functions or applicatives
			; so only at this point can an invalid combiner be encountered
			(t (throw-exn (make-invalid-comb combiner)))
		)
	)
)

(declaim (ftype (function (function cons) function) eval-tail))

(defun eval-tail (done tail)
	"Finishs up for `eval-map`."
	(lambda (prev)
		(setf (cdr tail) prev)
		(funcall done)
	)
)

(declaim (ftype
	(function (abyss/environment::environment t function cons) function)
	eval-map))

(defun eval-map (env next done tail)
	"Evaluates arguments passed to an applicative."
	(lambda (prev)
		(let ((fresh (setf (cdr tail) (list prev))))
			(typecase next
				(cons
					(push-frame (eval-map env (cdr next) done fresh))
					(evaluate (car next) env)
				)
				(keyword
					(push-frame (eval-tail done fresh))
					(evaluate next env)
				)
				(t
					(setf (cdr fresh) next)
					(funcall done)
				)
			)
		)
	)
)

(defun combine (app env args)
	"Unwraps applicatives for combination."
	(let ((next
		(let ((combiner (app-comb app)))
			(declare (type (or function abyss/types::applicative)
				combiner))
			(if (functionp combiner)
				(lambda () (funcall combiner args))
				(lambda () (combine combiner env args))
			)
		)))
		(let ((tail (cdr args)))
			(typecase tail
				(cons
					(push-frame (eval-map env (cdr tail) next args))
					(evaluate (car tail) env)
				)
				(keyword
					(push-frame (eval-tail next args))
					(evaluate tail env)
				)
				(t (funcall next))
			)
		)
	)
)
