
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

(defun evaluate (x env)
	"The main evaluation function."
	(cond
		((keywordp x)
			(multiple-value-bind (found v) (env-lookup env x)
				(if found
					(normal-pass v)
					(recover-exn (make-sym-not-found x env))
				)
			)
		)
		((consp x)
			(push-frame (precombine (cons env (cdr x))))
			(evaluate (car x) env)
		)
		(t (normal-pass x))
	)
)

(declaim (ftype (function (abyss/types::applicative) function) combine))

(defun precombine (args)
	"Initial dispatch on combiner type."
	(lambda (combiner)
		(cond
			((applicative-p combiner)
				(funcall (combine combiner) args)
			)
			((functionp combiner)
				(funcall combiner args)
			)
			; applicatives can only hold functions or applicatives
			; so only at this point can an invalid combiner be encountered
			(t (throw-exn (make-invalid-comb combiner)))
		)
	)
)

(defun combine (app)
	"Unwraps applicatives for combination."
	(lambda (args)
			(let ((combiner (app-comb app)))
				(declare (type (or function abyss/types::applicative)
					combiner))
				(if (functionp combiner)
					(push-frame combiner)
					(push-frame (combine combiner))
				)
			)
			(let ((env (car args)) (tail (cdr args)))
				(if (consp tail)
					(let ((head (list env nil)))
						(push-frame (eval-map env (cdr tail) head (cdr head)))
						(evaluate (car tail) env)
					)
					(let ((head (list env)))
						(push-frame (eval-tail head head))
						(evaluate tail env)
					)
				)
			)
		)
)

(declaim (ftype (function (cons cons) function) eval-tail))

(defun eval-tail (head tail)
	"Finishs up for `eval-map`."
	(lambda (prev)
		(setf (cdr tail) prev)
		(normal-pass head)
	)
)

(declaim (ftype
	(function (abyss/environment::environment t cons cons) function)
	eval-map))

(defun eval-map (env next head tail)
	"Evaluates arguments passed to an applicative."
	(lambda (prev)
		(setf (car tail) prev)
		(if (consp next)
			(let ((fresh (list nil)))
				(setf (cdr tail) fresh)
				(push-frame (eval-map env (cdr next) head fresh))
				(evaluate (car next) env)
			)
			(progn
				(push-frame (eval-tail head tail))
				(evaluate next env)
			)
		)
	)
)
