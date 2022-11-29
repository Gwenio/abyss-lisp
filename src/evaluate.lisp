
; ISC License (ISC)
;
; Copyright 2022 James Adam Armstrong
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
		:env-lookup :env-key-not-found
	)
	(:import-from :abyss/types
		:app-comb :applicative-p :*eff-invalid-comb* :*eff-sym-not-found*
	)
	(:import-from :abyss/continuation
		:perform-effect
	)
	(:import-from :abyss/context
		:normal-pass :push-frame
	)
	(:export :evaluate)
)
(in-package :abyss/evaluate)

(defun evaluate (ctx x env)
	"The main evaluation function."
	(cond
		((keywordp x)
			(handler-case (env-lookup env x)
				(env-key-not-found ()
					(perform-effect ctx x *eff-sym-not-found*)
				)
				(:no-error (y) (normal-pass ctx y))
			)
		)
		((consp x)
			(push-frame ctx (precombine (cons env (cdr x))))
			(evaluate ctx (car x) env)
		)
		(t (normal-pass ctx x))
	)
)

(defun precombine (args)
	"Initial dispatch on combiner type."
	#'(lambda (ctx combiner)
		(cond
			((applicative-p combiner)
				(funcall (combine combiner) ctx args)
			)
			((functionp combiner)
				(funcall combiner ctx args)
			)
			; applicatives can only hold functions or applicatives
			; so only at this point can an invalid combiner be encountered
			(t (perform-effect ctx combiner *eff-invalid-comb*))
		)
	)
)

; TODO: investigate further use of labels to eliminate extra closue creations
(defun combine (app)
	"Unwraps applicatives for combination."
	(labels (
		(impl (ctx args)
			(let ((combiner (app-comb app)))
				(if (functionp combiner)
					(push-frame ctx combiner)
					(progn
						(setf app (app-comb app))
						(push-frame ctx #'impl)
					)
				)
			)
			(let ((env (car args)) (tail (cdr args)))
				(if (consp tail)
					(let ((head (list env nil)))
						(push-frame ctx (eval-map env (cdr tail) head (cdr head)))
						(evaluate ctx (car tail) env)
					)
					(let ((head (list env)))
						(push-frame ctx (eval-tail head head))
						(evaluate ctx tail env)
					)
				)
			)
		)
		)
		#'impl
	)
)

(defun eval-tail (head tail)
	"Finishs up for `eval-map`."
	#'(lambda (ctx prev)
		(setf (cdr tail) prev)
		(normal-pass ctx head)
	)
)

(defun eval-map (env next head tail)
	"Evaluates arguments passed to an applicative."
	#'(lambda (ctx prev)
		(setf (car tail) prev)
		(if (consp next)
			(let ((fresh (list nil)))
				(setf (cdr tail) fresh)
				(push-frame ctx (eval-map env (cdr next) head fresh))
				(evaluate ctx (car next) env)
			)
			(progn
				(push-frame ctx (eval-tail head tail))
				(evaluate ctx next env)
			)
		)
	)
)
