
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

(uiop:define-package :abyss/boole
	(:use :cl)
	(:import-from :abyss/types
		:+inert+ :+ignore+ :+true+ :+false+ :ignore-p :make-app
		:+eff-exn+
	)
	(:import-from :abyss/environment
		:make-environment :environment-p :env-table :base-env-child
		:extend-base-env
	)
	(:import-from :abyss/context
		:normal-pass :push-frame :perform-effect
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/helpers
		:bad-tail :bind-params :boole-branch
	)
	(:export
		:not-impl :and-app-impl :or-app-impl :and-oper-impl :or-oper-impl
		:eq-impl
	)
)
(in-package :abyss/boole)

(defun not-impl (args)
	(bind-params args (nil x)
		(boole-branch x
			(normal-pass +false+)
			(normal-pass +true+)
		)
	)
)

(defun and-app-impl (args)
	(bind-params args (nil . x)
		(labels ((aux (y)
			(typecase y
				(cons (boole-branch (car y)
					(aux (cdr y))
					(normal-pass +false+)
				))
				(null (normal-pass +true+))
				(t (bad-tail y))
			)))
			(aux x)
		)
	)
)

(defun or-app-impl (args)
	(bind-params args (nil . x)
		(labels ((aux (y)
			(typecase y
				(cons (boole-branch (car y)
					(normal-pass +true+)
					(aux (cdr y))
				))
				(null (normal-pass +false+))
				(t (bad-tail y))
			)))
			(aux x)
		)
	)
)

(defun and-oper-aux (env next)
	(if next
		(labels ((impl (prev)
			(boole-branch prev
				(if (consp next)
					(let ((head (pop next)))
						(unless (null next)
							(push-frame #'impl)
						)
						(evaluate head env)
					)
					(bad-tail next)
				)
				(normal-pass prev)
			)))
			#'impl
		)
		#'normal-pass
	)
)

(defun and-oper-impl (args)
	(funcall (and-oper-aux (car args) (cdr args)) +true+)
)

(defun or-oper-aux (env next)
	(if next
		(labels ((impl (prev)
			(boole-branch prev
				(normal-pass prev)
				(if (consp next)
					(let ((head (pop next)))
						(unless (null next)
							(push-frame #'impl)
						)
						(evaluate head env)
					)
					(bad-tail next)
				)
			)))
			#'impl
		)
		#'normal-pass
	)
)

(defun or-oper-impl (args)
	(funcall (or-oper-aux (car args) (cdr args)) +false+)
)

(defun eq-impl (args)
	(bind-params args (nil x y)
		(normal-pass
			(if (eq x y)
				+true+
				+false+
			))
	)
)
