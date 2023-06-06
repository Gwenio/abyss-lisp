
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
		:+true+ :+false+ :boole-type-p
	)
	(:import-from :abyss/error
		:make-improper-list
	)
	(:import-from :abyss/context
		:normal-pass :push-frame :recover-exn
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/helpers
		:bind-params :boole-branch :type-pred-body
	)
	(:export
		:not-impl :and-app-impl :or-app-impl :and-oper-impl :or-oper-impl
		:eq-impl :boole-p-impl
	)
)
(in-package :abyss/boole)

(defun boole-p-impl (args)
	(type-pred-body args x (boole-type-p x))
)

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
				(t (recover-exn (make-improper-list y +true+)))
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
				(t (recover-exn (make-improper-list y +false+)))
			)))
			(aux x)
		)
	)
)

(declaim (ftype (function (abyss/environment::environment t) function)
	and-oper-aux or-oper-aux))

(defun and-oper-aux (env next)
	(if next
		(labels ((impl (prev)
			(boole-branch prev
				(if (listp next)
					(let ((head (pop next)))
						(unless (null next)
							(push-frame #'impl)
						)
						(evaluate head env)
					)
					(recover-exn (make-improper-list next +true+))
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
				(if (listp next)
					(let ((head (pop next)))
						(unless (null next)
							(push-frame #'impl)
						)
						(evaluate head env)
					)
					(recover-exn (make-improper-list next +false+))
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
