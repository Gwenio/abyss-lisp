
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

(uiop:define-package :abyss/applicatives
	(:use :cl)
	(:import-from :abyss/types
		:+inert+ :+ignore+ :+true+ :+false+ :inert-p :ignore-p :make-app
		:applicative-p :applicative :app-comb :+eff-exn+
	)
	(:import-from :abyss/error
		:make-arg-pair :make-arg-null :make-arg-repeat :make-bad-param
		:make-type-exn :make-invalid-comb
	)
	(:import-from :abyss/environment
		:make-environment :environment-p :environment
	)
	(:import-from :abyss/context
		:normal-pass :perform-effect
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/helpers
		:bind-params :type-pred-body
	)
	(:export :eval-impl :wrap-impl :unwrap-impl :make-env-impl :apply-impl
		:current-env-impl :ignore-p-impl :inert-p-impl :symbol-p-impl
		:oper-p-impl :app-p-impl :comb-p-impl :env-p-impl
	)
)
(in-package :abyss/applicatives)

(defun eval-impl (args)
	(bind-params args (nil x env)
		(if (environment-p env)
			(evaluate x env)
			(perform-effect (make-type-exn env 'environment) +eff-exn+)
		)
	)
)

(defun wrap-impl (args)
	(bind-params args (nil x)
		(if (or (functionp x) (applicative-p x))
			(normal-pass (make-app x))
			(perform-effect (make-invalid-comb x) +eff-exn+)
		)
	)
)

(defun unwrap-impl (args)
	(bind-params args (nil x)
		(if (applicative-p x)
			(normal-pass (app-comb x))
			(perform-effect (make-type-exn x 'applicative) +eff-exn+)
		)
	)
)

(defun apply-impl (args)
	(bind-params args (nil app x . opt)
		(if (applicative-p app)
			(typecase opt
				(null
					(evaluate (cons app x) (make-environment nil))
				)
				(cons
					(bind-params opt (env)
						(if (environment-p env)
							(evaluate (cons app x) env)
							(perform-effect (make-type-exn env 'environment)
								+eff-exn+)
						)
					)
				)
				(t (perform-effect (make-arg-pair opt) +eff-exn+))
			)
			(perform-effect (make-type-exn app 'applicative) +eff-exn+)
		)
	)
)

(defun make-env-impl (args)
	(if (cdr args)
		(bind-params args (nil parent)
			(if (environment-p parent)
				(normal-pass (make-environment parent))
				(perform-effect
					(make-type-exn parent 'environment)
					+eff-exn+)
			)
		)
		(normal-pass (make-environment nil))
	)
)

(defun current-env-impl (args)
	(bind-params args (env)
		(normal-pass env)
	)
)

(defun ignore-p-impl (args)
	(type-pred-body args x (ignore-p x))
)

(defun inert-p-impl (args)
	(type-pred-body args x (inert-p x))
)

(defun symbol-p-impl (args)
	(type-pred-body args x (keywordp x))
)

(defun oper-p-impl (args)
	(type-pred-body args x (functionp x))
)

(defun app-p-impl (args)
	(type-pred-body args x (applicative-p x))
)

(defun comb-p-impl (args)
	(type-pred-body args x (or (functionp x) (applicative-p x)))
)

(defun env-p-impl (args)
	(type-pred-body args x (environment-p x))
)
