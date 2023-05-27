
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
		:+true+ :+false+ :+eff-exn+ :boole-type
	)
	(:import-from :abyss/error
		:make-improper-list :make-arg-pair :make-arg-null :make-type-exn
	)
	(:import-from :abyss/context
		:normal-pass :perform-effect
	)
	(:export :bad-tail :bind-params :boole-branch :type-pred-body)
)
(in-package :abyss/helpers)

(defun bad-tail (x)
	(perform-effect (make-improper-list x) +eff-exn+)
)

(defmacro bind-params (args (env &rest params) &body body)
	(setf body `(progn ,@body))
	(labels (
		(impl (x y)
			(cond
				((null y)
					`(if (null ,x)
						,body
						(perform-effect (make-arg-null ,x) +eff-exn+)
					)
				)
				((consp y)
					(setf body (impl `(cdr ,x) (cdr y)))
					`(if (consp ,x)
						,(impl `(car ,x) (car y))
						(perform-effect (make-arg-pair ,x) +eff-exn+)
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
			`(progn (pop ,args) ,(impl args params))
		)
	)
)

(defmacro boole-branch (check t-branch f-branch)
	`(cond
		((eq ,check +true+) ,@t-branch)
		((eq ,check +false+) ,@f-branch)
		(t (perform-effect
			(make-type-exn ,check 'boole)
			+eff-exn+)
		)
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
