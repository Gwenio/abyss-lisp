
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

(uiop:define-package :abyss/environment
	(:use :cl)
	(:export :make-environment :environment-p :env-table :env-lookup
		:env-key-not-found :base-env-child :extend-base-env
	)
)
(in-package :abyss/environment)

(defstruct
	(environment
		(:conc-name env-)
		(:constructor make-environment (parent))
	)
	"Symbol binding environment."
	(table (make-hash-table :test 'eq)
		:type hash-table
		:read-only t
	)
	(parent nil
		:read-only t
	)
)

(define-condition env-key-not-found (error) ())

(defun env-lookup (env key)
	(multiple-value-bind (value found) (gethash key (env-table env))
		(if found
			value
			(let ((parent (env-parent env)))
				(if parent
					(env-lookup parent key)
					(error 'env-key-not-found)
				)
			)
		)
	)
)

(defvar *base-env* (make-environment nil)
	"An environment containing to core built-ins."
)

(defun base-env-child ()
	"Returns a new child of `*base-env*`."
	(make-environment *base-env*)
)

(defun extend-base-env (&rest body)
	"Adds definitions to `*base-env*`. Expects an `alist` with `keyword` keys."
	(loop for x in body
		with table = (env-table *base-env*)
		for key = (car x)
		for val = (cadr x)
		do (setf (gethash key table) val)
	)
)
