
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

(uiop:define-package :abyss/environment
	(:use :cl)
	(:export
		:make-environment :environment-p :env-table :env-lookup
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

(defun env-lookup (env key)
	(multiple-value-bind (value found) (gethash key (env-table env))
		(if found
			(values t value)
			(let ((parent (env-parent env)))
				(if parent
					(env-lookup parent key)
					(values nil nil)
				)
			)
		)
	)
)
