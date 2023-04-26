
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

(uiop:define-package :abyss/continuation
	(:use :cl)
	(:import-from :abyss/types :*eff-bad-continuation*)
	(:import-from :abyss/context
		:shift-context :resume-context :final-guard :normal-pass
		:context-handler
	)
	(:export :perform-effect :perform-effect/k :resume-cont :resume-cont/h
		:continuation-p
	)
)
(in-package :abyss/continuation)

(defstruct
	(continuation
		(:conc-name cont-)
		(:constructor make-continuation (ctx)))
	(ctx)
)

(defun invalidate-cont (cont)
	"Used as a guard to invalidate a continuation."
	#'(lambda (x)
		(setf (cont-ctx cont) nil)
		(normal-pass x)
	)
)

(defun perform-effect (x eff)
	"Initiates effect handling."
	(let ((cont (make-continuation ())))
		(final-guard (invalidate-cont cont))
		(multiple-value-bind (h k) (shift-context eff)
			(setf (cont-ctx cont) k)
			(funcall h (cons x cont))
		)
	)
)

; bidirectional effect handling support
(defun perform-effect/k (x eff cont)
	"Initiates effect handling from a continuation, resusing `cont`."
	(let ((suspended (cont-ctx cont)))
		(if suspended
			(progn
				(resume-context suspended)
				(multiple-value-bind (h k) (shift-context eff)
					(setf (cont-ctx cont) k)
					(funcall h (cons x cont))
				)
			)
			(perform-effect cont *eff-bad-continuation*)
		)
	)
)

(defun resume-cont (x cont)
	"Resumes a continuation if it is valid."
	(let ((suspended (cont-ctx cont)))
		(if suspended
			(progn
				(resume-context suspended)
				(normal-pass x)
			)
			(perform-effect cont *eff-bad-continuation*)
		)
	)
)

; shallow handler support
(defun resume-cont/h (x cont handler)
	"Like `resume-cont`, but also sets a new handler."
	(let ((suspended (cont-ctx cont)))
		(if suspended
			(progn
				(context-handler handler)
				(resume-context suspended)
				(normal-pass x)
			)
			(perform-effect cont *eff-bad-continuation*)
		)
	)
)
