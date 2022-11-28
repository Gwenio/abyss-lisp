
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
	#'(lambda (ctx x)
		(setf (cont-ctx cont) nil)
		(normal-pass ctx x)
	)
)

(defun perform-effect (ctx x eff)
	"Initiates effect handling."
	(multiple-value-bind (h k next) (shift-context ctx eff)
		(let ((cont (make-continuation k)))
			(final-guard ctx (invalidate-cont cont))
			(funcall h next (cons x cont))
		)
	)
)

; bidirectional effect handling support
(defun perform-effect/k (ctx x eff cont)
	"Initiates effect handling from a continuation, resusing `cont`."
	(let ((suspended (cont-ctx cont)))
		(if suspended
			(let ((resumed (resume-context ctx suspended)))
				(multiple-value-bind (h k next) (shift-context resumed eff)
					(setf (cont-ctx cont) k)
					(funcall h next (cons x cont))
				)
			)
			(perform-effect ctx cont *eff-bad-continuation*)
		)
	)
)

(defun resume-cont (ctx x cont)
	"Resumes a continuation if it is valid."
	(let ((suspended (cont-ctx cont)))
		(if suspended
			(normal-pass (resume-context ctx suspended) x)
			(perform-effect ctx cont *eff-bad-continuation*)
		)
	)
)

; shallow handler support
(defun resume-cont/h (ctx x cont handler)
	"Like `resume-cont`, but also sets a new handler."
	(let ((suspended (cont-ctx cont)))
		(if suspended
			(progn
				(context-handler suspended handler)
				(normal-pass (resume-context ctx suspended) x)
			)
			(perform-effect ctx cont *eff-bad-continuation*)
		)
	)
)
