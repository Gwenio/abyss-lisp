
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
	(:import-from :abyss/types :+eff-exn+)
	(:import-from :abyss/error :make-bad-cont)
	(:import-from :abyss/context
		:shift-context :resume-context :final-guard :normal-pass
		:context-handler
	)
	(:export :perform-effect :continuation-p
		:resume-cont :resume-cont/h :resume-cont/call :resume-cont/call+h
	)
)
(in-package :abyss/continuation)

(defstruct
	(continuation
		(:conc-name cont-)
		(:constructor make-continuation ()))
	; the context to resume, `nil` if invalidated
	(ctx)
	; the handler to set when resumed
	(handler)
	; if `nil` normal-pass the value on resume, if `t` funcall it
	(action)
)

(defun invalidate-cont (cont)
	"Used as a guard to invalidate a continuation."
	#'(lambda (x)
		(setf (cont-ctx cont) nil)
		(if (cont-action cont)
			(funcall x)
			(normal-pass x)
		)
	)
)

(defun perform-effect (x eff)
	"Initiates effect handling."
	(let ((cont (make-continuation)))
		(final-guard (invalidate-cont cont))
		(multiple-value-bind (h k) (shift-context eff)
			(setf (cont-ctx cont) k)
			(setf (cont-handler cont) (context-handler))
			(funcall h (cons x cont))
		)
	)
)

(defmacro resume-cont-body (x cont handler &optional action)
	(let ((suspended (gensym "suspended")))
		`(let ((,suspended (cont-ctx ,cont)))
			(if ,suspended
				(progn
					(context-handler ,handler)
					(resume-context ,suspended)
					,@(if action
						`((setf (cont-action ,cont) ,action))
					)
					(normal-pass ,x)
				)
				(perform-effect (make-bad-cont ,cont) +eff-exn+)
			)
		)
	)
)

; deep handler
(defun resume-cont (x cont)
	"Resumes a continuation if it is valid."
	(resume-cont-body x cont (cont-handler cont))
)

; shallow handler support
(defun resume-cont/h (x cont handler)
	"Like `resume-cont`, but also sets a new handler."
	(resume-cont-body x cont handler)
)

; bidirectional effect handling support,
(defun resume-cont/call (f cont)
	"Calls `f` at the point `cont` resumes at."
	(resume-cont-body f cont (cont-handler cont) t)
)

; bidirectional + shallow effect handling support
(defun resume-cont/call+h (f cont handler)
	"Like `resume-cont/call`, but also sets a new handler."
	(resume-cont-body f cont handler t)
)
