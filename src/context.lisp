
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

(uiop:define-package :abyss/context
	(:use :cl)
	(:export :shift-context :resume-context :fresh-context :initial-context
		:error-guard :final-guard :push-frame :normal-pass :context-handler
	)
)
(in-package :abyss/context)

(defstruct
	(context
		(:conc-name ctx-)
		(:constructor make-context (pending handler))
	)
	"Interpreter evaluation context."
	; `frames` represents a call stack.
	; Ideally an implementation would switch out the 'real stack'
	; and we would be storing stacks that are not currently in use
	; but for a prototype that is too much work so an array of closures
	; is used to simulate it
	;
	; the closures take a context and an accumulator as parameters
	(frames (make-array 0 :fill-pointer 0 :adjustable t)
		:read-only t
	)
	; `pending` points to the context to switch to if `frames` is emptied.
	(pending)
	; `handler` passed an effect and returns a function to call on the
	; target context
	; `nil` is passed for a 'normal' return
	(handler)
	; `guards` is a list of closures to call if the context is dicarded
	(guards)
)

(defun unwind-guards (ctx x)
	"Unwinds guards until `stop` is reached."
	(if (ctx-guards ctx)
		(progn
			(push-frame ctx #'unwind-guards)
			(call-guard ctx x)
		)
		(next-context ctx x)
	)
)

(defun discard-context (child)
	"Discards a child context, unwinds all guards."
	#'(lambda (parent x)
		(if (ctx-guards child)
			; if child has guards, then `pending` points to a context to resume
			; rather than the parent. Set back to parent so handlers are
			; available.
			(let ((leaf (shiftf (ctx-pending child) parent)))
				(loop for current = (ctx-pending leaf)
					then (ctx-pending current)
					until (eq parent current)
					do (push-frame current #'unwind-guards)
					; start at leaf, so continuation is invalidated
					finally (return (unwind-guards leaf x))
				)
			)
			(normal-pass parent x)
		)
	)
)

(defun next-context (ctx x)
	"Placed as the final frame on a context to switch to the next."
	(funcall (funcall (ctx-handler ctx) nil) (ctx-pending ctx) x)
)

(defun shift-context (ctx effect)
	"Returns handler's result, the suspended context, and the resumed context."
	(loop for current = ctx then (ctx-pending current)
		; the handler of the initial context will raise an error if reached
		for found = (funcall (ctx-handler current) effect)
		; the handler of the initial context is expected to raise an error
		until found
		; `current` is needed to construct a continuation for user code
		; `found` is to be called on `(ctx-pending current)`
		;
		; '(ctx-pending current)' is set to 'ctx' to make a cyclic list
		; this is needed for unwinding if the contunation is not resumed
		;
		; a `final-guard` should be placed on `ctx` to invalidate the
		; continuation
		finally (return
			(values found current
				(shiftf (ctx-pending current) ctx))
		)
	)
)

(defun resume-context (current target)
	"Resumes the suspended context."
	; returning the resumed context allows for bidirectional effects
	(shiftf (ctx-pending target) current)
)

(defun fresh-context (ctx handler)
	"Creates a child context. Sets a `final-guard` to discard the child"
	(let ((fresh (make-context ctx handler)))
		(push-frame fresh #'next-context)
		(final-guard ctx (discard-context fresh))
		fresh
	)
)

(defun dummy (_ctx x)
	(declare (ignore _ctx))
	x
)

(defun initial-context (handler)
	"Creates an initial context. `handler` is expected to raise an error."
	(let ((fresh (make-context nil handler)))
		(push-frame fresh #'dummy)
		fresh
	)
)

(defun pop-guard (ctx x)
	"Pops the next guard on a context."
	(pop (ctx-guards ctx))
	(normal-pass ctx x)
)

(defun call-guard (ctx x)
	"Pops the next guard on a context and calls it."
	(funcall (pop (ctx-guards ctx)) ctx x)
)

(defun error-guard (ctx guard)
	"Push a guard that only runs only on unwind."
	(push guard (ctx-guards ctx))
	(push-frame ctx #'pop-guard)
)

(defun final-guard (ctx guard)
	"Push a guard that always runs."
	(push guard (ctx-guards ctx))
	(push-frame ctx #'call-guard)
)

(defun push-frame (ctx fun)
	"Push a closure on the frames stack to be called by `normal-pass`."
	(vector-push-extend fun (ctx-frames ctx))
)

(defun normal-pass (ctx x)
	"Pops and calls the closure of the top of the frames stack."
	(funcall (vector-pop (ctx-frames ctx)) ctx x)
)

(defun context-handler (ctx handler)
	"Sets the handler of a context."
	; supports shallow effect handlers
	(setf (ctx-handler ctx) handler)
)
