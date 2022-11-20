
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

(uiop:define-package :abyss/context
	(:use :cl)
	(:export :shift-context :resume-context :fresh-context :initial-context
		:error-guard :final-guard :push-frame :normal-pass
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

(defun unwind-guards (stop)
	"Unwinds guards until `stop` is reached."
	(labels (
			(unwinder (ctx x)
				(let ((guard (pop (ctx-guards ctx))))
					(unless (eq stop (ctx-guards ctx))
						(push-frame ctx #'unwinder)
					)
					(funcall guard ctx x)
				)
			)
		)
		#'unwinder
	)
)

(defun discard-context (child)
	"Discards a child context, unwinds all guards."
	#'(lambda (parent x)
		(print "discard")
		(if (ctx-guards child)
			; if child has guards, the pending chain is cyclic
			; even if pending points directly back to child
			; the last context in the chain will be child
			(loop for current = (shiftf (ctx-pending child) nil)
				then (ctx-pending current)
				; append parent's guards to child's with mutation
				;
				; Note: it is expected that guard lists will be short unless
				; an unwind is triggered while in the process of unwinding.
				; Keeping guards scheduled for unwinding in a guard list
				; while unwinding makes 'double unwinding' well defined
				initially (when (ctx-guards parent)
					(nconc (ctx-guards child) (ctx-guards parent))
				)
				while current
				; set guards to `nil` so they are only scheduled once
				; a context can come up both as part of a continuation chain
				; and from its own `discard-context` in its parent's guards
				;
				; Just scheduling child's guards would be enough to ensure all
				; guards that need to be called will get scheduled eventually
				; However, it is more natural to expect unwinding to begin with
				; the guards on the outermost suspended context
				;
				; IMPORTANT:
				; the first guard should invalidate the continuation
				; so that discarded contexts cannot be resumed by user code
				nconc (shiftf (ctx-guards current) nil) into temp
				finally (return
					(funcall
						; unwind till only original guards of `parent` remain
						(unwind-guards (shiftf (ctx-guards parent) temp))
						parent x)
				)
			)
			(normal-pass parent x)
		)
	)
)

(defun next-context (ctx x)
	"Placed as the final frame on a context to switch to the next."
	(funcall (funcall (ctx-handler ctx) nil) (ctx-pending ctx) x))

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
				(shiftf (ctx-pending current) ctx)))
	)
)

(defun resume-context (current target handler)
	"Resumes the suspended context. Sets target's handler."
	; supports shallow handlers.
	; the only time handler needs to be set is on resume
	; hence no separate function to set it
	(setf (ctx-handler target) handler)
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
