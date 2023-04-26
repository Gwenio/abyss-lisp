
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
		(:constructor make-context (pending))
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
	; `guards` is a list of closures to call if the context is discarded
	(guards)
)

(defvar *current-ctx*)

(defun unwind-guards (x)
	"Unwinds guards until `stop` is reached."
	(if (ctx-guards *current-ctx*)
		(progn
			(push-frame #'unwind-guards)
			(call-guard x)
		)
		(next-context x)
	)
)

(defun unwind-handler (effect)
	"Handler that initiates unwinding when the context resumes."
	; Does not trigger for effects other than normal return.
	; Normal return is indicated by `effect` being `nil`.
	(if effect nil #'unwind-guards)
)

(defun discard-context (child)
	"Discards a child context, unwinds all guards."
	#'(lambda (x)
		(if (ctx-guards child)
			(progn
				; Set handler for unwinding. The setup for handling effects
				; is not present if we are discarding.
				(setf (ctx-handler child) #'unwind-handler)
				; Ensure the pending context of child is parent.
				(let ((parent *current-ctx*))
					; intervening contexts should be discarded already
					(setf (ctx-pending child) parent)
				)
				(setf *current-ctx* child)
				(unwind-guards x)
			)
			(normal-pass x)
		)
	)
)

(defun next-context (x)
	"Placed as the final frame on a context to switch to the next."
	(let* ((prev *current-ctx*) (next (ctx-pending prev)))
		(setf *current-ctx* next)
		(funcall (funcall (ctx-handler next) ()) x)
	)
)

(defun shift-context (effect)
	"Returns handler's result, the suspended context, and the resumed context."
	(loop with original = *current-ctx*
		with prev = original
		for current = (ctx-pending prev)
		then (progn (setf prev current) (ctx-pending current))
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
		finally (progn
			(setf *current-ctx* current)
			(setf (ctx-pending prev) original)
			(return (values found prev))
		)
	)
)

(defun resume-context (target)
	"Resumes the suspended context."
	; returning the resumed context allows for bidirectional effects
	(rotatef (ctx-pending target) *current-ctx*)
)

(defun fresh-context (run handler)
	"Creates a child context. Sets a `final-guard` to discard the child"
	(let* ((ctx *current-ctx*) (fresh (make-context ctx)))
		(final-guard (discard-context fresh))
		(setf (ctx-handler ctx) handler)
		(setf *current-ctx* fresh)
		(push-frame #'next-context)
		(funcall run)
	)
)

(defun initial-context (run fallback)
	"Creates an initial context. `fallback` is expected to raise an error."
	(let ((*current-ctx* (make-context ())))
		(push-frame #'identity)
		(fresh-context run (lambda (effect)
			(if effect
				(funcall fallback effect)
				#'normal-pass
			)
		))
	)
)

(defun pop-guard (x)
	"Pops the next guard on a context."
	(pop (ctx-guards *current-ctx*))
	(normal-pass x)
)

(defun call-guard (x)
	"Pops the next guard on a context and calls it."
	(funcall (pop (ctx-guards *current-ctx*)) x)
)

(defun error-guard (guard)
	"Push a guard that only runs only on unwind."
	(push guard (ctx-guards *current-ctx*))
	(push-frame #'pop-guard)
)

(defun final-guard (guard)
	"Push a guard that always runs."
	(push guard (ctx-guards *current-ctx*))
	(push-frame #'call-guard)
)

(defun push-frame (fun)
	"Push a closure on the frames stack to be called by `normal-pass`."
	(vector-push-extend fun (ctx-frames *current-ctx*))
)

(defun normal-pass (x)
	"Pops and calls the closure of the top of the frames stack."
	(funcall (vector-pop (ctx-frames *current-ctx*)) x)
)

(defun context-handler (handler)
	"Sets the handler of a context."
	; supports shallow effect handlers
	(setf (ctx-handler *current-ctx*) handler)
)
