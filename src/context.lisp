
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
	(:import-from :abyss/types :+eff-exn+ :+eff-fix+ :+eff-ret+)
	(:import-from :abyss/error :make-bad-cont)
	(:export :shift-context :resume-context :fresh-context :initial-context
		:error-guard :final-guard :push-frame :normal-pass
		:perform-effect :perform-effect/k :continuation-p
		:resume-cont :resume-cont/h :resume-cont/call :resume-cont/call+h
		:throw-exn :recover-exn
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
	; but for a prototype that is too much work so cons cells are used
	;
	; Previous testing showed pushing and popping on arrays to be
	; more expensive most of the time for recursive `fib`
	; This goes both for `vector-push-extend` and switching to
	; a new vector when `vector-push` fails.
	;
	; the closures take a context and an accumulator as parameters
	(frames)
	; `pending` points to the context to switch to if `frames` is emptied.
	(pending)
	; `handler` passed an effect and returns a function to call on the
	; target context
	; `nil` is passed for a 'normal' return
	(handler #'null-handler)
	; `guards` is a list of closures to call if the context is discarded
	(guards)
)

(defvar *current-ctx*)

(defun unwind-guards (x)
	"Unwinds guards until `stop` is reached."
	(let ((guard (pop (ctx-guards *current-ctx*))))
		(if guard
			(progn
				(push-frame #'unwind-guards)
				(funcall guard x)
			)
			(next-context x)
		)
	)
)

(defun null-handler (effect)
	"Handler that handles no effect except normal return."
	(if (eq effect +eff-ret+)
		#'normal-pass
		nil
	)
)

(defun discard-context (child)
	"Discards a child context, unwinds all guards."
	#'(lambda (x)
		(if (ctx-guards child)
			(progn
				; Set handler for unwinding. The setup for handling effects
				; is not present if we are discarding.
				(setf (ctx-handler child) #'null-handler)
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
	(let* (
		(prev *current-ctx*)
		(next (ctx-pending prev))
		(handler (shiftf (ctx-handler next) #'null-handler))
		)
		(setf *current-ctx* next)
		(funcall (funcall handler +eff-ret+) x)
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
		(fresh-context run fallback)
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
	(push fun (ctx-frames *current-ctx*))
)

(defun normal-pass (x)
	"Pops and calls the closure of the top of the frames stack."
	(funcall (pop (ctx-frames *current-ctx*)) x)
)

(defun context-handler (&optional (handler #'null-handler))
	"Sets the handler of the current context, returning the previous."
	; supports shallow effect handlers
	(shiftf (ctx-handler *current-ctx*) handler)
)

(defstruct
	(continuation
		(:conc-name cont-)
		(:constructor make-continuation (ctx handler)))
	; the context to resume, `nil` if invalidated
	(ctx)
	; the handler to set when resumed
	(handler)
)

(defun invalidate-cont (cont)
	"Used as a guard to invalidate a continuation."
	#'(lambda (x)
		(setf (cont-ctx cont) nil)
		(normal-pass x)
	)
)

(defmacro perform/k-body (x eff)
	`(multiple-value-bind (h k) (shift-context ,eff)
		(let ((cont (make-continuation k (context-handler))))
			(push (invalidate-cont cont) (ctx-guards k))
			(funcall h (cons ,x cont))
		)
	)
)

(defun perform-effect/k (x eff)
	"Initiates effect handling."
	(perform/k-body x eff)
)

(defun perform-effect (x eff)
	"Initiates effect handling, no continuation created."
	(funcall (shift-context eff) x)
)

(defun throw-exn (exn)
	"Throw non-resumable exception"
	(funcall (shift-context +eff-exn+) exn)
)

(defun recover-exn (exn)
	"Recover from a resumable exception"
	(perform/k-body exn +eff-fix+)
)

(defmacro resume-cont-body (cont handler action)
	(let ((suspended (gensym "suspended")))
		; invalidate cont if still valid
		`(let ((,suspended (shiftf (cont-ctx ,cont) nil)))
			(if ,suspended
				(progn
					(pop (ctx-guards ,suspended)) ; remove invalidat-cont
					(context-handler ,handler)
					(resume-context ,suspended)
					,action
				)
				(throw-exn (make-bad-cont ,cont))
			)
		)
	)
)

; deep handler
(defun resume-cont (x cont)
	"Resumes a continuation if it is valid."
	(resume-cont-body cont (cont-handler cont) (normal-pass x))
)

; shallow handler support
(defun resume-cont/h (x cont handler)
	"Like `resume-cont`, but also sets a new handler."
	(resume-cont-body cont handler (normal-pass x))
)

; bidirectional effect handling support,
(defun resume-cont/call (f cont)
	"Calls `f` at the point `cont` resumes at."
	(resume-cont-body cont (cont-handler cont) (funcall f))
)

; bidirectional + shallow effect handling support
(defun resume-cont/call+h (f cont handler)
	"Like `resume-cont/call`, but also sets a new handler."
	(resume-cont-body cont handler (funcall f))
)
