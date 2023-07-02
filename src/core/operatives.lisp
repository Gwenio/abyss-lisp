
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

(uiop:define-package :abyss/operatives
	(:use :cl)
	(:mix :abyss/types)
	(:import-from :abyss/error
		:make-match-cons :make-match-null :make-match-repeat :make-match-param
		:make-type-exn :make-improper-list
	)
	(:import-from :abyss/context
		:normal-pass :push-frame :throw-exn :recover-exn
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/helpers
		:bad-tail :bind-params :boole-branch :destructure
	)
	(:export :seq-impl :define-impl :vau-impl :lambda-impl :if-impl :cond-impl
		:let-impl :let*-impl :pcase-impl
	)
)
(in-package :abyss/operatives)

(declaim (ftype (function (abyss/types::environment cons) function)
	seq-aux))

(defun seq-aux (env next)
	(lambda (_)
		(declare (ignore _))
		(let ((head (first next)) (tail (cdr next)))
			(typecase tail
				(cons (push-frame (seq-aux env tail)))
				(null nil)
				(t (push-frame (bad-tail tail)))
			)
			(evaluate head env)
		)
	)
)

(declaim (ftype (function ((cons abyss/types::environment t)) t)
	seq-impl define-impl vau-impl lambda-impl if-impl cond-impl
	let-impl let*-impl pcase-impl))

(defun seq-impl (args)
	(let ((body (cdr args)))
		(typecase body
			(cons
				(funcall (seq-aux (first args) body) nil)
			)
			(null (normal-pass +inert+))
			(t (recover-exn (make-improper-list body +inert+)))
		)
	)
)

(defun define-impl (args)
	(bind-params args (env bindings x)
		(push-frame (destructure (env-table env) bindings))
		(evaluate x env)
	)
)

(declaim (ftype (function (abyss/types::environment t cons) function)
	make-closure))

(defun make-closure (static-env formal-params body)
	(lambda (args)
		(let ((local-env (make-environment static-env)))
			(push-frame (seq-aux local-env body))
			(funcall
				(destructure (env-table local-env) formal-params)
				args)
		)
	)
)

(defun dummy-closure (_args)
	(declare (ignore _args))
	(normal-pass +inert+)
)

(defun vau-impl (args)
	(bind-params args (env bindings eformal . body)
		(if (or (glyph-p eformal) (ignore-p eformal))
			(typecase body
				(cons
					(normal-pass
						(make-closure
							env
							(cons eformal bindings)
							body))
				)
				(null
					(normal-pass #'dummy-closure)
				)
				(t (throw-exn (make-type-exn body 'list)))
			)
			(throw-exn (make-match-param eformal))
		)
	)
)

(defun lambda-impl (args)
	(bind-params args (env bindings . body)
		(typecase body
			(cons
				(normal-pass (make-app
					(make-closure
						env
						(cons +ignore+ bindings)
						body)))
			)
			(null
				(normal-pass (make-app #'dummy-closure))
			)
			(t (throw-exn (make-type-exn body 'list)))
		)
	)
)

(declaim (ftype (function (abyss/types::environment t t) (function (t) t))
	if-aux cond-aux))

(defun if-aux (env then else)
	(lambda (result)
		(boole-branch result
			(evaluate then env)
			(evaluate else env)
		)
	)
)

(defun if-impl (args)
	(bind-params args (env test then else)
		(push-frame (if-aux env then else))
		(evaluate test env)
	)
)

(defun cond-aux (env then else)
	(lambda (result)
		(boole-branch result
			(seq-impl (cons env then))
			(cond-impl (cons env else))
		)
	)
)

(defun cond-impl (args)
	(let ((env (first args)) (clauses (cdr args)))
		(typecase clauses
			(cons
				(let ((head (first clauses)))
					(if (consp head)
						(progn
							(push-frame
								(cond-aux env (cdr head) (cdr clauses)))
							(evaluate (first head) env)
						)
						(throw-exn (make-match-cons head))
					)
				)
			)
			(null (normal-pass +inert+))
			(t (recover-exn (make-improper-list clauses +inert+)))
		)
	)
)

(declaim (ftype (function (hash-table t) abyss/types::applicative) let-aux))

(defun let-aux (table bindings)
	(make-app (lambda (args)
		(funcall (destructure table bindings) (cdr args))
	))
)

(defun let-loop (bindings)
	(loop for x on bindings
		while (consp x)
		if (glyph-p (first x))
		collect (first x) into y
		and collect (first x) into z
		else unless (consp (first x))
		do (return (values nil nil (make-match-cons (first x))))
		else unless (consp (cdr (first x)))
		do (return (values nil nil (make-match-cons (cdr (first x)))))
		else unless (null (cddr (first x)))
		do (return (values nil nil (make-match-null (cddr (first x)))))
		else
		collect (first (first x)) into y
		and collect (second (first x)) into z
		end
		finally (return (if x
			(values nil nil (make-match-null x))
			(values z y nil)
		))
	)
)

(defun let-impl (args)
	(bind-params args (parent-env bindings . body)
		(let ((env (make-environment parent-env)))
			(typecase body
				(cons (push-frame (seq-aux env body)))
				(null nil)
				(t (push-frame (bad-tail body)))
			)
			(multiple-value-bind (z y x) (let-loop bindings)
				(if x
					(throw-exn x)
					(evaluate (cons (let-aux (env-table env) y) z) parent-env)
				)
			)
		)
	)
)

(defun let*-impl (args)
	(bind-params args (parent-env bindings . body)
		(typecase bindings
			(cons
				(let-impl (list
					parent-env
					(list (first bindings))
					(list* #'let*-impl (cdr bindings) body)
				))
			)
			(null
				(seq-impl (cons (make-environment parent-env) body))
			)
			(t (throw-exn (make-type-exn bindings 'list)))
		)
	)
)

(declaim (ftype (function (abyss/types::environment t t t) (function (t) t))
	pcase-select pcase-apply))

(declaim (ftype (function (abyss/types::environment t t) t)
	pcase-aux))

(defun pcase-select (env then x clauses)
	(lambda (result)
		(boole-branch result
			(seq-impl (cons env then))
			(pcase-aux env x clauses)
		)
	)
)

(defun pcase-apply (env then x clauses)
	(lambda (pred)
		(typecase pred
			(abyss/types::type-id
				(if (funcall (tid-pred pred) x pred)
					(seq-impl (cons env then))
					(pcase-aux env x clauses)
				)
			)
			(abyss/types::applicative
				(push-frame (pcase-select env then x clauses))
				(evaluate (list (app-comb pred) x) env)
			)
			(abyss/types::boole-type
				(if (eq +true+ pred)
					(seq-impl (cons env then))
					(pcase-aux env x clauses)
				)
			)
			(t
				(throw-exn (make-type-exn pred
					(list +tid-type-id+ +tid-applicative+ +tid-boole+)))
			)
		)
	)
)

(defun pcase-aux (env x clauses)
	(typecase clauses
		(cons (let ((head (first clauses)))
			(if (consp head)
				(progn
					(push-frame (pcase-apply env (cdr head) x (cdr clauses)))
					(evaluate (first head) env)
				)
				(throw-exn (make-match-cons head))
			)
		))
		(null (normal-pass +inert+))
		(t (recover-exn (make-improper-list clauses +inert+)))
	)
)

(defun pcase-impl (args)
	(bind-params args (env expr . clauses)
		(push-frame (lambda (x) (pcase-aux env x clauses)))
		(evaluate expr env)
	)
)
