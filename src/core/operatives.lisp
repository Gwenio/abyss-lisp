
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
	(:import-from :abyss/types
		:+inert+ :+ignore+ :+true+ :+false+ :ignore-p :make-app
	)
	(:import-from :abyss/error
		:make-arg-pair :make-arg-null :make-arg-repeat :make-bad-param
		:make-type-exn :make-improper-list
	)
	(:import-from :abyss/environment
		:make-environment :environment-p :env-table
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
		:let-impl :let*-impl :letrec-impl :letrec*-impl
	)
)
(in-package :abyss/operatives)

(declaim (ftype (function (abyss/environment::environment cons) function)
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

(declaim (ftype (function ((cons abyss/environment::environment t)) t)
	seq-impl define-impl vau-impl lambda-impl if-impl cond-impl
	let-impl let*-impl letrec-impl letrec*-impl))

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

(declaim (ftype (function (abyss/environment::environment t cons) function)
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
		(if (or (keywordp eformal) (ignore-p eformal))
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
			(throw-exn (make-bad-param eformal))
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

(declaim (ftype (function (abyss/environment::environment t t) t)
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
						(throw-exn (make-arg-pair head))
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

(defmacro let-body (args rec)
	`(bind-params ,args (parent-env bindings . body)
		(let ((env (make-environment parent-env)))
			(typecase body
				(cons (push-frame (seq-aux env body)))
				(null nil)
				(t (push-frame (bad-tail body)))
			)
			(handler-case
				(loop for x in bindings
					unless (consp x)
					do (error 'type-error :datum x :expected-type 'cons)
					else unless (null (cddr x))
					do (error 'type-error :datum (cddr x) :expected-type 'null)
					end
					collect (first x) into y
					collect (second x) into z
					finally (return (cons (let-aux (env-table env) y) z))
				)
				(type-error (e)
					(throw-exn
						(if (eq (type-error-expected-type e) 'cons)
							(make-arg-pair (type-error-datum e))
							(make-arg-null (type-error-datum e))
						))
				)
				(:no-error (v) (evaluate v ,(if rec 'env 'parent-env)))
			)
		)
	)
)

(defmacro let*-body (args basic iterative)
	`(bind-params ,args (parent-env bindings . body)
		(typecase bindings
			(cons
				(,basic (list
					parent-env
					(list (first bindings))
					(list* (function ,iterative) (cdr bindings) body)
				))
			)
			(null
				(seq-impl (cons (make-environment parent-env) body))
			)
			(t (throw-exn (make-type-exn bindings 'list)))
		)
	)
)

(defun let-impl (args) (let-body args nil))
(defun let*-impl (args) (let*-body args let-impl let*-impl))
(defun letrec-impl (args) (let-body args t))
(defun letrec*-impl (args) (let*-body args letrec-impl letrec*-impl))
