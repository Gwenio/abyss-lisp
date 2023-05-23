
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
		:+inert+ :+ignore+ :+true+ :+false+ :ignore-p :make-app :+eff-exn+
		:boole-type
	)
	(:import-from :abyss/error
		:make-arg-pair :make-arg-null :make-arg-repeat :make-bad-param
		:make-type-exn
	)
	(:import-from :abyss/environment
		:make-environment :environment-p :env-table :base-env-child
		:extend-base-env
	)
	(:import-from :abyss/context
		:normal-pass :push-frame
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/continuation
		:perform-effect
	)
	(:import-from :abyss/helpers
		:bad-tail :bind-params :boole-branch
	)
	(:export :seq-impl :define-impl :vau-impl :lambda-impl :if-impl :cond-impl
		:let-impl :let*-impl :letrec-impl :letrec*-impl
	)
)
(in-package :abyss/operatives)

(defun seq-aux (env tail)
	(labels (
		(aux (_)
			(declare (ignore _))
			(let ((head (pop tail)))
				(cond
					((consp tail) (push-frame #'aux))
					((null tail) nil)
					(t (push-frame #'bad-tail))
				)
				(evaluate head env)
			)
		))
		#'aux
	)
)

(defun seq-impl (args)
	(let ((body (cdr args)))
		(cond
			((null body) (normal-pass +inert+))
			((consp body)
				(funcall (seq-aux (car args) body) nil)
			)
			(t (bad-tail body))
		)
	)
)

(defun define-aux (table binding)
	(let ((pending nil) (used (make-hash-table :test 'eq)))
		(labels (
			(impl (x)
				(macrolet (
				(advance ()
					'(if pending
						(let ((next (pop pending)))
							(setf binding (car next))
							(impl (cdr next))
						)
						(normal-pass +inert+)
					)
				))
				(cond
					((null binding)
						(if (null x)
							(advance)
							(perform-effect (make-arg-null x) +eff-exn+)
						)
					)
					((consp binding)
						(if (consp x)
							(progn
								(push (cons (cdr binding) (cdr x)) pending)
								(setf binding (car binding))
								(impl (car x))
							)
							(perform-effect (make-arg-pair x) +eff-exn+)
						)
					)
					((keywordp binding)
						(if (gethash binding used)
							(perform-effect (make-arg-repeat x) +eff-exn+)
							(progn
								(setf (gethash binding table) x)
								(setf (gethash binding used) t)
								(advance)
							)
						)
					)
					((ignore-p binding)
						(advance)
					)
					(t (perform-effect (make-bad-param binding) +eff-exn+))
				))
			))
			#'impl
		)
	)
)

(defun define-impl (args)
	(bind-params args (env bindings x)
		(push-frame (define-aux (env-table env) bindings))
		(evaluate x env)
	)
)

(defun make-closure (static-env formal-params body)
	#'(lambda (args)
		(let ((local-env (make-environment static-env)))
			(push-frame (seq-aux local-env body))
			(funcall
				(define-aux (env-table local-env) formal-params)
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
			(cond
				((consp body)
					(normal-pass
						(make-closure
							(make-environment env)
							(cons eformal bindings)
							body))
				)
				((null body)
					(normal-pass #'dummy-closure)
				)
				(t (bad-tail body))
			)
			(perform-effect (make-bad-param eformal) +eff-exn+)
		)
	)
)

(defun lambda-impl (args)
	(bind-params args (env bindings . body)
		(cond
			((consp body)
				(normal-pass (make-app
					(make-closure
						(make-environment env)
						(cons +ignore+ bindings)
						body)))
			)
			((null body)
				(normal-pass (make-app #'dummy-closure))
			)
			(t (bad-tail body))
		)
	)
)

(defun if-aux (env then else)
	(lambda (result)
		(boole-branch result
			((evaluate then env))
			((evaluate else env))
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
	#'(lambda (result)
		(boole-branch result
			((seq-impl (cons env then)))
			((cond-impl (cons env else)))
		)
	)
)

(defun cond-impl (args)
	(let ((env (car args)) (clauses (cdr args)))
		(cond
			((consp clauses)
				(let ((head (car clauses)))
					(if (consp head)
						(progn
							(push-frame
								(cond-aux env (cdr head) (cdr clauses)))
							(evaluate (car head) env)
						)
						(perform-effect (make-arg-pair head) +eff-exn+)
					)
				)
			)
			((null clauses) (normal-pass +inert+))
			(t (bad-tail clauses))
		)
	)
)

(defun let-aux (table bindings)
	(make-app #'(lambda (args)
		(funcall (define-aux table bindings) (cdr args))
	))
)

(defmacro let-body (args rec)
	`(bind-params ,args (parent-env bindings . body)
		(let ((env (make-environment parent-env)))
			(cond
				((consp body) (push-frame (seq-aux env body)))
				((null body) nil)
				(t (push-frame #'bad-tail))
			)
			(handler-case
				(loop for x in bindings
					unless (consp x)
					do (error 'type-error :datum x :expected-type 'cons)
					else unless (null (cddr x))
					do (error 'type-error :datum (cddr x) :expected-type 'null)
					end
					collect (car x) into y
					collect (second x) into z
					finally (return (cons (let-aux (env-table env) y) z))
				)
				(type-error (e)
					(raise-exn
						(if (eq (type-error-expected-type e) 'cons)
							:arg-pair
							:arg-null
						)
						(type-error-datum e))
				)
				(:no-error (v) (evaluate v ,(if rec 'env 'parent-env)))
			)
		)
	)
)

(defmacro let*-body (args basic iterative)
	`(bind-params ,args (parent-env bindings . body)
		(cond
			((consp bindings)
				(,basic (list
					parent-env
					(list (car bindings))
					(list* (function ,iterative) (cdr bindings) body)
				))
			)
			((null bindings)
				(seq-impl (cons (make-environment parent-env) body))
			)
			(t (bad-tail bindings))
		)
	)
)

(defun let-impl (args) (let-body args nil))
(defun let*-impl (args) (let*-body args let-impl let*-impl))
(defun letrec-impl (args) (let-body args t))
(defun letrec*-impl (args) (let*-body args letrec-impl letrec*-impl))
