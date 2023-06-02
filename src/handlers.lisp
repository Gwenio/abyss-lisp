
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

(uiop:define-package :abyss/handlers
	(:use :cl)
	(:import-from :abyss/types
		:+inert+ :+ignore+ :+true+ :+false+ :inert-p :ignore-p :make-app
		:applicative-p :applicative :app-comb :effect-p :make-effect
		:+eff-exn+ :+eff-ret+ :+eff-init+
	)
	(:import-from :abyss/error
		:make-arg-pair :make-arg-null :make-arg-repeat :make-bad-param
		:make-type-exn :make-invalid-comb :make-bad-handler-case
	)
	(:import-from :abyss/environment
		:make-environment :environment-p :env-table
	)
	(:import-from :abyss/context
		:fresh-context
		:normal-pass :push-frame :perform-effect :continuation-p
		:resume-cont :resume-cont/h :resume-cont/call :resume-cont/call+h
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/helpers
		:bind-params :type-pred-body
	)
	(:import-from :abyss/operatives
		:seq-impl
	)
	(:export :eff-p-impl :cont-p-impl :handler-p-impl
		:make-eff-impl :throw-impl
		:resume-impl :resume/h-impl :resume/call-impl :resume/call+h-impl
		:handler-impl :with-impl
	)
)
(in-package :abyss/handlers)

(defun eff-p-impl (args)
	(type-pred-body args x (effect-p x))
)

(defun cont-p-impl (args)
	(type-pred-body args x (continuation-p x))
)

(defstruct
	(handler-type
		(:constructor make-handler (lookup init))
		(:conc-name handler-)
		(:predicate handler-p)
	)
	(lookup (lambda (_) (declare (ignore _)) nil)
		:type function :read-only t)
	(init nil :read-only t)
)

(defun handler-p-impl (args)
	(type-pred-body args x (handler-p x))
)

(defun make-eff-aux (eff)
	(lambda (args)
		(bind-params args (nil x)
			(perform-effect x eff)
		)
	)
)

(defun make-eff-impl (args)
	(bind-params args (nil name)
		(let ((eff (make-effect name)))
			(normal-pass (list eff
				(make-app (make-eff-aux eff))))
		)
	)
)

(defun throw-impl (args)
	(bind-params args (nil x)
		(perform-effect x +eff-exn+)
	)
)

(defun resume-impl (args)
	(bind-params args (nil cont x)
		(if (continuation-p cont)
			(resume-cont x cont)
			(perform-effect (make-type-exn cont 'continuation) +eff-exn+)
		)
	)
)

(defun resume/h-impl (args)
	(bind-params args (nil cont handler x)
		(if (continuation-p cont)
			(resume-cont/h x cont handler)
			(perform-effect (make-type-exn cont 'continuation) +eff-exn+)
		)
	)
)

(defmacro wrap-seq-aux (env body)
	`(lambda ()
		(seq-impl (cons ,env ,body))
	)
)

(defun resume/call-impl (args)
	(bind-params args (env cont . body)
		(if (continuation-p cont)
			(resume-cont/call (wrap-seq-aux env body) cont)
			(perform-effect (make-type-exn cont 'continuation) +eff-exn+)
		)
	)
)

(defun resume/call+h-impl (args)
	(bind-params args (env cont handler . body)
		(if (continuation-p cont)
			(resume-cont/call+h (wrap-seq-aux env body) cont handler)
			(perform-effect (make-type-exn cont 'continuation) +eff-exn+)
		)
	)
)

(defun handler-lookup-aux (table)
	(lambda (eff) (gethash eff table))
)

(defun init-handler-ret-aux (x)
	(make-app (lambda (_args)
		(declare (ignore _args))
		(normal-pass x)
	))
)

(defun init-handler-aux (args)
	(lambda (app)
		(funcall (app-comb app) (cons nil args))
	)
)

(defun init-handler (args)
	(push-frame (init-handler-aux (car args)))
	(resume-cont nil (cdr args))
)

(defun handler-aux (cont init params bodies wrapper)
	(labels ((aux (args)
			(loop for eff in (cdr args)
				for binds in params
				for body in bodies
				with env = (car args)
				with table = (make-hash-table :test 'eq)
				do (if (effect-p eff)
					(when (shiftf (gethash eff table)
							(funcall wrapper env body
								(if (eq eff +eff-ret+)
									binds
									(cons binds cont)
								))
						)
						(return (perform-effect
							(make-arg-repeat eff) +eff-exn+))
					)
					(return (perform-effect
						(make-type-exn eff 'effect) +eff-exn+))
				)
				finally (return (normal-pass (if init
					(progn
						(unless (gethash +eff-ret+ table)
							(setf (gethash +eff-ret+ table)
								#'init-handler-ret-aux)
						)
						(setf (gethash +eff-init+ table) #'init-handler)
						(make-handler (handler-lookup-aux table) t)
					)
					(progn
						(unless (gethash +eff-ret+ table)
							(setf (gethash +eff-ret+ table) #'normal-pass)
						)
						(make-handler (handler-lookup-aux table) nil)
					))
				))
			)
		))
		#'aux
	)
)

(defmacro handler-impl-case (env cases params bodies aux)
	(let ((x (gensym "x")) (y (gensym "y")))
		`(loop for ,x on ,cases
			while (consp ,x)
			unless (and (consp (car ,x)) (consp (cdar ,x)) (consp (cddar ,x)))
			do (return
				(perform-effect (make-bad-handler-case (car ,x)) +eff-exn+))
			else
			collect (caar ,x) into ,y
			and collect (cadar ,x) into ,params
			and collect (cddar ,x) into ,bodies
			end
			finally (if ,x
				(return (perform-effect (make-type-exn ,x 'list) +eff-exn+))
				(return (evaluate (cons (make-app ,aux) ,y) ,env))
			)
		)
	)
)

(defun handler-wrapper (env body params)
	(lambda (args)
		(let ((child (make-environment env)))
			(push-frame (abyss/operatives::seq-aux child body))
			(funcall
				(abyss/operatives::define-aux (env-table child) params)
				args)
		)
	)
)

(defun handler-wrapper/init (init)
	(lambda (env body params)
		(lambda (args)
			(let ((child (make-environment env)))
				(push-frame (lambda (_)
					(declare (ignore _))
					(make-app (lambda (x)
						(abyss/operatives::seq-aux child body)
						(funcall
							(abyss/operatives::define-aux
								(env-table child)
								init)
							(cdr x))
					))
				))
				(funcall
					(abyss/operatives::define-aux (env-table child) params)
					args)
			)
		)
	)
)

(defun handler-impl (args)
	(bind-params args (env cont init . cases)
		(if (consp cases) ; must be at least one effect handled
			(if (listp init)
				(if (or (keywordp cont) (ignore-p cont))
					(handler-impl-case env cases params bodies
						(handler-aux cont init params bodies
							(if init
								(handler-wrapper/init init)
								#'handler-wrapper
							)))
					(perform-effect (make-bad-param cont) +eff-exn+)
				)
				(perform-effect (make-bad-param init) +eff-exn+)
			)
			(perform-effect (make-type-exn cont 'cons) +eff-exn+)
		)
	)
)

(defvar +with-init-app+ (make-app
	(lambda (args)
		(perform-effect (cdr args) +eff-init+)
	)))

(defun with-aux (env body)
	(lambda (handler)
		(if (handler-p handler)
			(if (handler-init handler)
				(if (consp body)
					(let ((init (cons +with-init-app+ (pop body))))
						(fresh-context
							(wrap-seq-aux env (cons init body))
							(handler-lookup handler))
					)
					(perform-effect (make-type-exn body 'cons) +eff-exn+)
				)
				(fresh-context
					(wrap-seq-aux env body)
					(handler-lookup handler))
			)
			(perform-effect (make-type-exn handler 'handler) +eff-exn+)
		)
	)
)

(defun with-impl (args)
	(bind-params args (env handler . body)
		(push-frame (with-aux env body))
		(evaluate handler env)
	)
)
