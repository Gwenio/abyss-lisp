
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
		:applicative-p :applicative :app-comb
		:make-effect :effect-p :effect-name :effect-resumable
		:+eff-exn+ :+eff-fix+ :+eff-ret+ :+eff-init+
	)
	(:import-from :abyss/error
		:make-arg-pair :make-arg-null :make-arg-repeat :make-bad-param
		:make-type-exn :make-invalid-comb :make-bad-handler-case
	)
	(:import-from :abyss/environment
		:make-environment :environment-p :env-table
	)
	(:import-from :abyss/context
		:fresh-context :normal-pass :push-frame :throw-exn :recover-exn
		:perform-effect :perform-effect/k :continuation-p
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
		:make-eff-impl :make-eff/k-impl :throw-impl :recover-impl
		:resume-impl :resume/h-impl :resume/call-impl :resume/call+h-impl
		:handler-impl :handler/s-impl :with-impl
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
		(let ((eff (make-effect name nil)))
			(normal-pass (list eff
				(make-app (make-eff-aux eff))))
		)
	)
)

(defun make-eff/k-aux (eff)
	(lambda (args)
		(bind-params args (nil x)
			(perform-effect/k x eff)
		)
	)
)

(defun make-eff/k-impl (args)
	(bind-params args (nil name)
		(let ((eff (make-effect name t)))
			(normal-pass (list eff
				(make-app (make-eff/k-aux eff))))
		)
	)
)

(defun throw-impl (args)
	(bind-params args (nil x)
		(throw-exn x)
	)
)

(defun recover-impl (args)
	(bind-params args (nil x)
		(recover-exn x)
	)
)

(defun resume-impl (args)
	(bind-params args (nil cont x)
		(if (continuation-p cont)
			(resume-cont x cont)
			(throw-exn (make-type-exn cont 'continuation))
		)
	)
)

(defun resume/h-impl (args)
	(bind-params args (nil cont handler x)
		(if (continuation-p cont)
			(resume-cont/h x cont handler)
			(throw-exn (make-type-exn cont 'continuation))
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
			(throw-exn (make-type-exn cont 'continuation))
		)
	)
)

(defun resume/call+h-impl (args)
	(bind-params args (env cont handler . body)
		(if (continuation-p cont)
			(resume-cont/call+h (wrap-seq-aux env body) cont handler)
			(throw-exn (make-type-exn cont 'continuation))
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
								(if (effect-resumable eff)
									(cons binds cont)
									binds
								))
						)
						(return (throw-exn (make-arg-repeat eff)))
					)
					(return (throw-exn (make-type-exn eff 'effect)))
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

(defmacro handler-impl-case (cases)
	(let ((x (gensym "x")) (y (gensym "y")) (p (gensym "p")) (b (gensym "b")))
		`(loop for ,x on ,cases
			while (consp ,x)
			unless (and (consp (car ,x)) (consp (cdar ,x)) (consp (cddar ,x)))
			do (return (values (make-bad-handler-case (car ,x)) nil nil nil))
			else
			collect (caar ,x) into ,y
			and collect (cadar ,x) into ,p
			and collect (cddar ,x) into ,b
			end
			finally (return (values (and ,x (make-type-exn ,x 'list)) ,y ,p ,b))
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
					(normal-pass (make-app (lambda (x)
						(push-frame (abyss/operatives::seq-aux child body))
						(funcall
							(abyss/operatives::define-aux
								(env-table child)
								init)
							(cdr x))
					)))
				))
				(funcall
					(abyss/operatives::define-aux (env-table child) params)
					args)
			)
		)
	)
)

(defun handler-impl (args)
	(bind-params args (env cont . cases)
		(if (consp cases) ; must be at least one effect handled
			(if (or (keywordp cont) (ignore-p cont))
				(multiple-value-bind (x y p b) (handler-impl-case cases)
					(if x
						(throw-exn x)
						(evaluate (cons (make-app
							(handler-aux cont nil p b
								#'handler-wrapper))
							y) env)
					)
				)
				(throw-exn (make-bad-param cont))
			)
			(throw-exn (make-type-exn cont 'cons))
		)
	)
)

(defun handler/s-impl (args)
	(bind-params args (env cont init . cases)
		(if (consp cases) ; must be at least one effect handled
			(if (or (keywordp cont) (ignore-p cont))
				(multiple-value-bind (x y p b) (handler-impl-case cases)
					(if x
						(throw-exn x)
						(evaluate (cons (make-app
							(handler-aux cont init p b
								(handler-wrapper/init init)))
							y) env)
					)
				)
				(throw-exn (make-bad-param cont))
			)
			(throw-exn (make-type-exn cont 'cons))
		)
	)
)

(defvar +with-init-app+ (make-app
	(lambda (args)
		(perform-effect/k (cdr args) +eff-init+)
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
					(throw-exn (make-type-exn body 'cons))
				)
				(fresh-context
					(wrap-seq-aux env body)
					(handler-lookup handler))
			)
			(throw-exn (make-type-exn handler 'handler))
		)
	)
)

(defun with-impl (args)
	(bind-params args (env handler . body)
		(push-frame (with-aux env body))
		(evaluate handler env)
	)
)
