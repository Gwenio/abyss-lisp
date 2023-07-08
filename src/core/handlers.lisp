
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
	(:mix
		:abyss/types
		:abyss/error
		:abyss/context
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/helpers
		:bind-params :type-pred-body :destructure
	)
	(:import-from :abyss/operatives
		:seq-impl
	)
	(:export :eff-p-impl :cont-p-impl :handler-p-impl
		:make-eff-impl :make-eff/k-impl :throw-impl :recover-impl
		:set-handler-impl :resume-impl :resume/do-impl
		:handler-impl :handler/s-impl :with-impl
		:+tid-continuation+ :+tid-handler+
		:always-impl
		:guard-impl
	)
)
(in-package :abyss/handlers)

(declaim (ftype (function ((cons abyss/types::environment t)) t)
	resume/call-impl resume/call+h-impl handler-impl handler/s-impl with-impl))

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

(declaim (type abyss/types::type-id +tid-handler+ +tid-continuation+))

(def-simple-type-id +tid-continuation+ "continuation" continuation-p)
(def-simple-type-id +tid-handler+ "handler" handler-p)

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

(defun set-handler-impl (args)
	(bind-params args (nil cont h)
		(if (continuation-p cont)
			(if (handler-p h)
				(progn
					(set-handler cont (handler-lookup h))
					(normal-pass +inert+)
				)
				(throw-exn (make-type-exn h +tid-handler+))
			)
			(throw-exn (make-type-exn cont +tid-continuation+))
		)
	)
)

(defun resume-impl (args)
	(bind-params args (nil cont x)
		(if (continuation-p cont)
			(resume-cont x cont)
			(throw-exn (make-type-exn cont +tid-continuation+))
		)
	)
)

(defmacro wrap-seq-aux (env body)
	`(lambda ()
		(seq-impl (cons ,env ,body))
	)
)

(defun resume/do-aux (env body cont)
	(if (continuation-p cont)
		(resume-cont/call
			(wrap-seq-aux env body)
			cont)
		(throw-exn (make-type-exn cont +tid-continuation+))
	)
)

(defun resume/do-impl (args)
	(bind-params args (env expr . body)
		(push-frame (lambda (cont)
			(resume/do-aux env body cont)
		))
		(evaluate expr env)
	)
)

(defmacro guard-body (args push-guard)
	`(bind-params ,args (env guard . body)
		(,push-guard (lambda (x)
			(push-frame (lambda (_)
				(declare (ignore _))
				(normal-pass x)
			))
			(seq-impl (cons env guard)
		)))
		(seq-impl (cons env body))
	)
)

(defun always-impl (args)
	(guard-body args final-guard)
)

(defun guard-impl (args)
	(guard-body args error-guard)
)

(defun handler-lookup-aux (table)
	(lambda (eff) (gethash eff table))
)

(defun init-handler-ret-aux (x)
	(normal-pass (make-app (lambda (_args)
		(declare (ignore _args))
		(normal-pass x)
	)))
)

(defun init-handler-aux (args)
	(lambda (app)
		(let ((f (app-comb app)))
			(declare (type function f))
			(funcall f (cons nil args))
		)
	)
)

(defun init-handler (args)
	(push-frame (init-handler-aux (car args)))
	(resume-cont nil (cdr args))
)

(declaim (ftype (function (t t cons cons function) t) handler-aux))

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
						(return (throw-exn (make-match-repeat eff)))
					)
					(return (throw-exn (make-type-exn eff +tid-effect+)))
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
			do (return (values (make-bad-handler (car ,x)) nil nil nil))
			else
			collect (caar ,x) into ,y
			and collect (cadar ,x) into ,p
			and collect (cddar ,x) into ,b
			end
			finally (return (values
				(and ,x (make-type-exn ,x (list +tid-null+ +tid-cons+)))
				,y ,p ,b))
		)
	)
)

(defun handler-wrapper (env body params)
	(lambda (args)
		(let ((child (make-environment env)))
			(push-frame (abyss/operatives::seq-aux child body))
			(funcall
				(destructure (env-table child) params)
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
							(destructure (env-table child) init)
							(cdr x))
					)))
				))
				(funcall
					(destructure (env-table child) params)
					args)
			)
		)
	)
)

(defun handler-impl (args)
	(bind-params args (env cont . cases)
		(if (consp cases) ; must be at least one effect handled
			(if (or (glyph-p cont) (ignore-p cont))
				(multiple-value-bind (x y p b) (handler-impl-case cases)
					(if x
						(throw-exn x)
						(evaluate (cons (make-app
							(handler-aux cont nil p b
								#'handler-wrapper))
							y) env)
					)
				)
				(throw-exn (make-match-param cont))
			)
			(throw-exn (make-type-exn cont +tid-cons+))
		)
	)
)

(defun handler/s-impl (args)
	(bind-params args (env cont init . cases)
		(if (consp cases) ; must be at least one effect handled
			(if (or (glyph-p cont) (ignore-p cont))
				(multiple-value-bind (x y p b) (handler-impl-case cases)
					(if x
						(throw-exn x)
						(evaluate (cons (make-app
							(handler-aux cont init p b
								(handler-wrapper/init init)))
							y) env)
					)
				)
				(throw-exn (make-match-param cont))
			)
			(throw-exn (make-type-exn cont +tid-cons+))
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
					(throw-exn (make-type-exn body +tid-cons+))
				)
				(fresh-context
					(wrap-seq-aux env body)
					(handler-lookup handler))
			)
			(throw-exn (make-type-exn handler +tid-handler+))
		)
	)
)

(defun with-impl (args)
	(bind-params args (env handler . body)
		(push-frame (with-aux env body))
		(evaluate handler env)
	)
)
