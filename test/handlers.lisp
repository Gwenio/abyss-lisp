
(uiop:define-package :abyss/test/handlers
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/types
		:+ignore+ :+true+ :+false+ :applicative-p
		:make-app :inert-p :+eff-exn+ :+eff-fix+ :+eff-ret+
	)
	(:import-from :abyss/error
		:sym-not-found-exn :make-sym-not-found
		:invalid-comb-exn
		:improper-list-exn
		:bad-param-exn
		:arg-pair-exn
		:arg-null-exn
		:arg-repeat-exn
		:bad-cont-exn
		:type-exn
		:bad-handler-case-exn
	)
	(:import-from :abyss/environment
		:make-environment :env-table
	)
	(:import-from :abyss/context
		:initial-context :normal-pass :perform-effect :resume-cont/call
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/operatives
		:seq-impl :define-impl :vau-impl :lambda-impl :if-impl :cond-impl
		:let-impl :let*-impl :letrec-impl :letrec*-impl
	)
	(:import-from :abyss/handlers
		:make-eff-impl :with-impl :handler-impl :throw-impl
	)
)
(in-package :abyss/test/handlers)

(def-suite* abyss-handlers-tests :in abyss/test:abyss-tests)

(defun root-handler (eff)
	(cond
		((eq eff +eff-ret+) #'identity)
		((eq eff +eff-exn+)
			(lambda (x)
				(typecase x
					(sym-not-found-exn x)
					(invalid-comb-exn x)
					(improper-list-exn x)
					(bad-param-exn x)
					(arg-pair-exn x)
					(arg-null-exn x)
					(arg-repeat-exn x)
					(bad-cont-exn x)
					(type-exn x)
					(bad-handler-case-exn x)
					(t
						(print (type-of x))
						(error "Unexpected exception")
					)
				)
			)
		)
		((eq eff +eff-fix+)
			(lambda (x)
				(let ((x (car x))) (typecase x
					(sym-not-found-exn x)
					(invalid-comb-exn x)
					(improper-list-exn x)
					(bad-param-exn x)
					(arg-pair-exn x)
					(arg-null-exn x)
					(arg-repeat-exn x)
					(bad-cont-exn x)
					(type-exn x)
					(bad-handler-case-exn x)
					(t
						(print (type-of x))
						(error "Unexpected recoverable exception")
					)
				))
			)
		)
		(t (error "Unexpected effect"))
	)
)

(defmacro run-h-case (x env)
	`(initial-context (lambda () (evaluate ,x ,env)) #'root-handler)
)

(test handlers-basic
	(let ((env (make-environment nil)))
		(is (eq t
			(run-h-case
				(list #'seq-impl
					(list #'define-impl '(:eff :perform)
						(list #'make-eff-impl "dummy"))
					(list #'with-impl
						(list #'handler-impl +ignore+ '(:eff :x :x))
						(list :perform t)))
				env))
		)
	)
)

(test handlers-throw
	(let ((env (make-environment nil)))
		(is (eq t
			(run-h-case
				(list #'with-impl
					(list #'handler-impl +ignore+ (list +eff-exn+ :x :x))
					(list (make-app #'throw-impl) t)
				)
				env))
		)
	)
)
