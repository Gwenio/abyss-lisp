
(uiop:define-package :abyss/test/applicatives
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
	(:import-from :abyss/applicatives
		:apply-impl :current-env-impl
	)
)
(in-package :abyss/test/applicatives)

(def-suite* abyss-applicatives-tests :in abyss/test:abyss-tests)

(define-condition sym-not-found (error) (datum))
(define-condition invalid-combiner (error) (datum))
(define-condition improper-list (error) (datum))
(define-condition bad-param-spec (error) (datum))
(define-condition arg-expect-pair (error) (datum))
(define-condition arg-expect-null (error) (datum))
(define-condition arg-repeated (error) (datum))
(define-condition bad-continuation (error) (datum))
(define-condition arg-expect-type (error) (datum))

(defun root-handler (eff)
	(cond
		((eq eff +eff-ret+) #'identity)
		((eq eff +eff-exn+)
			(lambda (x)
				(typecase x
					(sym-not-found-exn
						(error 'sym-not-found :datum x))
					(invalid-comb-exn
						(error 'invalid-combiner :datum x))
					(improper-list-exn
						(error 'improper-list :datum x))
					(bad-param-exn
						(error 'bad-param-spec :datum x))
					(arg-pair-exn
						(error 'arg-expect-pair :datum x))
					(arg-null-exn
						(error 'arg-expect-null :datum x))
					(arg-repeat-exn
						(error 'arg-repeated :datum x))
					(bad-cont-exn
						(error 'bad-continuation :datum x))
					(type-exn
						(error 'arg-expect-type :datum x))
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
					(sym-not-found-exn
						(error 'sym-not-found :datum x))
					(invalid-comb-exn
						(error 'invalid-combiner :datum x))
					(improper-list-exn
						(error 'improper-list :datum x))
					(bad-param-exn
						(error 'bad-param-spec :datum x))
					(arg-pair-exn
						(error 'arg-expect-pair :datum x))
					(arg-null-exn
						(error 'arg-expect-null :datum x))
					(arg-repeat-exn
						(error 'arg-repeated :datum x))
					(bad-cont-exn
						(error 'bad-continuation :datum x))
					(type-exn
						(error 'arg-expect-type :datum x))
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

(defmacro run-app-case (x env)
	`(initial-context (lambda () (evaluate ,x ,env)) #'root-handler)
)

(test app-apply
	(let ((env (make-environment nil)))
		(is (eq env
			(run-app-case
				(list (make-app #'apply-impl)
					(make-app #'current-env-impl)
					nil env)
				env))
		)
		(is (not (eq env
			(run-app-case
				(list (make-app #'apply-impl)
					(make-app #'current-env-impl)
					nil)
				env)))
		)
		(signals arg-expect-pair
			(run-app-case
				(list* (make-app #'apply-impl)
					(make-app #'current-env-impl)
					nil env)
				env)
		)
		(signals arg-expect-type
			(run-app-case
				(list (make-app #'apply-impl)
					#'current-env-impl
					nil)
				env)
		)
		(signals arg-expect-type
			(run-app-case
				(list (make-app #'apply-impl)
					(make-app #'current-env-impl)
					nil t)
				env)
		)
	)
)




