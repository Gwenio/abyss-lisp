
(uiop:define-package :abyss/test/boole
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/types
		:+ignore+ :+true+ :+false+ :applicative-p
		:make-app :inert-p :+eff-exn+
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
	(:import-from :abyss/boole
		:not-impl :and-app-impl :or-app-impl :and-oper-impl :or-oper-impl
		:eq-impl
	)
)
(in-package :abyss/test/boole)

(def-suite* abyss-boole-tests :in abyss/test:abyss-tests)

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
	#'(lambda (x)
		(cond
			((eq eff +eff-exn+)
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
					(t (print (type-of x)) (error "Unexpected exception"))
				))
			)
			(t (error "Unexpected effect"))
		)
	)
)

(defmacro run-boole-case (x env)
	`(initial-context (lambda () (evaluate ,x ,env)) #'root-handler)
)

(test boole-not
	(let ((env (make-environment nil)))
		(is (eq +false+
			(run-boole-case
				(list (make-app #'not-impl) +true+)
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list (make-app #'not-impl) +false+)
				env))
		)
	)
)

(test boole-and-app
	(let ((env (make-environment nil)))
		(is (eq +true+
			(run-boole-case
				(list (make-app #'and-app-impl))
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list (make-app #'and-app-impl) +true+)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list (make-app #'and-app-impl) +false+)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list (make-app #'and-app-impl) +true+ +false+)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list (make-app #'and-app-impl) +false+ +false+)
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list (make-app #'and-app-impl) +true+ +true+)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list (make-app #'and-app-impl) +false+ +true+)
				env))
		)
	)
)

(test boole-or-app
	(let ((env (make-environment nil)))
		(is (eq +false+
			(run-boole-case
				(list (make-app #'or-app-impl))
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list (make-app #'or-app-impl) +true+)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list (make-app #'or-app-impl) +false+)
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list (make-app #'or-app-impl) +true+ +false+)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list (make-app #'or-app-impl) +false+ +false+)
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list (make-app #'or-app-impl) +true+ +true+)
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list (make-app #'or-app-impl) +false+ +true+)
				env))
		)
	)
)

(test boole-and-oper
	(let ((env (make-environment nil)))
		(is (eq +true+
			(run-boole-case
				(list #'and-oper-impl)
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list #'and-oper-impl +true+)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list #'and-oper-impl +false+)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list #'and-oper-impl +true+ +false+)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list #'and-oper-impl +false+ +false+)
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list #'and-oper-impl +true+ +true+)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list #'and-oper-impl +false+ +true+)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list #'and-oper-impl
					(list (make-app #'not-impl) +true+))
				env))
		)
		(is (eql 1
			(run-boole-case
				(list #'and-oper-impl 1)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list #'and-oper-impl +false+ 1)
				env))
		)
		(is (eql 1
			(run-boole-case
				(list #'and-oper-impl +true+ 1)
				env))
		)
	)
)

(test boole-or-oper
	(let ((env (make-environment nil)))
		(is (eq +false+
			(run-boole-case
				(list #'or-oper-impl)
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list #'or-oper-impl +true+)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list #'or-oper-impl +false+)
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list #'or-oper-impl +true+ +false+)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list #'or-oper-impl +false+ +false+)
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list #'or-oper-impl +true+ +true+)
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list #'or-oper-impl +false+ +true+)
				env))
		)
		(is (eq +false+
			(run-boole-case
				(list #'or-oper-impl
					(list (make-app #'not-impl) +true+))
				env))
		)
		(is (eql 1
			(run-boole-case
				(list #'or-oper-impl 1)
				env))
		)
		(is (eql 1
			(run-boole-case
				(list #'or-oper-impl +false+ 1)
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list #'or-oper-impl +true+ 1)
				env))
		)
	)
)

(test boole-eq
	(let ((env (make-environment nil)))
		(is (eq +false+
			(run-boole-case
				(list (make-app #'eq-impl) +false+ +true+)
				env))
		)
		(is (eq +true+
			(run-boole-case
				(list (make-app #'eq-impl) +true+ +true+)
				env))
		)
	)
)
