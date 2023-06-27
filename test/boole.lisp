
(uiop:define-package :abyss/test/boole
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/types
		:+true+ :+false+ :make-app :+eff-exn+ :+eff-fix+ :+eff-ret+
		:make-environment
	)
	(:import-from :abyss/context
		:initial-context
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/boole
		:not-impl :and-app-impl :or-app-impl :and-oper-impl :or-oper-impl
		:eq-impl
	)
)
(in-package :abyss/test/boole)

(def-suite* abyss-boole-tests :in abyss/test:abyss-tests)

(defun root-handler (eff)
	(cond
		((eq eff +eff-ret+) #'identity)
		((eq eff +eff-exn+) #'identity)
		((eq eff +eff-fix+) #'first)
		(t
			(print eff)
			(error "Unexpected effect")
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
