
(uiop:define-package :abyss/test/applicatives
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/types
		:+ignore+ :+true+ :+false+ :applicative-p
		:make-app :inert-p :+eff-exn+ :+eff-fix+ :+eff-ret+
	)
	(:import-from :abyss/error
		:arg-pair-p :type-exn-expect
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
	(:import-from :abyss/applicatives
		:apply-impl :current-env-impl
	)
)
(in-package :abyss/test/applicatives)

(def-suite* abyss-applicatives-tests :in abyss/test:abyss-tests)

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
		(is (arg-pair-p
			(run-app-case
				(list* (make-app #'apply-impl)
					(make-app #'current-env-impl)
					nil env)
				env))
		)
		(is (eq 'abyss/types::applicative (type-exn-expect
			(run-app-case
				(list (make-app #'apply-impl)
					#'current-env-impl
					nil)
				env)))
		)
		(is (eq 'abyss/environment::environment (type-exn-expect
			(run-app-case
				(list (make-app #'apply-impl)
					(make-app #'current-env-impl)
					nil t)
				env)))
		)
	)
)
