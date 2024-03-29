
(uiop:define-package :abyss/test/applicatives
	(:use :cl)
	(:mix :fiveam :abyss/types :abyss/error)
	(:import-from :abyss/context
		:initial-context :normal-pass
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
		(is (exn-type-p
			(run-app-case
				(list* (make-app #'apply-impl)
					(make-app #'current-env-impl)
					nil env)
				env)
			+tid-match-cons+)
		)
		(let ((exn (run-app-case
				(list (make-app #'apply-impl)
					#'current-env-impl
					nil)
				env)))
			(is (exn-type-p exn +tid-type-exn+))
			(is (eq +tid-applicative+
				(gethash (make-glyph "expected") (record-obj exn))))
		)
		(let ((exn (run-app-case
				(list (make-app #'apply-impl)
					(make-app #'current-env-impl)
					nil t)
				env)))
			(is (exn-type-p exn +tid-type-exn+))
			(is (eq +tid-environment+
				(gethash (make-glyph "expected") (record-obj exn))))
		)
	)
)
