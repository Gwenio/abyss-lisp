
(uiop:define-package :abyss/test/record
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/types
		:+true+ :+false+ :+inert+ :make-app :+eff-exn+ :+eff-fix+ :+eff-ret+
	)
	(:import-from :abyss/environment
		:make-environment
	)
	(:import-from :abyss/context
		:initial-context
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/operatives
		:let-impl
	)
	(:import-from :abyss/lists
		:list-impl
	)
	(:import-from :abyss/record
		:record-p-impl :record-impl :record-set-impl
	)
)
(in-package :abyss/test/record)

(def-suite* abyss-record-tests :in abyss/test:abyss-tests)

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

(defmacro run-record-case (x env)
	`(initial-context (lambda () (evaluate ,x ,env)) #'root-handler)
)

(test record-type-p
	(let ((env (make-environment nil)) (rec-p (make-app #'record-p-impl)))
		(is (eq +false+
			(run-record-case
				(list rec-p nil)
				env))
		)
		(is (eq +true+
			(run-record-case
				(list rec-p (list #'record-impl nil nil))
				env))
		)
	)
)

(test record-fetch
	(let ((env (make-environment nil)) (l (make-app #'list-impl)))
		(is (eq +true+
			(run-record-case
				(list* (list #'record-impl :x +true+) :x)
				env))
		)
		(is (equal '(2 1)
			(run-record-case
				(list (list #'record-impl '(:x :y) (list l 1 2)) :y :x)
				env))
		)
	)
)

(test record-set
	(let ((env (make-environment nil)) (l (make-app #'list-impl)))
		(is (eq +inert+
			(run-record-case
				(list #'record-set-impl (list #'record-impl :x +true+) :x
					+false+)
				env))
		)
		(is (eq +false+
			(run-record-case
				(list #'let-impl
					(list (list :rec (list #'record-impl :x +true+)))
					(list #'record-set-impl :rec :x +false+)
					'(:rec . :x))
				env))
		)
		(is (equal '(2 1 3)
			(run-record-case
				(list #'let-impl
					(list (list :rec (list #'record-impl '(:x :y) (list l 1 2))))
					(list #'record-set-impl :rec '(:z :x :y) (list l 3 2 1))
					'(:rec :x :y :z))
				env))
		)
	)
)
