
(uiop:define-package :abyss/test/environment
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/environment
		:make-environment :environment-p :env-table :env-lookup
		:env-key-not-found
	)
)
(in-package :abyss/test/environment)

(def-suite* abyss-environment-tests :in abyss/test:abyss-tests)

(test env-make
	(is (environment-p (make-environment nil)))
	(is (environment-p
		(make-environment (list
			(make-environment nil)))))
)

(test env-simple
	(let ((env (make-environment nil)))
		(let ((table (env-table env)))
			(setf (gethash :x table) 42)
			(is (eql 42 (env-lookup env :x)))
			(setf (gethash :y table) 7)
			(is (eql 7 (env-lookup env :y)))
			(setf (gethash :x table) 3)
			(is (eql 3 (env-lookup env :x)))
		)
	)
)

(test env-shadow
	(let ((parent (make-environment nil)))
		(let ((p-table (env-table parent)) (child (make-environment parent)))
			(let ((c-table (env-table child)))
				(setf (gethash :x p-table) 42)
				(is (eql 42 (env-lookup child :x)))
				(setf (gethash :x c-table) 7)
				(is (eql 7 (env-lookup child :x)))
				(is (eql 42 (env-lookup parent :x)))
			)
		)
	)
)

(test env-failure
	(let ((env (make-environment nil)))
		(signals env-key-not-found (env-lookup env :x))
	)
)
