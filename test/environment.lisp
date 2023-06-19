
(uiop:define-package :abyss/test/environment
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/types
		:make-glyph
	)
	(:import-from :abyss/environment
		:make-environment :environment-p :env-table :env-lookup
	)
)
(in-package :abyss/test/environment)

(def-suite* abyss-environment-tests :in abyss/test:abyss-tests)

(test env-make
	(is (environment-p (make-environment nil)))
	(is (environment-p
		(make-environment
			(make-environment nil))))
)

(defvar +x+ (make-glyph "x"))
(defvar +y+ (make-glyph "y"))

(test env-simple
	(let ((env (make-environment nil)))
		(let ((table (env-table env)))
			(setf (gethash +x+ table) 42)
			(is (eql 42 (multiple-value-bind (found x) (env-lookup env +x+)
				(is (eq t found))
				x
			)))
			(setf (gethash +y+ table) 7)
			(is (eql 7 (multiple-value-bind (found x) (env-lookup env +y+)
				(is (eq t found))
				x
			)))
			(setf (gethash +x+ table) 3)
			(is (eql 3 (multiple-value-bind (found x) (env-lookup env +x+)
				(is (eq t found))
				x
			)))
		)
	)
)

(test env-shadow
	(let ((parent (make-environment nil)))
		(let ((p-table (env-table parent)) (child (make-environment parent)))
			(let ((c-table (env-table child)))
				(setf (gethash +x+ p-table) 42)
				(is (eql 42 (multiple-value-bind (found x)
					(env-lookup child +x+)
					(is (eq t found))
					x
				)))
				(setf (gethash +x+ c-table) 7)
				(is (eql 7 (multiple-value-bind (found x)
					(env-lookup child +x+)
					(is (eq t found))
					x
				)))
				(is (eql 42 (multiple-value-bind (found x)
					(env-lookup parent +x+)
					(is (eq t found))
					x
				)))
			)
		)
	)
)

(test env-failure
	(let ((env (make-environment nil)))
		(is (not (multiple-value-bind (found _) (env-lookup env +x+)
			(declare (ignore _))
			found
		)))
	)
)

(test env-glyph
	(let ((env (make-environment nil)))
		(let ((table (env-table env)))
			(setf (gethash (make-glyph "x") table) 42)
			(is (eql 42 (multiple-value-bind (found x)
				(env-lookup env (make-glyph "x"))
				(is (eq t found))
				x
			)))
			(setf (gethash (make-glyph "y") table) 7)
			(is (eql 7 (multiple-value-bind (found x)
				(env-lookup env (make-glyph "y"))
				(is (eq t found))
				x
			)))
			(setf (gethash (make-glyph "x") table) 3)
			(is (eql 3 (multiple-value-bind (found x)
				(env-lookup env (make-glyph "x"))
				(is (eq t found))
				x
			)))
		)
	)
)
