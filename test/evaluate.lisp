
(uiop:define-package :abyss/test/evaluate
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/types
		:make-app :*eff-invalid-comb* :*eff-sym-not-found*
	)
	(:import-from :abyss/environment
		:make-environment :env-lookup :env-table :env-key-not-found
	)
	(:import-from :abyss/context
		:initial-context :normal-pass
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
)
(in-package :abyss/test/evaluate)

(def-suite* abyss-eval-tests :in abyss/test:abyss-tests)

(define-condition sym-not-found (error) ())
(define-condition invalid-combiner (error) ())

(defun root-handler (eff)
	(cond
		((eq eff *eff-sym-not-found*) (error 'sym-not-found))
		((eq eff *eff-invalid-comb*) (error 'invalid-combiner))
		(t (error "Unexpected effect."))
	)
)

(defmacro run-eval-case (x env)
	`(initial-context
		(lambda () (evaluate ,x ,env))
		#'root-handler)

)

(test eval-identity
	(let ((empty (make-environment nil)))
		(is (null (run-eval-case nil empty)))
		(is (eql 0 (run-eval-case 0 empty)))
		(is (equal "test" (run-eval-case "test" empty)))
		(is (eq empty (run-eval-case empty empty)))
		(is (functionp (run-eval-case #'identity empty)))
	)
)

(test eval-lookup
	(let ((env (make-environment nil)))
		(setf (gethash :x (env-table env)) nil)
		(is (null (run-eval-case :x env)))
		(signals sym-not-found
			(run-eval-case :fake env))
	)
)

(test eval-combine-operative
	(let ((env (make-environment nil)))
		(setf (gethash :x (env-table env)) nil)
		(is (equalp (list env)
			(run-eval-case (list #'normal-pass) env)
		))
		(is (equalp (list env :x)
			(run-eval-case (list #'normal-pass :x) env)
		))
		(is (equalp (list env :x)
			(run-eval-case
				(list (list #'(lambda (_)
					(declare (ignore _))
					(normal-pass #'normal-pass)
				)) :x)
				env)
		))
	)
)

(test eval-combine-applicative
	(let (
			(env (make-environment nil))
			(dummy (make-app #'normal-pass))
		)
		(setf (gethash :x (env-table env)) nil)
		(is (equalp (list env)
			(run-eval-case (list dummy) env)))
		(is (equalp (list env nil)
			(run-eval-case (list dummy nil) env)))
		(is (equalp (list env nil 0)
			(run-eval-case (list dummy nil 0) env)))
		(setf (gethash :f (env-table env)) dummy)
		(is (equalp (list env nil 0)
			(run-eval-case (list :f nil 0) env)))
		(setf (gethash :y (env-table env)) :x)
		(is (equalp (list* env nil 0 :x)
			(run-eval-case  (list* dummy :x 0 :y) env)))
		(is (equalp (list* env nil 0 nil)
			(run-eval-case (list* (make-app dummy) :y 0 :y) env)))
	)
)

(test eval-invalid-comb
	(let ((empty (make-environment nil)))
		(signals invalid-combiner
			(run-eval-case (cons nil nil) empty)
		)
	)
)
