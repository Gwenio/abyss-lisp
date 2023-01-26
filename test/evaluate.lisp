
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

(test eval-identity
	(let ((empty (make-environment nil)))
		(is (null
			(evaluate (initial-context #'root-handler) nil empty)))
		(is (eql 0
			(evaluate (initial-context #'root-handler) 0 empty)))
		(is (equal "test"
			(evaluate (initial-context #'root-handler) "test" empty)))
		(is (eq empty
			(evaluate (initial-context #'root-handler) empty empty)))
		(is (functionp
			(evaluate (initial-context #'root-handler) #'identity empty)))
	)
)

(test eval-lookup
	(let ((env (make-environment nil)))
		(setf (gethash :x (env-table env)) nil)
		(is (null
			(evaluate (initial-context #'root-handler) :x env)))
		(signals sym-not-found
			(evaluate (initial-context #'root-handler) :fake env))
	)
)

(test eval-combine-operative
	(let ((env (make-environment nil)))
		(setf (gethash :x (env-table env)) nil)
		(is (equalp (list env)
			(evaluate
				(initial-context #'root-handler)
				(list #'normal-pass)
				env))
		)
		(is (equalp (list env :x)
			(evaluate
				(initial-context #'root-handler)
				(list #'normal-pass :x)
				env))
		)
		(is (equalp (list env :x)
			(evaluate
				(initial-context #'root-handler)
				(list (list #'(lambda (ctx _)
						(declare (ignore _))
						(normal-pass ctx #'normal-pass)
					)) :x)
				env))
		)
	)
)

(test eval-combine-applicative
	(let (
			(env (make-environment nil))
			(dummy (make-app #'normal-pass))
		)
		(setf (gethash :x (env-table env)) nil)
		(is (equalp (list env)
			(evaluate
				(initial-context #'root-handler)
				(list dummy)
				env)))
		(is (equalp (list env nil)
			(evaluate
				(initial-context #'root-handler)
				(list dummy nil)
				env)))
		(is (equalp (list env nil 0)
			(evaluate
				(initial-context #'root-handler)
				(list dummy nil 0)
				env)))
		(setf (gethash :f (env-table env)) dummy)
		(is (equalp (list env nil 0)
			(evaluate
				(initial-context #'root-handler)
				(list :f nil 0)
				env)))
		(setf (gethash :y (env-table env)) :x)
		(is (equalp (list* env nil 0 :x)
			(evaluate
				(initial-context #'root-handler)
				(list* dummy :x 0 :y)
				env)))
		(is (equalp (list* env nil 0 nil)
			(evaluate
				(initial-context #'root-handler)
				(list* (make-app dummy) :y 0 :y)
				env)))
	)
)

(test eval-invalid-comb
	(let ((empty (make-environment nil)))
		(signals invalid-combiner
			(evaluate
				(initial-context #'root-handler)
				(cons nil nil)
				empty)
		)
	)
)
