
(uiop:define-package :abyss/test/evaluate
	(:use :cl)
	(:mix :fiveam :abyss/types :abyss/error)
	(:import-from :abyss/context
		:initial-context :normal-pass
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
)
(in-package :abyss/test/evaluate)

(def-suite* abyss-eval-tests :in abyss/test:abyss-tests)

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

(defvar +f+ (make-glyph "f"))
(defvar +x+ (make-glyph "x"))
(defvar +y+ (make-glyph "y"))

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
		(setf (gethash +x+ (env-table env)) nil)
		(is (null (run-eval-case +x+ env)))
		(is (exn-type-p
			(run-eval-case +f+ env)
			+tid-sym-not-found+))
	)
)

(test eval-combine-operative
	(let ((env (make-environment nil)))
		(setf (gethash +x+ (env-table env)) nil)
		(is (equalp (list env)
			(run-eval-case (list #'normal-pass) env)
		))
		(is (equalp (list env +x+)
			(run-eval-case (list #'normal-pass +x+) env)
		))
		(is (equalp (list env +x+)
			(run-eval-case
				(list (list #'(lambda (_)
					(declare (ignore _))
					(normal-pass #'normal-pass)
				)) +x+)
				env)
		))
	)
)

(test eval-combine-applicative
	(let (
			(env (make-environment nil))
			(dummy (make-app #'normal-pass))
		)
		(setf (gethash +x+ (env-table env)) nil)
		(is (equalp (list env)
			(run-eval-case (list dummy) env)))
		(is (equalp (list env nil)
			(run-eval-case (list dummy nil) env)))
		(is (equalp (list env nil 0)
			(run-eval-case (list dummy nil 0) env)))
		(setf (gethash +f+ (env-table env)) dummy)
		(is (equalp (list env nil 0)
			(run-eval-case (list +f+ nil 0) env)))
		(setf (gethash +y+ (env-table env)) +x+)
		(is (equalp (list* env nil 0 +x+)
			(run-eval-case  (list* dummy +x+ 0 +y+) env)))
		(is (equalp (list* env nil 0 nil)
			(run-eval-case (list* (make-app dummy) +y+ 0 +y+) env)))
	)
)

(test eval-invalid-comb
	(let ((empty (make-environment nil)))
		(is (exn-type-p
			(run-eval-case (cons nil nil) empty)
			+tid-invalid-comb+)
		)
	)
)

(test eval-type-pred
	(let ((empty (make-environment nil)))
		(is (eq +true+
			(run-eval-case (list +tid-type-id+ +tid-type-id+) empty))
		)
	)
)
