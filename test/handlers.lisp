
(uiop:define-package :abyss/test/handlers
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/types
		:+inert+ :+ignore+ :+true+ :+false+ :applicative-p
		:make-app :inert-p :+eff-exn+ :+eff-fix+ :+eff-ret+
	)
	(:import-from :abyss/error
		:sym-not-found-p
	)
	(:import-from :abyss/environment
		:make-environment :env-table
	)
	(:import-from :abyss/context
		:initial-context
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/operatives
		:seq-impl :define-impl :vau-impl :lambda-impl :if-impl :cond-impl
		:let-impl :let*-impl :letrec-impl :letrec*-impl
	)
	(:import-from :abyss/lists
		:cons-impl
	)
	(:import-from :abyss/handlers
		:make-eff-impl :make-eff/k-impl
		:with-impl :handler-impl :handler/s-impl
		:throw-impl :recover-impl :resume-impl
	)
)
(in-package :abyss/test/handlers)

(def-suite* abyss-handlers-tests :in abyss/test:abyss-tests)

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

(defmacro run-h-case (x env)
	`(initial-context (lambda () (evaluate ,x ,env)) #'root-handler)
)

(test handlers-non-resume
	(let ()
		(is (eq t
			(run-h-case
				(list #'seq-impl
					(list #'define-impl '(:eff :perform)
						(list #'make-eff-impl "dummy"))
					(list #'with-impl
						(list #'handler-impl +ignore+ '(:eff :x :x))
						(list :perform t)))
				(make-environment nil)))
		)
		(is (eq t ; with symbol for binding continuation
			(run-h-case
				(list #'seq-impl
					(list #'define-impl '(:eff :perform)
						(list #'make-eff-impl "dummy"))
					(list #'with-impl
						(list #'handler-impl :k '(:eff :x :x))
						(list :perform t)))
				(make-environment nil)))
		)
		(is (sym-not-found-p
			(run-h-case ; check continuation is not bound for non-resumable
				(list #'seq-impl
					(list #'define-impl '(:eff :perform)
						(list #'make-eff-impl "dummy"))
					(list #'with-impl
						(list #'handler-impl :k '(:eff :x :k))
						(list :perform t)))
				(make-environment nil)))
		)
	)
)

(test handlers-resumable
	(let ()
		(is (eq t
			(run-h-case
				(list #'seq-impl
					(list #'define-impl '(:eff :perform)
						(list #'make-eff/k-impl "dummy"))
					(list #'with-impl
						(list #'handler-impl +ignore+ '(:eff :x :x))
						(list :perform t)))
				(make-environment nil)))
		)
		(is (eq t ; with symbol for binding continuation
			(run-h-case
				(list #'seq-impl
					(list #'define-impl '(:eff :perform)
						(list #'make-eff/k-impl "dummy"))
					(list #'with-impl
						(list #'handler-impl :k '(:eff :x :x))
						(list :perform t)))
				(make-environment nil)))
		)
		(is (eq t ; check continuation is bound for resumable
			(run-h-case
				(list #'seq-impl
					(list #'define-impl '(:eff :perform)
						(list #'make-eff/k-impl "dummy"))
					(list #'with-impl
						(list #'handler-impl :k
							(list :eff :x
								(list (make-app #'resume-impl) :k :x)))
						(list :perform t)))
				(make-environment nil)))
		)
	)
)

(test handlers-throw
	(let ((env (make-environment nil)))
		(is (eq t
			(run-h-case
				(list #'with-impl
					(list #'handler-impl +ignore+ (list +eff-exn+ :x :x))
					(list (make-app #'throw-impl) t)
				)
				env))
		)
	)
)

(test handlers-state
	(let ()
		(is (equal '((4 . 2) 4 . 2)
			(run-h-case
				(list #'seq-impl
					(list #'define-impl :resume
						(make-app #'resume-impl))
					(list #'define-impl :cons
						(make-app #'cons-impl))
					(list #'define-impl '(:load :get)
						(list #'make-eff/k-impl "get"))
					(list #'define-impl '(:save :set)
						(list #'make-eff/k-impl "set"))
					(list #'with-impl
						(list #'handler/s-impl :k '(:state)
							`(,+eff-ret+ :x (:cons :state :x))
							'(:load nil ((:resume :k :state) :state))
							`(:save :x ((:resume :k ,+inert+) :x)))
						'(2)
						'(:set (:cons 4 (:get nil)))
						'(:get nil)))
				(make-environment nil)))
		)
	)
)
