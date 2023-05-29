
(uiop:define-package :abyss/test/lists
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/types
		:+ignore+ :+true+ :+false+ :applicative-p
		:make-app :inert-p :+eff-exn+ :+eff-ret+
	)
	(:import-from :abyss/error
		:sym-not-found-exn :make-sym-not-found
		:invalid-comb-exn
		:improper-list-exn
		:bad-param-exn
		:arg-pair-exn
		:arg-null-exn
		:arg-repeat-exn
		:bad-cont-exn
		:type-exn
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
	(:import-from :abyss/operatives
		:seq-impl :define-impl :vau-impl :lambda-impl :if-impl :cond-impl
		:let-impl :let*-impl :letrec-impl :letrec*-impl
	)
	(:import-from :abyss/lists
		:null-p-impl :cons-p-impl :list-p-impl
		:cons-impl :list-impl :list*-impl :list-len-impl
		:first-impl :second-impl :third-impl :nth-impl
		:tail-impl :nth-tail-impl :last-impl :last-n-impl
	)
)
(in-package :abyss/test/lists)

(def-suite* abyss-lists-tests :in abyss/test:abyss-tests)

(define-condition sym-not-found (error) (datum))
(define-condition invalid-combiner (error) (datum))
(define-condition improper-list (error) (datum))
(define-condition bad-param-spec (error) (datum))
(define-condition arg-expect-pair (error) (datum))
(define-condition arg-expect-null (error) (datum))
(define-condition arg-repeated (error) (datum))
(define-condition bad-continuation (error) (datum))
(define-condition arg-expect-type (error) (datum))

(defun root-handler (eff)
	(cond
		((eq eff +eff-ret+) #'identity)
		((eq eff +eff-exn+)
			(lambda (x)
				(let ((x (car x))) (typecase x
					(sym-not-found-exn
						(error 'sym-not-found :datum x))
					(invalid-comb-exn
						(error 'invalid-combiner :datum x))
					(improper-list-exn
						(error 'improper-list :datum x))
					(bad-param-exn
						(error 'bad-param-spec :datum x))
					(arg-pair-exn
						(error 'arg-expect-pair :datum x))
					(arg-null-exn
						(error 'arg-expect-null :datum x))
					(arg-repeat-exn
						(error 'arg-repeated :datum x))
					(bad-cont-exn
						(error 'bad-continuation :datum x))
					(type-exn
						(error 'arg-expect-type :datum x))
					(t (print (type-of x)) (error "Unexpected exception"))
				))
			)
		)
		(t (error "Unexpected effect"))
	)
)

(defmacro run-lists-case (x env)
	`(initial-context (lambda () (evaluate ,x ,env)) #'root-handler)
)

(test lists-type-preds
	(let ((env (make-environment nil)))
		(setf (gethash :x (env-table env)) (cons nil nil))
		(is (eq +true+
			(run-lists-case
				(list (make-app #'null-p-impl) nil)
				env))
		)
		(is (eq +false+
			(run-lists-case
				(list (make-app #'null-p-impl) :x)
				env))
		)
		(is (eq +false+
			(run-lists-case
				(list (make-app #'cons-p-impl) nil)
				env))
		)
		(is (eq +true+
			(run-lists-case
				(list (make-app #'cons-p-impl) :x)
				env))
		)
		(is (eq +true+
			(run-lists-case
				(list (make-app #'list-p-impl) nil)
				env))
		)
		(is (eq +true+
			(run-lists-case
				(list (make-app #'list-p-impl) :x)
				env))
		)
	)
)

(test lists-construct
	(let ((env (make-environment nil)))
		(let ((cons-app (make-app #'cons-impl)))
			(is (equal (cons 0 1)
				(run-lists-case
					(list cons-app 0 1)
					env))
			)
			(is (equal (cons 0 (cons 1 2))
				(run-lists-case
					(list cons-app 0 (list cons-app 1 2))
					env))
			)
		)
		(let ((list-app (make-app #'list-impl)))
			(is (eq nil
				(run-lists-case
					(list list-app)
					env))
			)
			(is (eql 0
				(run-lists-case
					(list* list-app 0)
					env))
			)
			(is (equal '(0)
				(run-lists-case
					(list list-app 0)
					env))
			)
			(is (equal '(0 1 2)
				(run-lists-case
					(list list-app 0 1 2)
					env))
			)
			(is (equal '(0 1 . 2)
				(run-lists-case
					(list* list-app 0 1 2)
					env))
			)
		)
		(let ((list*-app (make-app #'list*-impl)))
			(is (eql 0
				(run-lists-case
					(list list*-app 0)
					env))
			)
			(is (equal '(0 1 . 2)
				(run-lists-case
					(list list*-app 0 1 2)
					env))
			)
		)
	)
)

(test lists-accessors
	(let ((env (make-environment nil)))
		(setf (gethash :x (env-table env)) '(0 1 2 3 4 5))
		(is (eql 0
			(run-lists-case
				(list (make-app #'first-impl) :x)
				env))
		)
		(is (eql 1
			(run-lists-case
				(list (make-app #'second-impl) :x)
				env))
		)
		(is (eql 2
			(run-lists-case
				(list (make-app #'third-impl) :x)
				env))
		)
		(is (equal '(1 2 3 4 5)
			(run-lists-case
				(list (make-app #'tail-impl) :x)
				env))
		)
		(is (eql 0
			(run-lists-case
				(list (make-app #'nth-impl) 0 :x)
				env))
		)
		(is (eql 3
			(run-lists-case
				(list (make-app #'nth-impl) 3 :x)
				env))
		)
		(is (equal '(4 5)
			(run-lists-case
				(list (make-app #'nth-tail-impl) 3 :x)
				env))
		)
		(is (equal '(5)
			(run-lists-case
				(list (make-app #'last-impl) :x)
				env))
		)
		(is (eq nil
			(run-lists-case
				(list (make-app #'last-n-impl) 0 :x)
				env))
		)
		(is (equal '(5)
			(run-lists-case
				(list (make-app #'last-n-impl) 1 :x)
				env))
		)
		(is (equal '(4 5)
			(run-lists-case
				(list (make-app #'last-n-impl) 2 :x)
				env))
		)
		(is (equal '(0 1 2 3 4 5)
			(run-lists-case
				(list (make-app #'last-n-impl) 8 :x)
				env))
		)
	)
)

(test lists-length
	(let ((env (make-environment nil)))
		(setf (gethash :x (env-table env)) '(0 1 2 3 4 5))
		(is (eql 6
			(run-lists-case
				(list (make-app #'list-len-impl) :x)
				env))
		)
	)
)
