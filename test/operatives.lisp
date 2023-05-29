
(uiop:define-package :abyss/test/operatives
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
)
(in-package :abyss/test/operatives)

(def-suite* abyss-operatives-tests :in abyss/test:abyss-tests)

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

(defmacro run-oper-case (x env)
	`(initial-context (lambda () (evaluate ,x ,env)) #'root-handler)
)

(test oper-seq
	(let ((env (make-environment nil)))
		(is (inert-p
			(run-oper-case
				(list #'seq-impl)
				env))
		)
		(is (eql 0
			(run-oper-case
				(list #'seq-impl 0)
				env))
		)
		(is (eql 2
			(run-oper-case
				(list #'seq-impl 0 1 2)
				env))
		)
	)
)

(test oper-def
	(let ((env (make-environment nil)))
		(is (inert-p
			(run-oper-case
				(flet ((foo (x) (normal-pass (cdr x))))
					(list #'seq-impl
						(list #'define-impl :i 0)
						(list #'define-impl
							'(:x :y . :z)
							(list* #'foo 1 2 3))
						(list #'define-impl
							(list* +ignore+ '(:m :n) +ignore+)
							(list #'foo 4 (list 5 6)))
						(list #'define-impl nil nil)
					)
				)
				env))
		)
		(loop with table = (env-table env)
			for x = 0 then (+ x 1)
			for y in '(:i :x :y :z nil :m :n)
			when y
			do (is (eql x (gethash y table)))
		)
		(signals arg-expect-null
			(run-oper-case
				(list #'define-impl nil 1)
				env)
		)
		(signals arg-expect-pair
			(run-oper-case
				(list #'define-impl '(:x) 1)
				env)
		)
	)
)

(test oper-vau
	(let ((env (make-environment nil)))
		(is (functionp
			(run-oper-case
				(list #'vau-impl nil +ignore+)
				env))
		)
		(is (functionp
			(run-oper-case
				(list #'vau-impl nil +ignore+ 1)
				env))
		)
		(signals improper-list
			(run-oper-case
				(list* #'vau-impl nil +ignore+ 1)
				env)
		)
		(is (inert-p
			(run-oper-case
				(list (list #'vau-impl nil +ignore+))
				env))
		)
		(is (eql 1
			(run-oper-case
				(list (list #'vau-impl nil +ignore+ 1))
				env))
		)
		(setf (gethash :ident (env-table env))
			(make-app #'(lambda (args) (normal-pass (cadr args)))))
		(setf (gethash :x (env-table env)) 42)
		(is (eql 42
			(run-oper-case
				(list (list #'vau-impl nil +ignore+ '(:ident :x)))
				env))
		)
		(is (eql 7
			(run-oper-case
				(list
					(list #'vau-impl '(:y) +ignore+
						'(:ident :x)
						'(:ident :y))
					7)
				env))
		)
		(is (eql 3
			(run-oper-case
				(list
					(list #'vau-impl '(:y :x) +ignore+
						'(:ident :y)
						'(:ident :x))
					7 3)
				env))
		)
		(is (eql 9
			(run-oper-case
				(list
					(list #'vau-impl '(:y :x) +ignore+
						'(:ident :y)
						(list #'define-impl :x 9)
						'(:ident :x))
					7 3)
				env))
		)
	)
)

(test oper-lambda
	(let ((env (make-environment nil)))
		(is (applicative-p
			(run-oper-case
				(list #'lambda-impl nil)
				env))
		)
		(is (applicative-p
			(run-oper-case
				(list #'lambda-impl nil 1)
				env))
		)
		(signals improper-list
			(run-oper-case
				(list* #'lambda-impl nil 1)
				env)
		)
		(is (inert-p
			(run-oper-case
				(list (list #'lambda-impl nil))
				env))
		)
		(is (eql 1
			(run-oper-case
				(list (list #'lambda-impl nil 1))
				env))
		)
		(setf (gethash :ident (env-table env))
			(make-app #'(lambda (args) (normal-pass (cadr args)))))
		(setf (gethash :x (env-table env)) 42)
		(is (eql 42
			(run-oper-case
				(list (list #'lambda-impl nil '(:ident :x)))
				env))
		)
		(is (eql 7
			(run-oper-case
				(list
					(list #'lambda-impl '(:y)
						'(:ident :x)
						'(:ident :y))
					7)
				env))
		)
		(is (eql 3
			(run-oper-case
				(list
					(list #'lambda-impl '(:y :x)
						'(:ident :y)
						'(:ident :x))
					7 3)
				env))
		)
		(is (eql 9
			(run-oper-case
				(list
					(list #'lambda-impl '(:y :x)
						'(:ident :y)
						(list #'define-impl :x 9)
						'(:ident :x))
					7 3)
				env))
		)
	)
)

(test oper-if
	(let ((env (make-environment nil)))
		(is (eql 0
			(run-oper-case
				(list #'if-impl +true+ 0 1)
				env))
		)
		(is (eql 1
			(run-oper-case
				(list #'if-impl +false+ 0 1)
				env))
		)
		(loop with table = (env-table env)
			for x in (list +true+ +false+ 0 1)
			for y in '(:w :x :y :z)
			do (setf (gethash y table) x)
		)
		(is (eql 0
			(run-oper-case
				(list #'if-impl :w :y :z)
				env))
		)
		(is (eql 1
			(run-oper-case
				(list #'if-impl :x :y :z)
				env))
		)
	)
)

(test oper-cond
	(let ((env (make-environment nil)))
		(is (eql 0
			(run-oper-case
				(list #'cond-impl (list +true+ 0))
				env))
		)
		(is (eql 1
			(run-oper-case
				(list #'cond-impl
					(list +false+ 0)
					(list +true+ 1)
				)
				env))
		)
		(is (eql 0
			(run-oper-case
				(list #'cond-impl
					(list +true+ 0)
					(list +true+ 1)
				)
				env))
		)
		(loop with table = (env-table env)
			for x in (list +true+ +false+ 0 1)
			for y in '(:w :x :y :z)
			do (setf (gethash y table) x)
		)
		(is (eql 0
			(run-oper-case
				(list #'cond-impl '(:w :y))
				env))
		)
		(is (eql 1
			(run-oper-case
				(list #'cond-impl
					'(:x :y)
					'(:w :z)
				)
				env))
		)
		(is (eql 0
			(run-oper-case
				(list #'cond-impl
					'(:w :y)
					'(:w :z)
				)
				env))
		)
		(is (eql 0
			(run-oper-case
				(list #'cond-impl
					'(:w :w :y)
				)
				env))
		)
	)
)

(test oper-let
	(let ((env (make-environment nil)))
		(is (inert-p
			(run-oper-case
				(list #'let-impl nil)
				env))
		)
		(is (eql 0
			(run-oper-case
				(list #'let-impl nil 0)
				env))
		)
		(is (eql 0
			(run-oper-case
				(list #'let-impl '((:x 0)) :x)
				env))
		)
		(is (eql 0
			(run-oper-case
				(list #'let-impl '((:x 0) (:y 1)) :x)
				env))
		)
		(is (eql 1
			(run-oper-case
				(list #'let-impl '((:x 0) (:y 1)) :y)
				env))
		)
		; todo: test errors are produced when given bad params
	)
)

(test oper-let*
	(let ((env (make-environment nil)))
		(is (eql 0
			(run-oper-case
				(list #'let*-impl '((:x 0) (:y :x)) :y)
				env))
		)
		(is (eql 0
			(run-oper-case
				(list #'let*-impl '((:x 0) (:y :x)) :x)
				env))
		)
	)
)

(declaim (ftype (function (fixnum) fixnum) fib))
(defun fib (n)
	(if (< n 2)
		n
		(+ (fib (- n 1)) (fib (- n 2)))
	)
)

(test oper-letrec
	(let ((env (make-environment nil)) (fib-n 10))
		(setf (gethash :$if (env-table env)) #'if-impl)
		(setf (gethash :$lambda (env-table env)) #'lambda-impl)
		(setf (gethash :<? (env-table env))
			(make-app (lambda (args)
				(normal-pass (if (< (second args) (third args))
					+true+ +false+
				)))))
		(setf (gethash :+ (env-table env))
			(make-app (lambda (args)
				(normal-pass (+ (second args) (third args))))))
		(setf (gethash :- (env-table env))
			(make-app (lambda (args)
				(normal-pass (- (second args) (third args))))))
		(is (eql (fib fib-n) (initial-context (lambda ()
				(evaluate (list
					#'letrec-impl
					'((:fib (:$lambda (:n)
						(:$if (:<? :n 2)
							:n
							(:+ (:fib (:- :n 1)) (:fib (:- :n 2)))
						)
					)))
					`(:fib ,fib-n)) env)
			) #'root-handler))
		)
		; todo: test that letrec evals bound values in the inner env
	)
)
