
(uiop:define-package :abyss/test/operatives
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/types
		:+ignore+ :+true+ :+false+ :applicative-p :make-glyph
		:make-app :inert-p :+eff-exn+ :+eff-fix+ :+eff-ret+
	)
	(:import-from :abyss/error
		:arg-null-p :arg-pair-p :improper-list-p :type-exn-p
	)
	(:import-from :abyss/environment
		:make-environment :env-table
	)
	(:import-from :abyss/context
		:initial-context :normal-pass
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/operatives
		:seq-impl :define-impl :vau-impl :lambda-impl :if-impl :cond-impl
		:let-impl :let*-impl
	)
)
(in-package :abyss/test/operatives)

(def-suite* abyss-operatives-tests :in abyss/test:abyss-tests)

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

(defvar +i+ (make-glyph "i"))
(defvar +m+ (make-glyph "m"))
(defvar +n+ (make-glyph "n"))
(defvar +w+ (make-glyph "w"))
(defvar +x+ (make-glyph "x"))
(defvar +y+ (make-glyph "y"))
(defvar +z+ (make-glyph "z"))

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
						(list #'define-impl +i+ 0)
						(list #'define-impl
							(list* +x+ +y+ +z+)
							(list* #'foo 1 2 3))
						(list #'define-impl
							(list* +ignore+ (list +m+ +n+) +ignore+)
							(list #'foo 4 (list 5 6)))
						(list #'define-impl nil nil)
					)
				)
				env))
		)
		(loop with table = (env-table env)
			for x = 0 then (+ x 1)
			for y in (list +i+ +x+ +y+ +z+ nil +m+ +n+)
			when y
			do (is (eql x (gethash y table)))
		)
		(is (arg-null-p
			(run-oper-case
				(list #'define-impl nil 1)
				env))
		)
		(is (arg-pair-p
			(run-oper-case
				(list #'define-impl (list +x+) 1)
				env))
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
		(is (type-exn-p
			(run-oper-case
				(list* #'vau-impl nil +ignore+ 1)
				env))
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
		(setf (gethash +i+ (env-table env))
			(make-app #'(lambda (args) (normal-pass (cadr args)))))
		(setf (gethash +x+ (env-table env)) 42)
		(is (eql 42
			(run-oper-case
				(list (list #'vau-impl nil +ignore+ (list +i+ +x+)))
				env))
		)
		(is (eql 7
			(run-oper-case
				(list
					(list #'vau-impl (list +y+) +ignore+
						(list +i+ +x+)
						(list +i+ +y+))
					7)
				env))
		)
		(is (eql 3
			(run-oper-case
				(list
					(list #'vau-impl (list +y+ +x+) +ignore+
						(list +i+ +y+)
						(list +i+ +x+))
					7 3)
				env))
		)
		(is (eql 9
			(run-oper-case
				(list
					(list #'vau-impl (list +y+ +x+) +ignore+
						(list +i+ +y+)
						(list #'define-impl +x+ 9)
						(list +i+ +x+))
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
		(is (type-exn-p
			(run-oper-case
				(list* #'lambda-impl nil 1)
				env))
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
		(setf (gethash +i+ (env-table env))
			(make-app #'(lambda (args) (normal-pass (cadr args)))))
		(setf (gethash +x+ (env-table env)) 42)
		(is (eql 42
			(run-oper-case
				(list (list #'lambda-impl nil (list +i+ +x+)))
				env))
		)
		(is (eql 7
			(run-oper-case
				(list
					(list #'lambda-impl (list +y+)
						(list +i+ +x+)
						(list +i+ +y+))
					7)
				env))
		)
		(is (eql 3
			(run-oper-case
				(list
					(list #'lambda-impl (list +y+ +x+)
						(list +i+ +y+)
						(list +i+ +x+))
					7 3)
				env))
		)
		(is (eql 9
			(run-oper-case
				(list
					(list #'lambda-impl (list +y+ +x+)
						(list +i+ +y+)
						(list #'define-impl +x+ 9)
						(list +i+ +x+))
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
			for y in (list +w+ +x+ +y+ +z+)
			do (setf (gethash y table) x)
		)
		(is (eql 0
			(run-oper-case
				(list #'if-impl +w+ +y+ +z+)
				env))
		)
		(is (eql 1
			(run-oper-case
				(list #'if-impl +x+ +y+ +z+)
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
			for y in (list +w+ +x+ +y+ +z+)
			do (setf (gethash y table) x)
		)
		(is (eql 0
			(run-oper-case
				(list #'cond-impl (list +w+ +y+))
				env))
		)
		(is (eql 1
			(run-oper-case
				(list #'cond-impl
					(list +x+ +y+)
					(list +w+ +z+)
				)
				env))
		)
		(is (eql 0
			(run-oper-case
				(list #'cond-impl
					(list +w+ +y+)
					(list +w+ +z+)
				)
				env))
		)
		(is (eql 0
			(run-oper-case
				(list #'cond-impl
					(list +w+ +w+ +y+)
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
				(list #'let-impl `((,+x+ 0)) +x+)
				env))
		)
		(is (eql 0
			(run-oper-case
				(list #'let-impl `((,+x+ 0) (,+y+ 1)) +x+)
				env))
		)
		(is (eql 1
			(run-oper-case
				(list #'let-impl `((,+x+ 0) (,+y+ 1)) +y+)
				env))
		)
		; todo: test errors are produced when given bad params
	)
)

(test oper-let*
	(let ((env (make-environment nil)))
		(is (eql 0
			(run-oper-case
				(list #'let*-impl `((,+x+ 0) (,+y+ ,+x+)) +y+)
				env))
		)
		(is (eql 0
			(run-oper-case
				(list #'let*-impl `((,+x+ 0) (,+y+ ,+x+)) +x+)
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

; (defvar +fib+ (make-glyph "fib"))
; (defvar +lam+ (make-glyph "$lambda"))
; (defvar +if+ (make-glyph "$if"))
; (defvar +lt+ (make-glyph "<?"))
; (defvar +plus+ (make-glyph "+"))
; (defvar +sub+ (make-glyph "-"))

; (test oper-letrec
; 	(let ((env (make-environment nil)) (fib-n 10))
; 		(setf (gethash +if+ (env-table env)) #'if-impl)
; 		(setf (gethash +lam+ (env-table env)) #'lambda-impl)
; 		(setf (gethash +lt+ (env-table env))
; 			(make-app (lambda (args)
; 				(normal-pass (if (< (second args) (third args))
; 					+true+ +false+
; 				)))))
; 		(setf (gethash +plus+ (env-table env))
; 			(make-app (lambda (args)
; 				(normal-pass (+ (second args) (third args))))))
; 		(setf (gethash +sub+ (env-table env))
; 			(make-app (lambda (args)
; 				(normal-pass (- (second args) (third args))))))
; 		(is (eql (fib fib-n) (initial-context (lambda ()
; 				(evaluate (list
; 					#'letrec-impl
; 					`((,+fib+ (,+lam+ (,+n+)
; 						(,+if+ (,+lt+ ,+n+ 2)
; 							,+n+
; 							(,+plus+ (,+fib+ (,+sub+ ,+n+ 1))
; 								(,+fib+ (,+sub+ ,+n+ 2)))
; 						)
; 					)))
; 					(list +fib+ fib-n)) env)
; 			) #'root-handler))
; 		)
; 		; todo: test that letrec evals bound values in the inner env
; 	)
; )
