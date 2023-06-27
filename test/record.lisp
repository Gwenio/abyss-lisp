
(uiop:define-package :abyss/test/record
	(:use :cl)
	(:mix :fiveam :abyss/types)
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

(defvar +x+ (make-glyph "x"))
(defvar +y+ (make-glyph "y"))
(defvar +z+ (make-glyph "z"))

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
				(list* (list #'record-impl +x+ +true+) +x+)
				env))
		)
		(is (equal '(2 1)
			(run-record-case
				(list (list #'record-impl (list +x+ +y+) (list l 1 2)) +y+ +x+)
				env))
		)
	)
)

(test record-set
	(let (
			(env (make-environment nil))
			(l (make-app #'list-impl))
			(rec (make-glyph "rec"))
		)
		(is (eq +inert+
			(run-record-case
				(list #'record-set-impl (list #'record-impl +x+ +true+) +x+
					+false+)
				env))
		)
		(is (eq +false+
			(run-record-case
				(list #'let-impl
					(list (list rec (list #'record-impl +x+ +true+)))
					(list #'record-set-impl rec +x+ +false+)
					(list* rec +x+))
				env))
		)
		(is (equal '(2 1 3)
			(run-record-case
				(list #'let-impl
					(list (list rec (list #'record-impl
						(list +x+ +y+)
						(list l 1 2))))
					(list #'record-set-impl rec
						(list +z+ +x+ +y+)
						(list l 3 2 1))
					(list rec +x+ +y+ +z+))
				env))
		)
	)
)
