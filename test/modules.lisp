
(uiop:define-package :abyss/test/modules
	(:use :cl)
	(:mix :fiveam :abyss/types)
	(:import-from :abyss/error
		:match-cons-p :type-exn-p
	)
	(:import-from :abyss/context
		:initial-context :normal-pass
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/operatives
		:seq-impl
	)
	(:import-from :abyss/ground
		:ground-env
	)
	(:import-from :abyss/modules
		:do-module-part
	)
)
(in-package :abyss/test/modules)

(def-suite* abyss-modules-tests :in abyss/test:abyss-tests)

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

(defmacro run-mod-case (imps x)
	`(initial-context
		(lambda () (do-module-part (ground-env) ,imps ,x))
		#'root-handler)
)

(test mod-basic
	(let (x y (foo (make-glyph "foo")) (bar (make-glyph "bar")))
		(setf x (run-mod-case nil
			(list (list (make-glyph "export") (list foo 7)))
		))
		(is (hash-table-p x))
		(is (= (gethash foo x)) 7)
		(setf y (run-mod-case (list (cons foo x))
			(list
				(list (make-glyph "let!") (list foo)
					(list (make-glyph "import") foo foo))
				(list (make-glyph "export") (list bar
					(list (make-glyph "*") 6 foo))))
		))
		(is (hash-table-p y))
		(is (= (gethash bar y)) 42)
	)
)
