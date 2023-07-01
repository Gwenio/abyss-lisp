
(uiop:define-package :abyss/test/handlers
	(:use :cl)
	(:mix :fiveam :abyss/types :abyss/error)
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

(defvar +k+ (make-glyph "k"))
(defvar +p+ (make-glyph "perform"))
(defvar +x+ (make-glyph "x"))
(defvar +ep+ (list (make-glyph "eff") +p+))
(defvar +exx+ (list (make-glyph "eff") +x+ +x+))
(defvar +exk+ (list (make-glyph "eff") +x+ +k+))

(test handlers-non-resume
	(let ()
		(is (eq t
			(run-h-case
				(list #'seq-impl
					(list #'define-impl +ep+
						(list #'make-eff-impl "dummy"))
					(list #'with-impl
						(list #'handler-impl +ignore+ +exx+)
						(list +p+ t)))
				(make-environment nil)))
		)
		(is (eq t ; with symbol for binding continuation
			(run-h-case
				(list #'seq-impl
					(list #'define-impl +ep+
						(list #'make-eff-impl "dummy"))
					(list #'with-impl
						(list #'handler-impl +k+ +exx+)
						(list +p+ t)))
				(make-environment nil)))
		)
		(is (exn-type-p
			(run-h-case ; check continuation is not bound for non-resumable
				(list #'seq-impl
					(list #'define-impl +ep+
						(list #'make-eff-impl "dummy"))
					(list #'with-impl
						(list #'handler-impl +k+ +exk+)
						(list +p+ t)))
				(make-environment nil))
			+tid-sym-not-found+)
		)
	)
)

(test handlers-resumable
	(let ()
		(is (eq t
			(run-h-case
				(list #'seq-impl
					(list #'define-impl +ep+
						(list #'make-eff/k-impl "dummy"))
					(list #'with-impl
						(list #'handler-impl +ignore+ +exx+)
						(list +p+ t)))
				(make-environment nil)))
		)
		(is (eq t ; with symbol for binding continuation
			(run-h-case
				(list #'seq-impl
					(list #'define-impl +ep+
						(list #'make-eff/k-impl "dummy"))
					(list #'with-impl
						(list #'handler-impl +k+ +exx+)
						(list +p+ t)))
				(make-environment nil)))
		)
		(is (eq t ; check continuation is bound for resumable
			(run-h-case
				(list #'seq-impl
					(list #'define-impl +ep+
						(list #'make-eff/k-impl "dummy"))
					(list #'with-impl
						(list #'handler-impl +k+
							(list (make-glyph "eff") +x+
								(list (make-app #'resume-impl) +k+ +x+)))
						(list +p+ t)))
				(make-environment nil)))
		)
	)
)

(test handlers-throw
	(let ((env (make-environment nil)))
		(is (eq t
			(run-h-case
				(list #'with-impl
					(list #'handler-impl +ignore+ (list +eff-exn+ +x+ +x+))
					(list (make-app #'throw-impl) t)
				)
				env))
		)
	)
)

(test handlers-state
	(let (
		(c (make-glyph "cons"))
		(g (make-glyph "get"))
		(s (make-glyph "set"))
		(save (make-glyph "save"))
		(st (make-glyph "state"))
		(load (make-glyph "load"))
		(res (make-glyph "resume"))
		)
		(is (equal '((4 . 2) 4 . 2)
			(run-h-case
				(list #'seq-impl
					(list #'define-impl res
						(make-app #'resume-impl))
					(list #'define-impl c
						(make-app #'cons-impl))
					(list #'define-impl (list load g)
						(list #'make-eff/k-impl "get"))
					(list #'define-impl (list save s)
						(list #'make-eff/k-impl "set"))
					(list #'with-impl
						(list #'handler/s-impl +k+ (list st)
							`(,+eff-ret+ ,+x+ (,c ,st ,+x+))
							`(,load nil ((,res ,+k+ ,st) ,st))
							`(,save ,+x+ ((,res ,+k+ ,+inert+) ,+x+)))
						'(2)
						`(,s (,c 4 (,g nil)))
						`(,g nil)))
				(make-environment nil)))
		)
		(is (equal '(4 . 2)
			(run-h-case
				(list #'seq-impl
					(list #'define-impl res
						(make-app #'resume-impl))
					(list #'define-impl c
						(make-app #'cons-impl))
					(list #'define-impl (list load g)
						(list #'make-eff/k-impl "get"))
					(list #'define-impl (list save s)
						(list #'make-eff/k-impl "set"))
					(list #'with-impl
						(list #'handler/s-impl +k+ (list st)
							`(,load nil ((,res ,+k+ ,st) ,st))
							`(,save ,+x+ ((,res ,+k+ ,+inert+) ,+x+)))
						'(2)
						`(,s (,c 4 (,g nil)))
						`(,g nil)))
				(make-environment nil)))
		)
	)
)
