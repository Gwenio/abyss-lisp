
(uiop:define-package :abyss/test/context
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/types :+eff-ret+)
	(:import-from :abyss/context
		:shift-context :resume-context :fresh-context :initial-context
		:error-guard :final-guard :push-frame :normal-pass
	)
)
(in-package :abyss/test/context)

(def-suite* abyss-context-tests :in abyss/test:abyss-tests)


(defun root-handler (eff)
	(cond
		((eq eff +eff-ret+) #'identity)
		(t (print eff)
			(error "Unhandled effect.")
		)
	)
)


(defun shift-handler (eff)
	(if (eq eff +eff-ret+)
		#'normal-pass
		(lambda (x)
			(resume-context (car x))
			(normal-pass (cdr x))
		)
	)
)

(test initial-ctx-exit
	(is (eql 42 (initial-context
		(lambda () (normal-pass 42))
		#'root-handler)))
)

(test ctx-frame-pushing
	(is (eql 42 (initial-context (lambda ()
		(push-frame #'(lambda (x) (normal-pass (- x 7))))
		(push-frame #'(lambda (x) (normal-pass (* x 7))))
		(normal-pass 7)
	) #'root-handler)))
)

(test ctx-next-frame
	(is (eql 42 (initial-context (lambda ()
		(fresh-context (lambda () (normal-pass 42)) #'shift-handler)
	) #'root-handler)))
)

(test ctx-error-guard
	(let ((called nil))
		(is (eql 42 (initial-context (lambda ()
			(error-guard #'(lambda (x)
				(setf called t)
				(normal-pass x)
			))
			(normal-pass 42)
		) #'root-handler)))
		(is (null called))
	)
)

(test ctx-final-guard
	(let ((called nil))
		(is (eql 42 (initial-context (lambda ()
			(final-guard #'(lambda (x)
				(setf called t)
				(normal-pass x)
			))
			(normal-pass 42)
		) #'root-handler)))
		(is (eq t called))
	)
)

(test ctx-shift-resume
	(let ((called nil))
		(initial-context (lambda ()
			(push-frame #'(lambda (x) (normal-pass (- x 7))))
			(fresh-context  (lambda ()
				(push-frame #'(lambda (x) (normal-pass (* x 7))))
				(error-guard #'(lambda (x)
					(setf called t)
					(normal-pass x)
				))
				(multiple-value-bind (f k) (shift-context t)
					(is (eql 42 (funcall f (cons k 7))))
					(is (null called))
				)
			) #'shift-handler)
		) #'root-handler)
	)
)

(test ctx-shift-unwind
	(let ((called nil))
		(initial-context (lambda ()
			(push-frame #'(lambda (x) (normal-pass (- x 7))))
			(fresh-context  (lambda ()
				(push-frame #'(lambda (x) (normal-pass (* x 7))))
				(error-guard #'(lambda (x)
					(setf called t)
					(normal-pass x)
				))
				(multiple-value-bind (f _k) (shift-context +eff-ret+)
 					(declare (ignore _k))
					(is (eql 35 (funcall f 42)))
					(is (eq t called))
				)
			) #'shift-handler)
		) #'root-handler)
	)
)
