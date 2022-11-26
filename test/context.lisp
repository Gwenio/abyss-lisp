
(uiop:define-package :abyss/test/context
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/context
		:shift-context :resume-context :fresh-context :initial-context
		:error-guard :final-guard :push-frame :normal-pass :context-handler
	)
)
(in-package :abyss/test/context)

(def-suite* abyss-context-tests :in abyss/test:abyss-tests)

(defun root-handler (_effect)
	(declare (ignore _effect))
	(error "Unhandled effect.")
)

(defun shift-handler (effect)
	(if effect
		#'(lambda (ctx x)
			(normal-pass
				(resume-context ctx (car x))
				(cdr x)
			)
		)
		#'normal-pass
	)
)

(test initial-ctx-exit
	(is (eql 42 (normal-pass (initial-context #'root-handler) 42)))
)

(test ctx-frame-pushing
	(let ((ctx (initial-context #'root-handler)))
		(push-frame ctx #'(lambda (k x) (normal-pass k (- x 7))))
		(push-frame ctx #'(lambda (k x) (normal-pass k (* x 7))))
		(is (eql 42 (normal-pass ctx 7)))
	)
)

(test ctx-next-frame
	(let ((initial (initial-context #'root-handler)))
		(is (eql 42 (normal-pass
			(fresh-context initial #'shift-handler)
			42)))
	)
)

(test ctx-error-guard
	(let ((ctx (initial-context #'root-handler)) (called nil))
		(error-guard ctx #'(lambda (k x)
			(setf called t)
			(normal-pass k x)
		))
		(is (eql 42 (normal-pass ctx 42)))
		(is (null called))
	)
)

(test ctx-final-guard
	(let ((ctx (initial-context #'root-handler)) (called nil))
		(final-guard ctx #'(lambda (k x)
			(setf called t)
			(normal-pass k x)
		))
		(is (eql 42 (normal-pass ctx 42)))
		(is (eq t called))
	)
)

(test ctx-shift-resume
	(let ((initial (initial-context #'root-handler)) (called nil))
		(push-frame initial #'(lambda (k x) (normal-pass k (- x 7))))
		(let ((child (fresh-context initial #'shift-handler)))
			(push-frame child #'(lambda (k x) (normal-pass k (* x 7))))
			(error-guard child #'(lambda (k x)
				(setf called t)
				(normal-pass k x)
			))
			(multiple-value-bind (f k next) (shift-context child t)
				(is (eql 42 (funcall f next (cons k 7))))
				(is (null called))
			)
		)
	)
)

(test ctx-shift-unwind
	(let ((initial (initial-context #'root-handler)) (called nil))
		(push-frame initial #'(lambda (k x) (normal-pass k (- x 7))))
		(let ((child (fresh-context initial #'shift-handler)))
			(push-frame child #'(lambda (k x) (normal-pass k (* x 7))))
			(error-guard child #'(lambda (k x)
				(setf called t)
				(normal-pass k x)
			))
			(multiple-value-bind (f _k next) (shift-context child nil)
				(declare (ignore _k))
				(is (eql 35 (funcall f next 42)))
				(is (eq t called))
			)
		)
	)
)
