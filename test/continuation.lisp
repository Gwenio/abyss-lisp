
(uiop:define-package :abyss/test/continuation
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/types
		:make-effect :*eff-bad-continuation*
	)
	(:import-from :abyss/context
		:fresh-context :initial-context :push-frame :normal-pass
	)
	(:import-from :abyss/continuation
		:perform-effect :perform-effect/k :resume-cont :resume-cont/h
	)
)
(in-package :abyss/test/continuation)

(def-suite* abyss-continuation-tests :in abyss/test:abyss-tests)

(defun root-handler (_effect)
	(declare (ignore _effect))
	(error "Unhandled effect.")
)

(defvar *eff-dummy* (make-effect))
(defvar *eff-bidir* (make-effect))

(defun test-handler-add (effect)
	(cond
		((eq effect *eff-dummy*)
			#'(lambda (ctx x)
				(resume-cont ctx (+ (car x) 2) (cdr x))
			)
		)
		((null effect) #'normal-pass)
		(t nil)
	)
)

(defun test-handler-bad (effect)
	(cond
		((eq effect *eff-dummy*)
			#'(lambda (ctx x)
				(resume-cont ctx (cdr x) (cdr x))
			)
		)
		((eq effect *eff-bad-continuation*)
			#'(lambda (ctx _x)
				(declare (ignore _x))
				(normal-pass ctx *eff-bad-continuation*)
			)
		)
		((null effect) #'normal-pass)
		(t nil)
	)
)

(defun test-handler-shallow (effect)
	(cond
		((eq effect *eff-dummy*)
			#'(lambda (ctx x)
				(resume-cont/h ctx (/ (car x) 2) (cdr x) #'test-handler-add)
			)
		)
		((null effect) #'normal-pass)
		(t nil)
	)
)

(defun test-handler-bidir (effect)
	(cond
		((eq effect *eff-bidir*)
			#'(lambda (ctx x)
				(perform-effect/k ctx (/ (car x) 2) *eff-dummy* (cdr x))
			)
		)
		((null effect) #'normal-pass)
		(t nil)
	)
)

(test cont-basic
	(let ((initial (initial-context #'root-handler)))
		(push-frame initial #'(lambda (k x) (normal-pass k (- x 7))))
		(let ((child (fresh-context initial #'test-handler-add)))
			(push-frame child #'(lambda (k x) (normal-pass k (* x 7))))
			(is (eql 42 (perform-effect child 5 *eff-dummy*)))
		)
	)
)

(test cont-invalidate
	(let ((initial (initial-context #'root-handler)))
		(let ((child (fresh-context initial #'test-handler-bad)))
			(push-frame child #'(lambda (k x) (resume-cont k t x)))
			(is (eq *eff-bad-continuation*
				(perform-effect child nil *eff-dummy*))
			)
		)
	)
)

(test cont-shallow
	(let ((initial (initial-context #'root-handler)))
		(push-frame initial #'(lambda (k x) (normal-pass k (- x 7))))
		(let ((child (fresh-context initial #'test-handler-shallow)))
			(push-frame child #'(lambda (k x) (normal-pass k (* x 7))))
			(push-frame child #'(lambda (k x) (perform-effect k x *eff-dummy*)))
			(is (eql 42 (perform-effect child 10 *eff-dummy*)))
		)
	)
)

(test cont-bidir
	(let ((initial (initial-context #'root-handler)))
		(push-frame initial #'(lambda (k x) (normal-pass k (- x 7))))
		(let ((outer (fresh-context initial #'test-handler-bidir)))
			(let ((inner (fresh-context outer #'test-handler-add)))
				(push-frame inner #'(lambda (k x) (normal-pass k (* x 7))))
				(is (eql 42 (perform-effect inner 10 *eff-bidir*)))
			)
		)
	)
)
