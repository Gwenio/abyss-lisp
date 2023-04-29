
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
		:perform-effect :resume-cont :resume-cont/h :resume-cont/call
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
			#'(lambda (x)
				(resume-cont (+ (car x) 2) (cdr x))
			)
		)
		((null effect) #'normal-pass)
		(t nil)
	)
)

(defun test-handler-bad (effect)
	(cond
		((eq effect *eff-dummy*)
			#'(lambda (x)
				(resume-cont (cdr x) (cdr x))
			)
		)
		((eq effect *eff-bad-continuation*)
			#'(lambda (_x)
				(declare (ignore _x))
				(normal-pass *eff-bad-continuation*)
			)
		)
		((null effect) #'normal-pass)
		(t nil)
	)
)

(defun test-handler-shallow (effect)
	(cond
		((eq effect *eff-dummy*)
			#'(lambda (x)
				(resume-cont/h (/ (car x) 2) (cdr x) #'test-handler-add)
			)
		)
		((null effect) #'normal-pass)
		(t nil)
	)
)

(defun test-handler-bidir (effect)
	(cond
		((eq effect *eff-bidir*)
			#'(lambda (x)
				(resume-cont/call
					(lambda ()
						(perform-effect (/ (car x) 2) *eff-dummy*)
					)
					(cdr x))
			)
		)
		((null effect) #'normal-pass)
		(t nil)
	)
)

(defmacro run-cont-case (&rest x)
	`(initial-context (lambda () ,@x) #'root-handler)
)

(test cont-basic
	(run-cont-case
		(push-frame #'(lambda (x) (normal-pass (- x 7))))
		(fresh-context
			(lambda ()
				(push-frame #'(lambda (x) (normal-pass (* x 7))))
				(is (eql 42 (perform-effect 5 *eff-dummy*)))
			)
			#'test-handler-add)
	)
)

(test cont-invalidate
	(run-cont-case
		(fresh-context
			(lambda ()
				(push-frame #'(lambda (x) (resume-cont t x)))
				(is (eq *eff-bad-continuation*
					(perform-effect nil *eff-dummy*))
				)
			)
		#'test-handler-bad)
	)
)

(test cont-shallow
	(run-cont-case
		(push-frame #'(lambda (x) (normal-pass (- x 7))))
		(fresh-context
			(lambda ()
				(push-frame #'(lambda (x) (normal-pass (* x 7))))
				(push-frame #'(lambda (x) (perform-effect x *eff-dummy*)))
				(is (eql 42 (perform-effect 10 *eff-dummy*)))
			)
			#'test-handler-shallow)
	)
)

(test cont-bidir
	(run-cont-case
		(push-frame #'(lambda (x) (normal-pass (- x 7))))
		(fresh-context
			(lambda ()
				(fresh-context
					(lambda ()
						(push-frame #'(lambda (x) (normal-pass (* x 7))))
						(is (eql 42 (perform-effect 10 *eff-bidir*)))
					)
					#'test-handler-add)
			)
			#'test-handler-bidir)
	)
)
