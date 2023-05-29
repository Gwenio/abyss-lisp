
(uiop:define-package :abyss/test/continuation
	(:use :cl)
	(:mix :fiveam)
	(:import-from :abyss/types
		:make-effect :+eff-exn+ :+eff-ret+
	)
	(:import-from :abyss/error
		:bad-cont-p
	)
	(:import-from :abyss/context
		:fresh-context :initial-context :push-frame :normal-pass
		:perform-effect :resume-cont :resume-cont/h :resume-cont/call
	)
)
(in-package :abyss/test/continuation)

(def-suite* abyss-continuation-tests :in abyss/test:abyss-tests)

(defun root-handler (eff)
	(cond
		((eq eff +eff-ret+) #'identity)
		((eq eff +eff-exn+) #'normal-pass)
		(t (print eff)
			(error "Unhandled effect.")
		)
	)
)

(defvar +eff-dummy+ (make-effect 'dummy))
(defvar +eff-bidir+ (make-effect 'bidir))

(defun test-handler-add (effect)
	(cond
		((eq effect +eff-dummy+)
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
		((eq effect +eff-dummy+)
			#'(lambda (x)
				(resume-cont (cdr x) (cdr x))
			)
		)
		((eq effect +eff-exn+)
			(lambda (x) (normal-pass (car x)))
		)
		((null effect) #'normal-pass)
		(t nil)
	)
)

(defun test-handler-shallow (effect)
	(cond
		((eq effect +eff-dummy+)
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
		((eq effect +eff-bidir+)
			#'(lambda (x)
				(resume-cont/call
					(lambda ()
						(perform-effect (/ (car x) 2) +eff-dummy+)
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
				(is (eql 42 (perform-effect 5 +eff-dummy+)))
			)
			#'test-handler-add)
	)
)

(test cont-invalidate
	(run-cont-case
		(fresh-context
			(lambda ()
				(push-frame #'(lambda (x) (resume-cont t x)))
				(is (bad-cont-p
					(perform-effect nil +eff-dummy+))
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
				(push-frame #'(lambda (x) (perform-effect x +eff-dummy+)))
				(is (eql 42 (perform-effect 10 +eff-dummy+)))
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
						(is (eql 42 (perform-effect 10 +eff-bidir+)))
					)
					#'test-handler-add)
			)
			#'test-handler-bidir)
	)
)
