
(uiop:define-package :abyss/test/continuation
	(:use :cl)
	(:mix :fiveam :abyss/types :abyss/error)
	(:import-from :abyss/context
		:fresh-context :initial-context :push-frame :normal-pass
		:perform-effect :perform-effect/k
		:set-handler :resume-cont :resume-cont/call
	)
)
(in-package :abyss/test/continuation)

(def-suite* abyss-continuation-tests :in abyss/test:abyss-tests)

(defun root-handler (eff)
	(cond
		((eq eff +eff-ret+) #'identity)
		((eq eff +eff-exn+) #'identity)
		((eq eff +eff-fix+) #'first)
		(t (print eff)
			(error "Unhandled effect.")
		)
	)
)

(defvar +eff-dummy+ (make-effect 'dummy t))
(defvar +eff-bidir+ (make-effect 'bidir t))

(defun test-handler-add (effect)
	(cond
		((eq effect +eff-dummy+)
			#'(lambda (x)
				(resume-cont (+ (car x) 2) (cdr x))
			)
		)
		((eq effect +eff-ret+) #'normal-pass)
		(t nil)
	)
)

(defun test-handler-bad (effect)
	(cond
		((eq effect +eff-dummy+)
			(lambda (x)
				(funcall (first x) (cdr x))
			)
		)
		((eq effect +eff-exn+) #'normal-pass)
		((eq effect +eff-ret+) #'normal-pass)
		(t nil)
	)
)

(defun test-handler-shallow (effect)
	(cond
		((eq effect +eff-dummy+)
			#'(lambda (x) (let ((y (first x)) (k (cdr x)))
				(set-handler k #'test-handler-add)
				(resume-cont (/ y 2) k)
			))
		)
		((eq effect +eff-ret+) #'normal-pass)
		(t nil)
	)
)

(defun test-handler-bidir (effect)
	(cond
		((eq effect +eff-bidir+)
			#'(lambda (x)
				(resume-cont/call
					(lambda ()
						(perform-effect/k (/ (car x) 2) +eff-dummy+)
					)
					(cdr x))
			)
		)
		((eq effect +eff-ret+) #'normal-pass)
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
				(is (eql 42 (perform-effect/k 5 +eff-dummy+)))
			)
			#'test-handler-add)
	)
)

(test cont-invalidate
	; invalidate on resume
	(run-cont-case
		(fresh-context
			(lambda ()
				(push-frame #'(lambda (x) (resume-cont t x)))
				(is (exn-type-p
					(perform-effect/k
						(lambda (k)
							(resume-cont k k)
						) +eff-dummy+)
					+tid-bad-cont+)
				)
			)
			#'test-handler-bad)
	)
	; invalidate on exiting the context
	(run-cont-case
		(push-frame #'(lambda (x) (resume-cont t x)))
		(fresh-context
			(lambda ()
				(is (exn-type-p
					(perform-effect/k
						(lambda (k)
							(normal-pass k)
						) +eff-dummy+)
					+tid-bad-cont+)
				)
			)
			#'test-handler-bad)
	)
	(let ((called nil))
		; check invalidation does not screw up guards added after it
		(run-cont-case
			(fresh-context
				(lambda ()
					(push-frame #'(lambda (x)
						(resume-cont t x)
					))
					(perform-effect/k
						(lambda (k)
							(abyss/context:final-guard (lambda (y)
								(setf called t)
								(normal-pass y)
							))
							(resume-cont k k)
						) +eff-dummy+)
				)
				#'test-handler-bad)
		)
		(is (eq called t))
	)
)

(test cont-shallow
	(run-cont-case
		(push-frame #'(lambda (x) (normal-pass (- x 7))))
		(fresh-context
			(lambda ()
				(push-frame #'(lambda (x) (normal-pass (* x 7))))
				(push-frame #'(lambda (x) (perform-effect/k x +eff-dummy+)))
				(is (eql 42 (perform-effect/k 10 +eff-dummy+)))
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
						(is (eql 42 (perform-effect/k 10 +eff-bidir+)))
					)
					#'test-handler-add)
			)
			#'test-handler-bidir)
	)
)
