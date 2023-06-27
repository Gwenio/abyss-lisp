
; ISC License (ISC)
;
; Copyright 2023 James Adam Armstrong
;
; Permission to use, copy, modify, and/or distribute this software for any
; purpose with or without fee is hereby granted, provided that the above copyright
; notice and this permission notice appear in all copies.
;
; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
; REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
; FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
; INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
; OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
; PERFORMANCE OF THIS SOFTWARE.

(uiop:define-package :abyss/modules
	(:use :cl)
	(:mix :abyss/types :abyss/error)
	(:import-from :abyss/context
		:normal-pass :push-frame :fresh-context :resume-cont :resume-cont/call
		:throw-exn :recover-exn :perform-effect/k
	)
	(:import-from :abyss/evaluate
		:evaluate
	)
	(:import-from :abyss/helpers
		:bad-tail :bind-params :boole-branch :destructure
	)
	(:export
		:import-impl :export-impl :do-module-part
	)
)
(in-package :abyss/modules)

(declaim (type abyss/types::environment +module-env+))

(defvar +eff-import+ (make-effect (make-glyph "import") t))

(defvar +eff-export+ (make-effect (make-glyph "export") t))

(declaim (ftype (function ((cons abyss/types::environment t)) t)
	import-impl export-impl))

(defun import-impl (args)
	(bind-params args (nil . spec)
		(if (glyph-p (first spec))
			(perform-effect/k spec +eff-import+)
			(throw-exn (make-type-exn (first spec) +tid-symbol+))
		)
	)
)

(declaim (ftype (function (t) (function (cons) t)) export-aux))

(defun export-aux (bindings)
	(lambda (args)
		(perform-effect/k (list bindings (cdr args)) +eff-export+)
	)
)

(defun export-impl (args)
	(bind-params args (env . exps)
		(loop for x on exps
			while (consp x)
			if (glyph-p (first x))
			collect (first x) into y
			and collect (first x) into z
			else unless (consp (first x))
			do (return (throw-exn (make-match-cons (first x))))
			else unless (consp (cdr (first x)))
			do (return (throw-exn (make-match-cons (cdr (first x)))))
			else unless (null (cddr (first x)))
			do (return (throw-exn (make-match-null (cddr (first x)))))
			else
			collect (first (first x)) into y
			and collect (second (first x)) into z
			end
			finally (return (progn
				(when x (push-frame (bad-tail x)))
				(evaluate (cons (make-app (export-aux y)) z) env)
			))
		)
	)
)

(declaim (ftype (function (list)
	(function ((cons (cons abyss/types::glyph list) abyss/context::continuation)) t))
	import-handler))

(defun import-handler (import-list)
	(lambda (args)
		(let ((from (first (first args))) (k (cdr args)))
			(loop for x on (cdr (first args))
				with table hash-table = (cdr (assoc from import-list))
				while (consp x)
				if (glyph-p (first x))
				collect (gethash (first x) table) into y
				else
				do (return (resume-cont/call (lambda ()
					(throw-exn (make-type-exn (first x) +tid-symbol+))
				) k))
				end
				finally (return (if x
					(resume-cont/call (lambda ()
						(recover-exn (make-improper-list x y))
					) k)
					(resume-cont y k)
				))
			)
		)
	)
)

(declaim (ftype
	(function (hash-table)
		(function ((cons (cons list (cons list nil))
			abyss/context::continuation)) t))
	export-handler))

(defun export-handler (table)
	(lambda (args)
		(let ((k (cdr args)))
			(loop for bind in (first (first args))
				for val in (second (first args))
				do (multiple-value-bind (old found) (gethash bind table)
					(if found
						(return (resume-cont/call (lambda ()
							(throw-exn (make-export-exn bind old val))
						) k))
						(setf (gethash bind table) val)
					)
				)
				finally (return (resume-cont +inert+ k))
			)
		)
	)
)

(declaim (ftype (function
	((function((cons (cons abyss/types::glyph list) abyss/context::continuation)) t))
		(function (abyss/types::effect)
			(or null (function (t) t))))
	module-handler))

(defun module-handler (im-handler)
	(let* (
			(export-table (make-hash-table :test 'eq))
			(ex-handler (export-handler export-table))
		)
		(lambda (eff)
			(cond
				((eq eff +eff-import+) im-handler)
				((eq eff +eff-export+) ex-handler)
				((eq eff +eff-ret+) (lambda (_)
					(declare (ignore _))
					(normal-pass export-table)
				))
				(t nil)
			)
		)
	)
)

(declaim (ftype (function (abyss/types::environment list cons) t)
	do-module-part))

(defun do-module-part (env import-list code)
	(fresh-context
		(lambda () (funcall (abyss/operatives::seq-aux env code) nil))
		(module-handler (import-handler import-list)))
)
