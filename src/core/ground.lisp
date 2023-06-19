
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

(uiop:define-package :abyss/ground
	(:use :cl)
	(:import-from :abyss/types
		:make-app :+eff-ret+ :+eff-exn+ :+eff-fix+ :make-glyph
	)
	(:import-from :abyss/environment
		:make-environment :env-table
	)
	(:import-from :abyss/helpers
		:bind-params
	)
	(:import-from :abyss/context
		:normal-pass
	)
	(:import-from :abyss/operatives
		:seq-impl :define-impl :vau-impl :lambda-impl :if-impl :cond-impl
		:let-impl :let*-impl :letrec-impl :letrec*-impl
	)
	(:import-from :abyss/applicatives
		:eval-impl :wrap-impl :unwrap-impl :make-env-impl :apply-impl
		:current-env-impl :ignore-p-impl :inert-p-impl :symbol-p-impl
		:oper-p-impl :app-p-impl :comb-p-impl :env-p-impl
	)
	(:import-from :abyss/handlers
		:eff-p-impl :cont-p-impl :handler-p-impl
		:make-eff-impl :make-eff/k-impl :throw-impl :recover-impl
		:resume-impl :resume/h-impl :resume/call-impl :resume/call+h-impl
		:handler-impl :handler/s-impl :with-impl
	)
	(:import-from :abyss/boole
		:not-impl :and-app-impl :or-app-impl :and-oper-impl :or-oper-impl
		:eq-impl :boole-p-impl
	)
	(:import-from :abyss/numbers
		:int-p-impl :rat-p-impl :num-p-impl
		:add-impl :sub-impl :mul-impl :div-impl
		:min-impl :max-impl :mod-impl :rem-impl
		:abs-impl :num-impl :den-impl
		:=impl :<impl :>impl :<=impl :>=impl
	)
	(:import-from :abyss/lists
		:null-p-impl :cons-p-impl :list-p-impl
		:cons-impl :list-impl :list*-impl :list-len-impl
		:first-impl :second-impl :third-impl :nth-impl
		:tail-impl :nth-tail-impl :last-impl :last-n-impl
	)
	(:import-from :abyss/record
		:record-p-impl :record-impl :record-set-impl
	)
	(:export
		:ground-env :extend-ground :extend-ground-app
	)
)
(in-package :abyss/ground)

(declaim (type abyss/environment::environment +ground-env+))

(defvar +ground-env+
	(let* ((env (make-environment nil)) (table (env-table env)))
		(setf (gethash :std-environment table) (make-app
			(lambda (args)
				(bind-params args (nil)
					(normal-pass (make-environment env))
				)
			)))
		(mapcar
			(lambda (x)
				(setf (gethash (make-glyph (first x)) table)
					(second x))
			)
			(list
				; operatives
				(list "$seq" #'seq-impl)
				(list "$define!" #'define-impl)
				(list "$vau" #'vau-impl)
				(list "$lambda" #'lambda-impl)
				(list "$if" #'if-impl)
				(list "$cond" #'cond-impl)
				(list "$let" #'let-impl)
				(list "$let*" #'let*-impl)
				(list "$letrec" #'letrec-impl)
				(list "$letrec*" #'letrec*-impl)
				; applicatives
				(list "eval" (make-app #'eval-impl))
				(list "wrap" (make-app #'wrap-impl))
				(list "unwrap" (make-app #'unwrap-impl))
				(list "make-environment" (make-app #'make-env-impl))
				(list "apply" (make-app #'apply-impl))
				(list "current-environment" (make-app #'current-env-impl))
				(list "ignore?" (make-app #'ignore-p-impl))
				(list "inert?" (make-app #'inert-p-impl))
				(list "symbol?" (make-app #'symbol-p-impl))
				(list "operative?" (make-app #'oper-p-impl))
				(list "applicative?" (make-app #'app-p-impl))
				(list "combiner?" (make-app #'comb-p-impl))
				(list "environment?" (make-app #'env-p-impl))
				; handlers
				(list "effect?" (make-app #'eff-p-impl))
				(list "continuation?" (make-app #'cont-p-impl))
				(list "handler?" (make-app #'handler-p-impl))
				(list "make-effect" (make-app #'make-eff/k-impl))
				(list "make-abortive-effect" (make-app #'make-eff-impl))
				(list "throw" (make-app #'throw-impl))
				(list "recover" (make-app #'recover-impl))
				(list "resume" (make-app #'resume-impl))
				(list "resume/h" (make-app #'resume/h-impl))
				(list "$resume" #'resume/call-impl)
				(list "$resume/h" #'resume/call+h-impl)
				(list "$handler" #'handler-impl)
				(list "$handler/state" #'handler/s-impl)
				(list "$with" #'with-impl)
				(list "ret" +eff-ret+)
				(list "exn" +eff-exn+)
				(list "fix" +eff-fix+)
				; boole
				(list "boole?" (make-app #'boole-p-impl))
				(list "not?" (make-app #'not-impl))
				(list "and?" (make-app #'and-app-impl))
				(list "or?" (make-app #'or-app-impl))
				(list "eq?" (make-app #'eq-impl))
				(list "$and?" #'and-oper-impl)
				(list "$or?" #'or-oper-impl)
				; numbers
				(list "integer?" (make-app #'int-p-impl))
				(list "rational?" (make-app #'rat-p-impl))
				(list "number?" (make-app #'num-p-impl))
				(list "+" (make-app #'add-impl))
				(list "-" (make-app #'sub-impl))
				(list "*" (make-app #'mul-impl))
				(list "/" (make-app #'div-impl))
				(list "min" (make-app #'min-impl))
				(list "max" (make-app #'max-impl))
				(list "mod" (make-app #'mod-impl))
				(list "rem" (make-app #'rem-impl))
				(list "numerator" (make-app #'num-impl))
				(list "denominator" (make-app #'den-impl))
				(list "=?" (make-app #'=impl))
				(list "<?" (make-app #'<impl))
				(list ">?" (make-app #'>impl))
				(list "<=?" (make-app #'<=impl))
				(list ">=?" (make-app #'>=impl))
				; lists
				(list "null?" (make-app #'null-p-impl))
				(list "cons?" (make-app #'cons-p-impl))
				(list "list?" (make-app #'list-p-impl))
				(list "cons" (make-app #'cons-impl))
				(list "list" (make-app #'list-impl))
				(list "list*" (make-app #'list*-impl))
				(list "list-len" (make-app #'list-len-impl))
				(list "first" (make-app #'first-impl))
				(list "second" (make-app #'second-impl))
				(list "third" (make-app #'third-impl))
				(list "nth" (make-app #'nth-impl))
				(list "tail" (make-app #'tail-impl))
				(list "nth-tail" (make-app #'nth-tail-impl))
				(list "last" (make-app #'last-impl))
				(list "last-n" (make-app #'last-n-impl))
				; records
				(list "record?" (make-app #'record-p-impl))
				(list "$record" #'record-impl)
				(list "$record!" #'record-set-impl)
			))
		env
	)
)

(declaim (ftype (function () abyss/environment::environment) ground-env))

(defun ground-env ()
	(make-environment +ground-env+)
)

(declaim (ftype (function (keyword t) t) extend-ground))

(defun extend-ground (key x)
	(shiftf (gethash key (env-table +ground-env+)) x)
)

(declaim (ftype (function (keyword function) t) extend-ground-app))

(defun extend-ground-app (key x)
	(shiftf (gethash key (env-table +ground-env+))
		(make-app x))
)
