
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
		:make-app
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
		(mapcar (lambda (x) (setf (gethash (first x) table) (second x)))
			(list
				; operatives
				`(:$seq ,#'seq-impl)
				`(:$define! ,#'define-impl)
				`(:$vau ,#'vau-impl)
				`(:$lambda ,#'lambda-impl)
				`(:$if ,#'if-impl)
				`(:$cond ,#'cond-impl)
				`(:$let ,#'let-impl)
				`(:$let* ,#'let*-impl)
				`(:$letrec ,#'letrec-impl)
				`(:$letrec* ,#'letrec*-impl)
				; applicatives
				`(:eval ,(make-app #'eval-impl))
				`(:wrap ,(make-app #'wrap-impl))
				`(:unwrap ,(make-app #'unwrap-impl))
				`(:make-environment ,(make-app #'make-env-impl))
				`(:apply ,(make-app #'apply-impl))
				`(:current-environment ,(make-app #'current-env-impl))
				`(:ignore? ,(make-app #'ignore-p-impl))
				`(:inert? ,(make-app #'inert-p-impl))
				`(:symbol? ,(make-app #'symbol-p-impl))
				`(:operative? ,(make-app #'oper-p-impl))
				`(:applicative? ,(make-app #'app-p-impl))
				`(:combiner? ,(make-app #'comb-p-impl))
				`(:environment? ,(make-app #'env-p-impl))
				; handlers
				`(:effect? ,(make-app #'eff-p-impl))
				`(:continuation? ,(make-app #'cont-p-impl))
				`(:handler? ,(make-app #'handler-p-impl))
				`(:make-effect ,(make-app #'make-eff/k-impl))
				`(:make-abortive-effect ,(make-app #'make-eff-impl))
				`(:throw ,(make-app #'throw-impl))
				`(:recover ,(make-app #'recover-impl))
				`(:resume ,(make-app #'resume-impl))
				`(:resume/h ,(make-app #'resume/h-impl))
				`(:$resume ,#'resume/call-impl)
				`(:$resume/h ,#'resume/call+h-impl)
				`(:$handler ,#'handler-impl)
				`(:$handler/state ,#'handler/s-impl)
				`(:$with ,#'with-impl)
				; boole
				`(:boole? ,(make-app #'boole-p-impl))
				`(:not? ,(make-app #'not-impl))
				`(:and? ,(make-app #'and-app-impl))
				`(:or? ,(make-app #'or-app-impl))
				`(:eq? ,(make-app #'eq-impl))
				`(:$and? ,#'and-oper-impl)
				`(:$or? ,#'or-oper-impl)
				; numbers
				`(:integer? ,(make-app #'int-p-impl))
				`(:rational? ,(make-app #'rat-p-impl))
				`(:number? ,(make-app #'num-p-impl))
				`(:+ ,(make-app #'add-impl))
				`(:- ,(make-app #'sub-impl))
				`(:* ,(make-app #'mul-impl))
				`(:/ ,(make-app #'div-impl))
				`(:min ,(make-app #'min-impl))
				`(:max ,(make-app #'max-impl))
				`(:mod ,(make-app #'mod-impl))
				`(:rem ,(make-app #'rem-impl))
				`(:numerator ,(make-app #'num-impl))
				`(:denominator ,(make-app #'den-impl))
				`(:=? ,(make-app #'=impl))
				`(:<? ,(make-app #'<impl))
				`(:>? ,(make-app #'>impl))
				`(:<=? ,(make-app #'<=impl))
				`(:>=? ,(make-app #'>=impl))
				; lists
				`(:null? ,(make-app #'null-p-impl))
				`(:cons? ,(make-app #'cons-p-impl))
				`(:list? ,(make-app #'list-p-impl))
				`(:cons ,(make-app #'cons-impl))
				`(:list ,(make-app #'list-impl))
				`(:list* ,(make-app #'list*-impl))
				`(:list-len ,(make-app #'list-len-impl))
				`(:first ,(make-app #'first-impl))
				`(:second ,(make-app #'second-impl))
				`(:third ,(make-app #'third-impl))
				`(:nth ,(make-app #'nth-impl))
				`(:tail ,(make-app #'tail-impl))
				`(:nth-tail ,(make-app #'nth-tail-impl))
				`(:last ,(make-app #'last-impl))
				`(:last-n ,(make-app #'last-n-impl))
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
