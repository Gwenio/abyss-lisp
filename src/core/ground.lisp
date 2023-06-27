
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
	(:mix
		:abyss/types
		:abyss/error
		:abyss/operatives
		:abyss/applicatives
		:abyss/handlers
		:abyss/boole
		:abyss/numbers
		:abyss/lists
		:abyss/record
	)
	(:import-from :abyss/helpers
		:bind-params
	)
	(:import-from :abyss/context
		:normal-pass
	)
	(:import-from :abyss/modules
		:export-impl
		:import-impl
	)
	(:export
		:ground-env :extend-ground :extend-ground-app
	)
)
(in-package :abyss/ground)

(declaim (ftype (function ((cons abyss/types::environment t)) t)
	type-of-impl))

(defun type-of-impl (args)
	(bind-params args (nil x)
		(normal-pass (typecase x
			(null +tid-null+)
			(cons +tid-cons+)
			(function +tid-operative+)
			(integer +tid-integer+)
			(ratio +tid-ratio+)
			(string +tid-string+)
			(abyss/types::inert-type +tid-inert+)
			(abyss/types::ignore-type +tid-ignore+)
			(abyss/types::boole-type +tid-boole+)
			(abyss/types::glyph +tid-symbol+)
			(abyss/types::environment +tid-environment+)
			(abyss/types::applicative +tid-applicative+)
			(abyss/context::continuation +tid-continuation+)
			(abyss/types::effect +tid-effect+)
			(abyss/handlers::handler-type +tid-handler+)
			(abyss/types::record (record-subtype x))
			(abyss/types::type-id +tid-type-id+)
			(t (print (type-of x)) (error "Unknown type"))
		))
	)
)

(declaim (type abyss/types::environment +ground-env+))

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
				; type-id
				(list "type-of" (make-app #'type-of-impl))
				; operatives
				(list "$do" #'seq-impl)
				(list "$define!" #'define-impl)
				(list "$vau" #'vau-impl)
				(list "$lambda" #'lambda-impl)
				(list "$if" #'if-impl)
				(list "$cond" #'cond-impl)
				(list "$let" #'let-impl)
				(list "$let*" #'let*-impl)
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
				(list "type-id?" (make-app #'type-id-p-impl))
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
				(list "list1" (make-app #'cons-impl))
				(list "list0" (make-app #'list-impl))
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
				(list "$record-subtype" #'record-subtype-impl)
				(list "$record!" #'record-set-impl)
				(list "make-type-exn" (make-app #'make-type-exn-impl))
				(list "make-bounds-exn" (make-app #'make-bounds-exn-impl))
				(list "sym-not-found?" (make-app #'sym-not-found-p-impl))
				(list "invalid-combiner?" (make-app #'invalid-comb-p-impl))
				(list "improper-list?" (make-app #'improper-list-p-impl))
				(list "match-param?" (make-app #'match-param-p-impl))
				(list "match-cons?" (make-app #'match-cons-p-impl))
				(list "match-null?" (make-app #'match-null-p-impl))
				(list "match-repeat?" (make-app #'match-repeat-p-impl))
				(list "bad-continuation?" (make-app #'bad-cont-p-impl))
				(list "bad-handler-case?" (make-app #'bad-handler-p-impl))
				(list "type-exn?" (make-app #'type-exn-p-impl))
				(list "div-by-zero?" (make-app #'div-zero-p-impl))
				(list "bounds-exn?" (make-app #'bounds-exn-p-impl))
				(list "repeat-export?" (make-app #'export-exn-p-impl))
				; modules
				(list "$export" #'export-impl)
				(list "$import" #'import-impl)
			))
		(mapcar (lambda (x) (setf (gethash (tid-name x) table) x))
			(list
				+tid-type-id+ +tid-null+ +tid-inert+ +tid-ignore+ +tid-cons+
				+tid-boole+ +tid-symbol+ +tid-environment+ +tid-continuation+
				+tid-operative+ +tid-applicative+ +tid-effect+ +tid-handler+
				+tid-record+ +tid-string+ +tid-integer+ +tid-ratio+
				+tid-sym-not-found+ +tid-invalid-comb+ +tid-improper-list+
				+tid-match-param+ +tid-match-repeat+ +tid-match-null+
				+tid-match-cons+ +tid-bad-cont+ +tid-bad-handler+
				+tid-type-exn+ +tid-div-zero+ +tid-bounds-exn+ +tid-export-exn+
				))
		env
	)
)

(declaim (ftype (function () abyss/types::environment) ground-env))

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
