
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

(uiop:define-package :abyss/error
	(:use :cl)
	(:export
		:sym-not-found-exn :sym-not-found-p :make-sym-not-found
		:sym-not-found-sym :sym-not-found-env
		:invalid-comb-exn :invalid-comb-p :make-invalid-comb
		:improper-list-exn :improper-list-exn-p :make-improper-list
		:bad-param-exn :bad-param-p :make-bad-param
		:arg-pair-exn :arg-pair-p :make-arg-pair
		:arg-null-exn :arg-null-p :make-arg-null
		:arg-repeat-exn :arg-repeat-p :make-arg-repeat
		:bad-cont-exn :bad-cont-p :make-bad-cont
		:type-exn :type-exn-p :make-type-exn :type-exn-obj :type-exn-expect
	)
)
(in-package :abyss/error)

(defstruct
	(sym-not-found-exn
		(:constructor make-sym-not-found (sym env))
		(:conc-name sym-not-found-)
		(:predicate sym-not-found-p)
	)
	(sym)
	(env)
)

(defstruct
	(invalid-comb-exn
		(:constructor make-invalid-comb (obj))
		(:conc-name invalid-comb-)
		(:predicate invalid-comb-p)
	)
	(obj)
)
(defstruct
	(improper-list-exn
		(:constructor make-improper-list (obj))
		(:conc-name improper-list-)
		(:predicate improper-list-p)
	)
	(obj)
)

(defstruct
	(bad-param-exn
		(:constructor make-bad-param (obj))
		(:conc-name bad-param-)
		(:predicate bad-param-p)
	)
	(obj)
)

(defstruct
	(arg-pair-exn
		(:constructor make-arg-pair (obj))
		(:conc-name arg-pair-)
		(:predicate arg-pair-p)
	)
	(obj)
)

(defstruct
	(arg-null-exn
		(:constructor make-arg-null (obj))
		(:conc-name arg-null-)
		(:predicate arg-null-p)
	)
	(obj)
)

(defstruct
	(arg-repeat-exn
		(:constructor make-arg-repeat (obj))
		(:conc-name arg-repeat-)
		(:predicate arg-repeat-p)
	)
	(obj)
)

(defstruct
	(bad-cont-exn
		(:constructor make-bad-cont (obj))
		(:conc-name bad-cont-)
		(:predicate bad-cont-p)
	)
	(obj)
)

(defstruct
	(type-exn
		(:constructor make-type-exn (obj expect))
		(:conc-name type-exn-)
		(:predicate type-exn-p)
	)
	(obj)
	(expect)
)
