
; ISC License (ISC)
;
; Copyright 2022-2023 James Adam Armstrong
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

(uiop:define-package :abyss/types
	(:use :cl)
	(:export :+inert+ :+ignore+ :+true+ :+false+ :boole-type
		:boole-type-p :inert-p :ignore-p :applicative-p :effect-p
		:make-app :app-comb :make-effect :applicative
		:+eff-exn+
	)
)
(in-package :abyss/types)

(defstruct literal-type)
(defstruct boole-type)
(defstruct
	(applicative
		(:conc-name app-)
		(:constructor make-app (comb))
	)
	(comb (error "`applicative` requires a combiner.")
		:read-only t
	)
)
(defstruct effect)

(defvar +inert+ (make-literal-type))
(defvar +ignore+ (make-literal-type))
(defvar +true+ (make-boole-type))
(defvar +false+ (make-boole-type))

(defun inert-p (x) (eq +inert+ x))

(defun ignore-p (x) (eq +ignore+ x))

(defvar +eff-exn+ (make-effect))
