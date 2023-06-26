
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
	(:export :+inert+ :+ignore+ :+true+ :+false+
		:boole-type-p :inert-p :ignore-p :applicative-p
		:make-effect :effect-p :effect-name :effect-resumable
		:make-app :app-comb :applicative
		:make-record :record-p :record-obj :record-subtype
		:make-glyph :glyph-p
		:+eff-exn+ :+eff-fix+ :+eff-ret+ :+eff-init+
		:make-type-id :type-id-p :tid-name
		:+tid-type-id+ :+tid-null+ :+tid-inert+ :+tid-ignore+ :+tid-boole+
		:+tid-cons+ :+tid-symbol+ :+tid-environment+ :+tid-continuation+
		:+tid-operative+ :+tid-applicative+ :+tid-effect+ :+tid-handler+
		:+tid-record+ :+tid-string+ :+tid-integer+ :+tid-ratio+
	)
)
(in-package :abyss/types)

(defstruct (inert-type
		(:predicate inert-p)
	)
)

(defstruct (ignore-type
		(:predicate ignore-p)
	)
)

(defstruct (boole-type
		(:constructor make-boole-type (x))
	)
	(x nil :read-only t)
)

(defstruct (applicative
		(:conc-name app-)
		(:constructor make-app (comb))
	)
	(comb (error "`applicative` requires a combiner.")
		:read-only t
		:type (or applicative function)
	)
)

(defstruct (effect
		(:constructor make-effect (name resumable))
	)
	(name t :read-only t)
	(resumable t :read-only t)
)

(defstruct (glyph
		(:constructor make-glyph-aux (str))
	)
	(str (error "glyph requires `str`")
		:type simple-string
		:read-only t
	)
)

(defstruct (type-id
		(:constructor make-tid-aux (name))
		(:conc-name tid-)
	)
	(name (error "type-id requires `name`")
		:type glyph
		:read-only t
	)
)

(defstruct (record
		(:constructor make-record (subtype))
	)
	(obj (make-hash-table :test 'eq)
		:type hash-table
		:read-only t
	)
	(subtype (error "record requires `subtype`")
		:type type-id
		:read-only t
	)
)

(declaim (type hash-table +glyph-table+))

(defvar +glyph-table+ (make-hash-table :test 'equal))

(declaim (ftype (function (simple-string) glyph) make-glyph))

(defun make-glyph (str)
	(or (gethash str +glyph-table+)
		(setf (gethash str +glyph-table+) (make-glyph-aux str)))
)

(declaim (type inert-type +inert+))
(defvar +inert+ (make-inert-type))

(declaim (type ignore-type +ignore+))
(defvar +ignore+ (make-ignore-type))

(declaim (type boole-type +true+ +false+))
(defvar +true+ (make-boole-type t))
(defvar +false+ (make-boole-type nil))

(declaim (type effect +eff-exn+ +eff-fix+ +eff-ret+ +eff-init+))
; non-resumable errors
(defvar +eff-exn+ (make-effect (make-glyph "exn") nil))
; resumabled errors
(defvar +eff-fix+ (make-effect (make-glyph "fix") t))
; normal returns
(defvar +eff-ret+ (make-effect (make-glyph "ret") nil))
; initialize stateful handler
(defvar +eff-init+ (make-effect (make-glyph "init") t))

(declaim (ftype (function (string) type-id) make-type-id))

(defun make-type-id (name)
	(make-tid-aux (make-glyph name))
)

(declaim (type type-id +tid-type-id+ +tid-null+ +tid-inert+ +tid-ignore+
	+tid-cons+ +tid-boole+ +tid-symbol+ +tid-environment+ +tid-continuation+
	+tid-operative+ +tid-applicative+ +tid-effect+ +tid-handler+ +tid-record+
	+tid-string+ +tid-integer+ +tid-ratio+
))

(defvar +tid-type-id+ (make-type-id "type-id"))
(defvar +tid-null+ (make-type-id "null"))
(defvar +tid-inert+ (make-type-id "inert"))
(defvar +tid-ignore+ (make-type-id "ignore"))
(defvar +tid-cons+ (make-type-id "cons"))
(defvar +tid-boole+ (make-type-id "boole"))
(defvar +tid-symbol+ (make-type-id "symbol"))
(defvar +tid-environment+ (make-type-id "environment"))
(defvar +tid-continuation+ (make-type-id "continuation"))
(defvar +tid-operative+ (make-type-id "operative"))
(defvar +tid-applicative+ (make-type-id "applicative"))
(defvar +tid-effect+ (make-type-id "effect"))
(defvar +tid-handler+ (make-type-id "handler"))
(defvar +tid-record+ (make-type-id "record"))
(defvar +tid-string+ (make-type-id "string"))
(defvar +tid-integer+ (make-type-id "integer"))
(defvar +tid-ratio+ (make-type-id "ratio"))
