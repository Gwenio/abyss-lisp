
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
		:make-glyph :glyph-p :glyph-str
		:make-environment :environment-p :env-table :env-parent
		:+eff-exn+ :+eff-fix+ :+eff-ret+ :+eff-init+
		:make-type-id :type-id-p :tid-name :tid-pred
		:+tid-type-id+ :+tid-null+ :+tid-inert+ :+tid-ignore+ :+tid-boole+
		:+tid-cons+ :+tid-symbol+ :+tid-environment+ :+tid-list+
		:+tid-operative+ :+tid-applicative+ :+tid-effect+
		:+tid-record+ :+tid-string+
		:+tid-integer+ :+tid-ratio+ :+tid-rational+ :+tid-number+
		:def-simple-type-id
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

(defstruct (environment
		(:constructor make-environment (parent))
		(:conc-name env-)
		(:predicate environment-p)
	)
	"Symbol binding environment."
	(table (make-hash-table :test 'eq)
		:type hash-table
		:read-only t
	)
	(parent nil
		:read-only t
		:type (or environment null)
	)
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
		(:constructor make-type-id (name pred))
		(:conc-name tid-)
		(:predicate type-id-p)
	)
	(name (error "type-id requires `name`")
		:type glyph
		:read-only t
	)
	(pred (error "type-id requires `pred`")
		:type (function (t type-id) t)
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

(declaim (ftype (function ((function (t) t)) (function (t type-id) t))
	simple-type-p))

(defun simple-type-p (pred)
	(lambda (x _tid)
		(declare (ignore _tid))
		(funcall pred x)
	)
)

(defmacro def-simple-type-id (var name pred)
	`(defvar ,var (make-type-id (make-glyph ,name) (simple-type-p #',pred)))
)

(declaim (type type-id +tid-type-id+ +tid-null+ +tid-inert+ +tid-ignore+
	+tid-cons+ +tid-boole+ +tid-symbol+ +tid-environment+
	+tid-operative+ +tid-applicative+ +tid-effect+ +tid-record+
	+tid-string+ +tid-integer+ +tid-ratio+
))

(def-simple-type-id +tid-type-id+ "type-id" type-id-p)
(def-simple-type-id +tid-null+ "null" null)
(def-simple-type-id +tid-inert+ "inert" inert-p)
(def-simple-type-id +tid-ignore+ "ignore" ignore-p)
(def-simple-type-id +tid-cons+ "cons" consp)
(def-simple-type-id +tid-list+ "list" listp)
(def-simple-type-id +tid-boole+ "boole" boole-type-p)
(def-simple-type-id +tid-symbol+ "symbol" symbolp)
(def-simple-type-id +tid-environment+ "environment" environment-p)
(def-simple-type-id +tid-operative+ "operative" functionp)
(def-simple-type-id +tid-applicative+ "applicative" applicative-p)
(def-simple-type-id +tid-effect+ "effect" effect-p)
(def-simple-type-id +tid-record+ "record" record-p)
(def-simple-type-id +tid-string+ "string" stringp)
(def-simple-type-id +tid-integer+ "integer" integerp)
(def-simple-type-id +tid-rational+ "rational" rationalp)
(def-simple-type-id +tid-number+ "number" realp)
(defvar +tid-ratio+ (make-type-id
	(make-glyph "ratio")
	(lambda (x _tid) (declare (ignore _tid)) (typep 'ratio x))))
