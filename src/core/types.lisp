
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
		:make-record :record-p :record-obj
		:make-glyph :glyph-p
		:+eff-exn+ :+eff-fix+ :+eff-ret+ :+eff-init+
	)
)
(in-package :abyss/types)

(defstruct
	(inert-type
		(:predicate inert-p)
	)
)

(defstruct
	(ignore-type
		(:predicate ignore-p)
	)
)

(defstruct
	(boole-type
		(:constructor make-boole-type (x))
	)
	(x nil :read-only t)
)

(defstruct
	(applicative
		(:conc-name app-)
		(:constructor make-app (comb))
	)
	(comb (error "`applicative` requires a combiner.")
		:read-only t
		:type (or applicative function)
	)
)

(defstruct
	(effect
		(:constructor make-effect (name resumable))
	)
	(name t :read-only t)
	(resumable t :read-only t)
)

(defstruct
	(record
		(:constructor make-record ())
	)
	(obj (make-hash-table :test 'eq)
		:type hash-table
		:read-only t
	)
)

(defstruct
	(glyph
		(:constructor make-glyph-aux (str))
	)
	(str (error "glyph requires `str`")
		:type simple-string
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

(defvar +inert+ (make-inert-type))
(defvar +ignore+ (make-ignore-type))
(defvar +true+ (make-boole-type t))
(defvar +false+ (make-boole-type nil))

; non-resumable errors
(defvar +eff-exn+ (make-effect (make-glyph "exn") nil))
; resumabled errors
(defvar +eff-fix+ (make-effect (make-glyph "fix") t))
; normal returns
(defvar +eff-ret+ (make-effect (make-glyph "ret") nil))
; initialize stateful handler
(defvar +eff-init+ (make-effect (make-glyph "init") t))
