
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

(uiop:define-package :abyss/print
	(:use :cl)
	(:mix :abyss/types)
	(:import-from :abyss/context
		:normal-pass
	)
	(:import-from :abyss/helpers
		:bind-params
	)
	(:import-from :abyss/handlers) ; for load ordering
	(:export :print-impl :newline-impl :abyss-print)
)
(in-package :abyss/print)

(defvar *abyss-print* nil)

(defmethod print-object ((x abyss/types::glyph) stream)
	(write-string (glyph-str x) stream)
	x
)

(defmethod print-object ((x abyss/types::boole-type) stream)
	(write-string (if (abyss/types::boole-type-x x)
		"#true"
		"#false"
	) stream)
	x
)

(defmethod print-object ((x abyss/types::inert-type) stream)
	(write-string "#inert" stream)
	x
)

(defmethod print-object ((x abyss/types::ignore-type) stream)
	(write-string "#ignore" stream)
	x
)

(defmethod print-object ((x null) stream)
	(if *abyss-print*
		(progn
			(write-string "#null" stream)
			x
		)
		(call-next-method nil stream)
	)
)

(defmethod print-object ((x abyss/types::record) stream)
	(write-string "#record" stream)
	(write (cons (record-subtype x)
		(loop for key being each hash-key in (record-obj x) using (hash-value val)
			collect (list key val) into y
			finally (return y)
		)) :stream stream)
	x
)

(defmethod print-object ((x abyss/types::type-id) stream)
	(write-string "#type-id:" stream)
	(write-string (glyph-str (tid-name x)) stream)
	x
)

(defmethod print-object ((x abyss/types::effect) stream)
	(write-string "#effect" stream)
	(write (list (effect-name x)) :stream stream)
	x
)

(defmethod print-object ((x abyss/types::environment) stream)
	(write-string "#environment" stream)
	x
)

(defmethod print-object ((x function) stream)
	(if *abyss-print*
		(progn
			(write-string "#operative" stream)
			x
		)
		(call-next-method nil stream)
	)
)

(defmethod print-object ((x abyss/types::applicative) stream)
	(write-string "#applicative" stream)
	x
)

(defmethod print-object ((x abyss/context::continuation) stream)
	(write-string "#continuation" stream)
	x
)

(defmethod print-object ((x abyss/handlers::handler-type) stream)
	(write-string "#handler" stream)
	x
)

(defun print-impl (args)
	(bind-params args (nil x)
		(normal-pass (prog1
			(let ((*abyss-print* t)) (print x))
			(fresh-line)
		))
	)
)

(defun newline-impl (args)
	(bind-params args (nil)
		(terpri)
		(normal-pass +inert+)
	)
)

(defun abyss-print (x &optional stream)
	(let ((*abyss-print* t))
		(print x stream)
	)
)
