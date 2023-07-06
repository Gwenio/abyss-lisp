
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

; (declaim (optimize (speed 3) (debug 0) (safety 0)))

(uiop:define-package :abyss/viewer
	(:use :cl)
	(:import-from :abyss/print
		:abyss-print
	)
	(:import-from :abyss/ffi/frontend
		:load-source
	)
	(:import-from :abyss/ffi/library
		:load-abyss-ffi
		:add-env-lib-path
	)
	(:export :main)
)
(in-package :abyss/viewer)

(defun main ()
	(add-env-lib-path)
	(load-abyss-ffi)
	(let ((file (first (uiop:command-line-arguments))))
		(if file
			(loop for x in (load-source file)
				do (abyss-print x)
			)
			(progn
				(write-string "No file provided.")
				(fresh-line)
			)
		)
	)
)

; #+sbcl
; (sb-ext:save-lisp-and-die "abyss-view" :toplevel #'main :executable t)
