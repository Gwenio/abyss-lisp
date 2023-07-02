
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

(uiop:define-package :abyss/ffi/library
	(:use :cl)
	(:export
		:load-abyss-ffi
		:add-env-lib-path
		:add-search-path
	)
)
(cl:in-package :abyss/ffi/library)

(cffi:define-foreign-library abyss-ffi
	(:darwin (:or (:framework "abyss") (:default "abyss")))
	(:unix (:or "libabyss.so" "libabyss"))
	(:windows "abyss.dll")
	(t (:default "abyss"))
)

(defun add-env-lib-path ()
	(when (uiop:getenvp "ABYSS_LIB_PATH")
		(pushnew
			(uiop:getenv-pathname "ABYSS_LIB_PATH" :defaults (uiop:getcwd)
				:ensure-directory t :ensure-absolute t)
			cffi:*foreign-library-directories* :test 'equal)
	)
)

(add-env-lib-path)

(defun add-search-path (path)
	(pushnew
		(uiop:ensure-pathname path :defaults (uiop:getcwd)
			:ensure-directory t :ensure-absolute t)
		cffi:*foreign-library-directories* :test 'equal)
)

(defun load-abyss-ffi ()
	(cffi:use-foreign-library abyss-ffi)
)
