
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

(uiop:define-package :abyss/ffi/frontend
	(:use :cl)
	(:import-from :cffi
		:defcstruct :defcfun :with-foreign-objects :with-foreign-slots
		:null-pointer-p :null-pointer :foreign-slot-value
		:foreign-enum-keyword :mem-ref :mem-aref :foreign-type-size
		:foreign-string-to-lisp :incf-pointer
		:translate-from-foreign :expand-from-foreign
	)
	(:import-from :abyss/ffi/enums
		:tokens :nodes
	)
	(:import-from :abyss/types
		:+inert+ :+ignore+ :+true+ :+false+ :make-glyph
	)
	(:export
		:load-source
	)
)
(in-package :abyss/ffi/frontend)

(defcstruct failure
	"Either a parse or lex error"
	(offset :uint32)
	(length :uint32)
	(line :uint32)
	(unmatched :uint32)
	(lex-id tokens) ; if :eoi, there is no lex error
	(parse-id nodes)
)

(defcstruct (slice :class c-slice)
	"A slice from the buffer data"
	(offset :uint32)
	(length :uint32)
)

(defmethod translate-from-foreign (ptr (type c-slice))
	(with-foreign-slots ((offset length) ptr (:struct slice))
		(values offset length)
	)
)

(defmethod expand-from-foreign (ptr (type c-slice))
	`(with-foreign-slots ((offset length) ,ptr (:struct slice))
		(values offset length)
	)
)

(defcstruct success
	"Contains parsed tree data"
	(ast (:pointer nodes)) ; failed if null pointer
	(atoms (:pointer tokens))
	(slices (:pointer (:struct slice)))
)

(defcstruct (loaded :class c-loaded)
	"Loaded source file data."
	(data (:pointer :uint8))
	(size :uint32)
)

(defmethod translate-from-foreign (ptr (type c-loaded))
	(with-foreign-slots ((data size) ptr (:struct loaded))
		(values data size)
	)
)

(defmethod expand-from-foreign (ptr (type c-loaded))
	`(with-foreign-slots ((data size) ,ptr (:struct loaded))
		(values data size)
	)
)

(defcfun (%load-source "abyss_load_file") (:struct loaded)
	"Loads a source file"
	(name :string)
	(len :uint32) ; length of name, not including any terminating null
	(fail (:pointer (:struct failure)))
	(tree (:pointer (:struct success)))
)

(defcfun (%unload-tree "abyss_unload_tree") :void
	"Free tree memory"
	(tree (:pointer (:struct success)))
)

(defcfun (%unload-source "abyss_unload_buffer") :void
	"Free loaded buffer memory"
	(data (:pointer :uint8))
)

(define-condition unknown-atom (error) (
	(id :type keyword
		:reader unknown-atom-id
		:initarg id
	)
	(str :type string
		:reader unknown-atom-str
		:initarg str
	)
))

(declaim (ftype (function (keyword string) t) process-atom))

(defun process-atom (id str)
	(case id
		(:symbol (make-glyph str))
		(:int-dec (parse-integer (remove #\' str)))
		(:int-hex (parse-integer (remove #\' str) :start 2 :radix 16))
		(:int-bin (parse-integer (remove #\' str) :start 2 :radix 2))
		(:int-oct (parse-integer (remove #\' str) :start 2 :radix 8))
		; TODO: escape sequences
		(:string (subseq str 1 (1- (length str))))
		(t
			(print (list id str))
			(error 'unknown-atom :id id :str str)
		)
	)
)

(define-condition unknown-node (error) (
	(id :type keyword
		:reader unknown-node-id
		:initarg id
	)
))

#+sbcl (declaim (ftype (function
	(sb-alien:system-area-pointer sb-alien:system-area-pointer
		sb-alien:system-area-pointer sb-alien:system-area-pointer) t)
	process-ast))

(defun process-ast (data ast atoms slices)
	(loop with stack = (let ((root (list t))) (setf (cdr root) root) root)
		for ast-count fixnum = 1 then (1+ ast-count)
		for node = (mem-ref ast 'nodes ast-count)
		with atom-n fixnum = 0
		with slice-n fixnum = 0
		do (case node
			(:cons
				(if (eq t (first stack))
					(let ((fresh (cons t stack)))
						(setf (first stack) fresh)
						(setf stack fresh)
					)
					(let ((fresh (cons t (cdr stack))))
						(setf (cdr stack) fresh)
						(setf stack fresh)
					)
				)
			)
			(:atom (let ((id (mem-ref atoms 'tokens atom-n)))
				(incf atom-n)
				(let ((x (case id
						(:inert +inert+)
						(:ignore +ignore+)
						(:null nil)
						(:true +true+)
						(:false +false+)
						(:empty-string "")
						(t (multiple-value-bind (off len)
							(mem-aref slices '(:struct slice) slice-n)
							(incf slice-n)
							(process-atom id (foreign-string-to-lisp data
								:offset off :count len))
						))
					)))
					(if (eq t (first stack))
						(setf (first stack) x)
						(shiftf stack (cdr stack) x)
					)
				)
			))
			(:null
				(if (eq t (first stack))
					(setf (first stack) nil)
					(shiftf stack (cdr stack) nil)
				)
			)
			(:eoi
				(return (shiftf (cdr stack) nil))
			)
			(t (error 'unknown-node :id node))
		)
	)
)

(define-condition lex-error (error) (
	(id :type keyword
		:reader lex-err-id
		:initarg id
	)
	(line :type integer
		:reader lex-err-line
		:initarg line
	)
	(file :type string
		:reader lex-err-file
		:initarg file
	)
))

(define-condition parser-error (error) (
	(id :type keyword
		:reader parse-err-id
		:initarg id
	)
	(line :type integer
		:reader parse-err-line
		:initarg line
	)
	(file :type string
		:reader parse-err-file
		:initarg file
	)
	(unmatched :type integer
		:reader parse-err-line
		:initarg line
	)
))

(define-condition read-error (error) (
	(file :type string
		:reader read-error-file
		:initarg file
	)
	(msg :type string
		:reader read-error-msg
		:initarg msg
	)
))

(declaim (ftype (function (string) t) load-source))

(defun load-source (name)
	(with-foreign-objects (
			(tree '(:struct success))
			(fail '(:struct failure))
		)
		(multiple-value-bind (data size)
			(%load-source name (length name) fail tree)
			#+sbcl(declare (type sb-alien:system-area-pointer data))
			(declare (type integer size))
			(if (null-pointer-p data)
				(if (zerop size)
					nil
					(case size
						(1 (error 'read-error :file name
							:msg "failed to open file"))
						(2 (error 'read-error :file name
							:msg "failed to read file"))
						(t (error 'read-error :file name
							:msg "file to big"))
					)
				)
				(with-foreign-slots ((ast atoms slices) tree (:struct success))
					(if (null-pointer-p ast)
						(unwind-protect
							(with-foreign-slots
								((offset length line unmatched lex-id parse-id)
									fail (:struct failure))
								(if (eq lex-id :eoi)
									(error 'parser-error :id parse-id
										:line line :unmatched unmatched
										:file name)
									(error 'lex-error :id lex-id :line line
										:file name)
								)
							)
							(%unload-source data)
						)
						(unwind-protect
							(if (eq :eoi (mem-ref ast 'nodes))
								nil ; first is either eoi or cons
								(process-ast data ast atoms slices)
							)
							(%unload-tree tree)
							(%unload-source data)
						)
					)
				)
			)
		)
	)
)
