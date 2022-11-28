
#-asdf3.1 (error "ASDF 3.1+ required")
(defsystem "abyss"
	:name "Abyss Lisp"
	:version (:read-file-form "version.sexp")
	:description "A prototype of a Lisp dialect."
	:class :package-inferred-system
	:depends-on (:uiop)
	:pathname "src/"
	:components (
		(:file "types")
		(:file "context")
		(:file "environment")
		(:file "continuation")
	)
	:author "James Adam Armstrong"
	;:maintainer ""
	:license "ISC"
	;:homepage ""
	;:bug-traker ""
	:source-control (:git "https://github.com/Gwenio/abyss-lisp")
	:long-description #.(uiop:read-file-string
		(uiop:subpathname *load-pathname* "README.md"))
	:in-order-to ((test-op (test-op :abyss/test)))
)

(defsystem :abyss/test
	:name "Abyss Lisp Tests"
	;:class :package-inferred-system
	:pathname "test/"
	:serial t
	:depends-on ("fiveam" "abyss")
	:components (
		(:file "test")
		(:file "context")
		(:file "environment")
		(:file "continuation")
	)
	:perform (test-op (o c) (symbol-call :fiveam :run!
		(find-symbol* :abyss-tests :abyss/test))
	)
)
