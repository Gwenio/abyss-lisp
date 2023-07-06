
#-asdf3.1 (error "ASDF 3.1+ required")
(defsystem :abyss
	:name "Abyss Lisp"
	:version (:read-file-form "version.sexp")
	:description "A prototype of a Lisp dialect."
	:class :package-inferred-system
	:depends-on (:uiop)
	:pathname "src/"
	:components (
		(module "core" :components (
			(:file "types")
			(:file "error")
			(:file "context")
			(:file "evaluate")
			(:file "helpers")
			(:file "operatives")
			(:file "applicatives")
			(:file "boole")
			(:file "numbers")
			(:file "lists")
			(:file "handlers")
			(:file "record")
			(:file "modules")
			(:file "print")
			(:file "ground")
		))
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

(defsystem :abyss/ffi
	:name "Abyss Lisp FFI"
	:serial t
	:depends-on (:uiop :cffi-libffi :abyss)
	:pathname "src/"
	:components (
		(:module "ffi" :components (
			(:file "enums")
			(:file "frontend")
			(:file "library")
		))
	)
	:license "ISC"
)

(defsystem :abyss/viewer
	:name "Abyss Source Viewer"
	:depends-on (:abyss :abyss/ffi)
	:description "Utility to see how Abyss recieves code from the FFI."
	:pathname "src/"
	:components (
		(:file "viewer")
	)
	; :build-operation program-op
	:entry-point "abyss/viewer:main"
	:perform (program-op (o c)
		(uiop:dump-image (uiop:subpathname
			(uiop:getenv-pathname "ABYSS_BUILD_PATH" :defaults (uiop:getcwd)
				:ensure-directory t)
			(if (uiop:os-windows-p)
				"abyss-viewer.exe"
				"abyss-viewer"
			))
			:executable t :compression t)
	)
	:license "ISC"
)

(defsystem :abyss/bench
	:name "Abyss Bench"
	:depends-on (:abyss :abyss/ffi)
	:description "Utility to time the evaluation of single file scrips."
	:pathname "src/"
	:components (
		(:file "bench")
	)
	; :build-operation program-op
	:entry-point "abyss/bench:main"
	:perform (program-op (o c)
		(uiop:dump-image (uiop:subpathname
			(uiop:getenv-pathname "ABYSS_BUILD_PATH" :defaults (uiop:getcwd)
				:ensure-directory t)
			(if (uiop:os-windows-p)
				"abyss-bench.exe"
				"abyss-bench"
			))
			:executable t :compression t)
	)
	:license "ISC"
)

(defsystem :abyss/test
	:name "Abyss Lisp Tests"
	;:class :package-inferred-system
	:pathname "test/"
	:serial t
	:depends-on (:fiveam :abyss)
	:components (
		(:file "test")
		(:file "context")
		(:file "continuation")
		(:file "environment")
		(:file "evaluate")
		(:file "operatives")
		(:file "applicatives")
		(:file "boole")
		(:file "numbers")
		(:file "lists")
		(:file "handlers")
		(:file "record")
		(:file "modules")
	)
	; :build-operation test-op
	:perform (test-op (o c)
		(symbol-call :fiveam :run!
			(find-symbol* :abyss-tests :abyss/test))
	)
)
