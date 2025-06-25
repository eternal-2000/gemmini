(asdf:defsystem "blas-gen"
  :serial t
  :components ((:file "datatypes")
	       (:file "gen-utils")
	       (:file "representation")
	       (:file "matrix-class")
	       (:file "c-utils")
	       (:file "microkernel-gen")
	       (:file "header-gen")
	       (:file "blas-writer")))
