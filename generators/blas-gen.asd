(asdf:defsystem "blas-gen"
  :serial t
  :components ((:file "datatypes")
	       (:file "macro-utils")
	       (:file "gen-utils")
	       (:file "representation")
	       (:file "matrix-class")
	       (:file "avx-ops")
	       (:file "genop-arch-maps")
	       (:file "gemm-class")
	       (:file "c-utils")
	       (:file "header-gen")
	       (:file "blas-writer")))
