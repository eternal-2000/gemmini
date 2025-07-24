(require :c-utils)

(provide :header-gen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; C header file generators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Gemm microkernel headers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mk-header (blas-op register-width)
  "Makes header file for blas-op_kernel"
  (let ((body (cat
	       (c-include "immintrin")
	       (c-prototype (make-avx-microkernel blas-op register-width)) ;; Temporary
	       (line-break))))
    (c-include-guard (microkernel-name blas-op) body)))
