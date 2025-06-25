(require :microkernel-gen)

(provide :header-gen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; C header file generators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Gemm microkernel headers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun mk-selector (blas-op &key (float-size *double-size*))
;;   "Simple token pasting microkernel choice at compile time given dimensions MR x NR"
;;   (flet ((mk-string (op row col)
;; 	   (emit "~a_MICROKERNEL(~a, ~a)" (string-upcase op) row col))
;; 	 (mk-string-sub (op row col)
;; 	   (emit "~a_MK_SIZE(~a, ~a)" (string-upcase op) row col))
;; 	 (mk-replaced (op row col)
;; 	   (emit "~a_kernel_##~a##x##~a" op row col)))
;;     (let ((dummy-row "M") (dummy-col "N")
;; 	  (op (blas-op-name blas-op :float-size float-size)))
;;       (cat (def-directive
;; 	       (mk-string-sub op dummy-row dummy-col)
;; 	       (mk-replaced op dummy-row dummy-col))
;; 	   (line-break)
;; 	   (def-directive
;; 	       (mk-string op dummy-row dummy-col)
;; 	       (mk-string-sub op dummy-row dummy-col))))))

(defun mk-header (blas-op fn-def-rep &key (float-size *double-size*))
  "Makes header file for blas-op_kernel"
  (let ((header-name (mk-name blas-op :float-size float-size))
	(body (cat
	       (c-include "immintrin")
	       (c-prototype fn-def-rep)
	       (line-break))))
;	       (mk-selector blas-op :float-size float-size))))
    (c-include-guard header-name body)))
