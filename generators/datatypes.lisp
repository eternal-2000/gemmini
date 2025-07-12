(provide :datatypes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Global parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Data types

(defparameter *index-type* '(type int (32))) ; Default type representing sizes

;;;; AVX

(defparameter *ymm-pd* "__m256d")
(defparameter *ymm-ps* "__m256")
(defparameter *zmm-pd* "__m512d")
(defparameter *zmm-ps* "__m256d")

(defparameter *avx2-prefix* "_mm256")
(defparameter *avx512-prefix* "_mm512")
(defparameter *pd-suffix* "_pd")
(defparameter *ps-suffix* "_ps")
(defparameter *sd-suffix* "_sd")
(defparameter *ss-suffix* "_ss")
(defparameter *load* "_load")
(defparameter *store* "_store")
(defparameter *fma* "_fmadd")
(defparameter *broadcast* "_broadcast")

;;;; Constants

(defparameter *zmm-size* 512)
(defparameter *ymm-size* 256)
(defparameter *register-sizes* (list *ymm-size* *zmm-size*))
(defparameter *double-size* 64)
(defparameter *single-size* 32)
(defparameter *precisions* (list *single-size* *double-size*))
(defparameter *default-int-size* 32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Valid data types and operation tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *op-dict*
  '(comp type pointer dec assign init for-loop op-block call))

(defparameter *special-tags*
  '(for-loop if))

(defparameter *fn-dict*
  '((load . 3) (fma . 5) (store . 4) (broadcast . 3) (inc . 2)))

(defparameter *avx-dict*
  '(load store broadcast fma))

(defparameter *memref-dict*
  '((address . 3) (dereference . 1)))

(defparameter *var-dict*
  '((simple . 1) (matrix-element . 3)))

(defparameter *type-dict*
  '((void)
    (int . (32))
    (float . (32)) (float . (64))
    (packed-float . (32 256))
    (packed-float . (64 256))
    (packed-float . (32 512))
    (packed-float . (64 512))))

(defparameter *comparion-dict*
  '(L LE E GE G))

(defparameter *funarg-options* ;; Valid types for function arguments
  '(var memref call))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Supported BLAS operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *blas-dict*
  '("gemm"))
