(require :c-utils)
(require :matrix-class)

(provide :microkernel-gen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Microkernel abstractions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun blas-op-name (blas-op &key (float-size *double-size*))
  (let ((op (string-downcase blas-op)))
    (assert (member op *blas-dict* :test #'string=))
    (let ((precision-prefix (ecase float-size
			      (32 "s")
			      (64 "d"))))
      (cat precision-prefix op))))

(defun mk-name (blas-op &key (float-size *double-size*))
  (cat (blas-op-name blas-op :float-size float-size)
       "_kernel"))

(defun mk-signature (blas-op &key (float-size *double-size*))
  (let ((float-pointer (make-pointer 'float float-size)))
    (ecase-str blas-op
      ("gemm" (list *index-type* float-pointer float-pointer float-pointer *index-type*)))))

(defmethod mk-args-gemm (loop-length
			 (microtile matrix)
			 (column-micropanel column-vector)
			 (row-micropanel row-vector))
  (mapcar (lambda (x) (make-var 'simple x))
	  (list loop-length
		(matrix-name column-micropanel)
		(matrix-name row-micropanel)
		(matrix-name microtile)
		(column-stride microtile))))

(defmethod make-mk-gemm ((microtile matrix)
			 (column-micropanel column-vector)
			 (row-micropanel row-vector)
			 &key (register-size 256) (loop-length "p"))
  (assert (and (= (matrix-rows microtile) (matrix-rows column-micropanel))
	       (= (matrix-columns microtile) (matrix-columns row-micropanel))))
  (make-fn-def (make-type 'void)
	       (mk-name "gemm" :float-size (matrix-float-size microtile))
	       (mk-signature "gemm" :float-size (matrix-float-size microtile))
	       (mk-args-gemm loop-length microtile column-micropanel row-micropanel)
	       (gemm-microkernel microtile column-micropanel row-micropanel register-size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; C output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod c-mk-gemm ((microtile matrix)
		      (column-micropanel column-vector)
		      (row-micropanel row-vector)
		      &key (register-size *ymm-size*) (loop-length "p"))
  (c-fn-def (make-mk-gemm microtile column-micropanel row-micropanel
			  :register-size register-size :loop-length loop-length)))

(defmethod make-c-mk-gemm ((microtile matrix)
			   (column-micropanel column-vector)
			   (row-micropanel row-vector)
			   &key (register-size *ymm-size*) (loop-length "p"))
  (cat (c-include "immintrin")
       (c-mk-gemm microtile column-micropanel row-micropanel
		  :register-size register-size :loop-length loop-length)))
