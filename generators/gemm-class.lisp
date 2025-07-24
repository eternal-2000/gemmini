(require :matrix-class)
(require :genop-arch-maps)

(provide :gemm-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Gemm class definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *gemm-variation-argcounts*
  `((default . 5)))

(defclass gemm ()
  ((input-matrices :initarg :input-matrices
		   :reader gemm-input-matrices
		   :type list
		   :documentation
		   "A list of the names of matrices which are strict inputs.")
   (input-scalars :initarg :input-scalars
		  :reader gemm-input-scalars
		  :type list
		  :documentation "A list of the names of the scalar inputs.")
   (input-precision :initarg :input-precision
		    :reader gemm-input-precision
		    :type integer
		    :documentation "Size in bits of the floats which are entries of the inputs.")
   (target-matrices :initarg :target-matrices
		    :reader gemm-target-matrices
		    :type list
		    :documentation
		    "A list of the names of matrices which are to be updated")
   (target-precision :initarg :target-precision
		     :reader gemm-target-precision
		     :type integer
		     :documentation
		     "Size in bits of the floats which are entries of the target matrices.")
   (loop-parameters :initarg :loop-parameters
		    :reader gemm-loop-parameters
		    :type list
		    :documentation
		    "Integers representing dimensions of submatrices taken into each loop.")
   (variation :initarg :variation
	      :reader gemm-variation
	      :initform 'default
	      :type symbol
	      :documentation "The algorithm used to implement the gemm operation."))
  (:documentation "The class of all GEMM operations, i.e. of the form C ← αAB + βC."))

(defun make-gemm (input-precision target-precision loop-parameters
		  &optional (variation 'default))
  (make-instance 'gemm :input-matrices '("A" "B")
		       :input-scalars '("alpha" "beta")
		       :input-precision input-precision
		       :target-matrices '("C")
		       :target-precision target-precision
		       :loop-parameters loop-parameters
		       :variation variation))

(defmethod initialize-instance :after ((op gemm) &key)
  (assert-all (every #'posintp (gemm-loop-parameters op))
	      (member (cons (gemm-variation op)
			    (total-length (gemm-input-matrices op)
					  (gemm-input-scalars op)
					  (gemm-target-matrices op)))
		      *gemm-variation-argcounts*
		      :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Gemm object descriptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric op-name (op)
  (:documentation "Generates BLAS-style name of operation from object data."))

(defmethod op-name ((op gemm))
  (let* ((inpre (gemm-input-precision op))
	 (tarpre (gemm-target-precision op))
	 (precision-prefix (if (= inpre tarpre)
			       (ecase tarpre
				 (32 "s")
				 (64 "d"))
			       (error "Mixed precision not implemented yet"))))
    (cat precision-prefix
	 (string-downcase (class-name (class-of op))))))

(defmethod print-object ((op gemm) stream)
  (format stream "#<~s ~a [~{~d~^, ~}]>"
	  (type-of op)
	  (op-name op)
	  (gemm-loop-parameters op)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General utility methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric microkernel-name (op)
  (:documentation "Name of microkernel used by OP."))

(defmethod microkernel-name ((op gemm))
  (cat (op-name op) "_kernel"))

(defgeneric microkernel-signature (op)
  (:documentation
   "Makes abstract list of types which should be passed as the OP microkernel signature."))

(defmethod microkernel-signature ((op gemm))
  (let ((inpre-pointer (make-pointer 'float (gemm-input-precision op)))
	(tarpre-pointer (make-pointer 'float (gemm-target-precision op))))
    (list
     *index-type* inpre-pointer inpre-pointer tarpre-pointer *index-type*)))

(defgeneric microkernel-arg-list (op)
  (:documentation
   "Makes abstract list of arguments which are required by OP microkernel."))

(defmethod microkernel-arg-list (op)
  (let ((loop-length "p"))
    (mapcar (lambda (x)
	      (make-var 'simple x))
	    (flatten (list loop-length
			   (gemm-input-matrices op)
			   (interleave-lists (gemm-target-matrices op)
					     (mapcar (lambda (matrix)
						       (column-stride matrix))
						     (gemm-target-matrices op))
					     1))))))

(defgeneric form-input-matrices (op)
  (:documentation "Takes names of input args in GEMM object, maps onto MATRIX objects.
Currently only implemented for microkernel-level code."))

(defmethod form-input-matrices ((op gemm))
  (let* ((mk-dims (cdddr (gemm-loop-parameters op)))
	 (rows (first mk-dims))
	 (columns (second mk-dims)))
    (mapzip (list (lambda (name)
		    (make-column-vector name rows (gemm-input-precision op)))
		  (lambda (name)
		    (make-row-vector name columns (gemm-input-precision op))))
	    (gemm-input-matrices op))))

(defgeneric form-target-matrices (op)
  (:documentation "Takes names of target args in GEMM object, maps onto MATRIX objects.
Currently only implemented for microkernel-level code."))

(defmethod form-target-matrices ((op gemm))
  (let* ((mk-dims (cdddr (gemm-loop-parameters op)))
	 (rows (first mk-dims))
	 (columns (second mk-dims)))
    (mapcar (lambda (name)
	      (make-matrix name rows columns (gemm-target-precision op)))
	    (gemm-target-matrices op))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Hardware-agnostic gemm representations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric make-abstract-microkernel (op)
  (:documentation
   "Constructs architecture-agnostic AST of GEMM-object OP microkernel."))

(defmethod make-abstract-microkernel ((op gemm))
  (let ((targets (form-target-matrices op))
	(inputs (form-input-matrices op)))
    (make-gendef (microkernel-name op)
		 inputs
		 targets
		 (mapcan (lambda (x)
			   (list 'outer-product inputs x))
			 targets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mapping genops to architecture-specific ASTs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-avx-microkernel ((op gemm) register-width)
  (gendef-avx (make-abstract-microkernel op) register-width))
