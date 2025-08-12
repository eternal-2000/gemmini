(require :matrix-class)
(provide :avx-ops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Matrix operations: used to build ASTs for operations on matrix objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Declaring variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric matrix-declare (obj register-width)
  (:documentation
   "Declares matrix object OBJ for vector register with REGISTER-WIDTH."))

(defmethod matrix-declare ((mat matrix) register-width)
  (let ((data-size (matrix-float-size mat)))
    (with-matrix (a i j) mat (/ register-width data-size)
      (make-dec (make-type 'packed-float data-size register-width)
		(make-var 'matrix-element a i j)))))

(defmethod matrix-declare ((v row-vector) register-width)
  (let ((data-size (matrix-float-size v)))
    (make-dec (make-type 'packed-float data-size register-width)
	      (make-var 'matrix-element v 0 "j"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Loading, initialising
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod matrix-init ((mat matrix) register-width)
  (let ((data-size (matrix-float-size mat)))
    (with-matrix (a i j) mat (/ register-width data-size)
      (make-init (make-type 'packed-float data-size register-width)
		 (make-var 'matrix-element a i j)
		 (make-call 'load
			    (make-memref 'address a i j)
			    data-size register-width)))))

(defgeneric matrix-load (obj register-width)
  (:documentation
   "Loads matrix object into registers of width REGISTER-WIDTH.
 Specialises to a single broadcast operation for ROW-VECTOR objects."))

(defmethod matrix-load ((mat matrix) register-width)
  (let ((data-size (matrix-float-size mat)))
    (with-matrix (a i j) mat (/ register-width data-size)
      (make-assign (make-var 'matrix-element a i j)
		   (make-call 'load
			      (make-memref 'address a i j)
			      data-size register-width)))))

(defmethod matrix-load ((v row-vector) register-width)
  (with-matrix (a k j) v 1
    (declare (ignore k))
    (let ((data-size (matrix-float-size a)))
      (make-assign (make-var 'matrix-element a 0 "j")
		   (make-call 'broadcast
			      (make-memref 'address a 0 j)
			      data-size register-width)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Storing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod matrix-store ((mat matrix) register-width)
  (let ((data-size (matrix-float-size mat)))
    (with-matrix (a i j) mat (/ register-width data-size)
      (make-call 'store
		 (make-memref 'address a i j)
		 (make-var 'matrix-element a i j)
		 data-size register-width))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Fused multiply-add operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod matrix-fma ((block-matrix matrix) (col-vec column-vector) (row-vec row-vector)
		       register-width &key (column-index "j"))
  (let ((data-size (matrix-float-size block-matrix)))
    (with-matrix (c i j) block-matrix (/ register-width data-size)
      (make-assign (make-var 'matrix-element c i j)
		   (make-call 'fma
			      (make-var 'matrix-element col-vec i 0)
			      (make-var 'matrix-element row-vec 0 column-index)
			      (make-var 'matrix-element c i j)
			      data-size register-width)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Array pointer operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod matrix-pointer-inc ((mat matrix) increment)
  (make-call 'inc (make-var 'simple mat) increment))
