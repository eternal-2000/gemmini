(require :gen-utils)
(require :representation)

(provide :matrix-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Matrix properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype symmetry-type ()
  `(member :symmetric :antisymmetric :hermitian :antihermitian nil))

(deftype triangular-type ()
  `(member :upper :lower :diagonal nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General matrix class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass matrix ()
  ((name :initarg :name
	 :reader matrix-name
	 :type (or null string)
	 :documentation
	 "Symbolic name of the matrix object.")
   (rows :initarg :rows
	 :reader matrix-rows
	 :initform 1
	 :type (or integer string)
	 :documentation
	 "Number of rows of the matrix.")
   (columns :initarg :columns
	    :reader matrix-columns
	    :initform 1
	    :type (or integer string)
	    :documentation
	    "Number of columns of the matrix.")
   (float-size :initarg :float-size
	       :reader matrix-float-size
	       :initform *double-size*
	       :type (member *precisions*)
	       :documentation
	       "Number of bits of an element of the matrix.")
   (symmetry :initarg :symmetric
	     :reader matrix-symmetry
	     :initform nil
	     :type symmetry-type
	     :documentation
	      "Can be SYMMETRIC, HERMITIAN, ANTISYMMETRIC, ANTIHERMITIAN, or NIL.")
   (triangular :initarg :symmetric
	       :reader matrix-triangular?
	       :initform nil
	       :type triangular-type
	       :documentation
	       "Can be one of 4 values: UPPER, LOWER, DIAGONAL, or NIL."))
  (:documentation
   "Class of matrices with entries floats of fixed precision, given in bits."))

(defclass column-vector (matrix) ()
  (:documentation
   "Subclass of MATRIX which has exactly 1 column."))

(defclass row-vector (matrix) ()
  (:documentation
   "Subclass of MATRIX which has exactly 1 row."))

(defclass scalar (column-vector row-vector) ()
  (:documentation
   "Subclass of COLUMN-VECTOR and ROW-VECTOR, i.e. MATRIX with exactly 1 entry."))

(defun make-matrix (name rows columns float-size)
  (make-instance 'matrix :name (string name)
			 :rows rows
			 :columns columns
			 :float-size float-size))

(defun make-column-vector (name rows float-size)
  (make-instance 'column-vector :name (string name)
				:rows rows
				:float-size float-size))

(defun make-row-vector (name columns float-size)
  (make-instance 'row-vector :name (string name)
			     :columns columns
			     :float-size float-size))

(defun make-scalar (name float-size)
  (make-instance 'scalar :name (string-downcase name)
			 :float-size float-size))

(defmethod initialize-instance :after ((mat matrix) &key)
  (flet ((valid-dimension-p (x)
	   (compose-call (or stringp posintp) x)))
    (assert-all (valid-dimension-p (matrix-rows mat))
		(valid-dimension-p (matrix-columns mat))
		(member (matrix-float-size mat) *precisions*))))

(defmethod initialize-instance :after ((vec column-vector) &key)
  (assert (= (matrix-columns vec) 1)))

(defmethod initialize-instance :after ((vec row-vector) &key)
  (assert (= (matrix-rows vec) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Matrix descriptions: allows clearer depictions of AST operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric emit-desc (matrix)
  (:documentation "Returns describe-object as string, i.e. with format NIL."))
(defmethod emit-desc ((obj matrix)) (print-object obj nil))

(defmethod print-object ((mat matrix) stream)
  (format stream "#<~s ~a[~a x ~a : ~d-bit] {~x}>"
	  (type-of mat)
	  (if (slot-boundp mat 'name) (matrix-name mat)
	      "(Nameless)")
	  (matrix-rows mat)
	  (matrix-columns mat)
	  (matrix-float-size mat)
	  (sb-kernel:get-lisp-obj-address mat)))

(defmethod print-object ((v column-vector) stream)
  (format stream "#<~s ~a[~a : ~d-bit] {~x}>"
	  (type-of v)
	  (if (slot-boundp v 'name) (matrix-name v)
	      "(Nameless)")
	  (matrix-rows v)
	  (matrix-float-size v)
	  (sb-kernel:get-lisp-obj-address v)))

(defmethod print-object ((v row-vector) stream)
  (format stream "#<~s ~a[~a : ~d-bit] {~x}>"
	  (type-of v)
	  (if (slot-boundp v 'name) (matrix-name v)
	      "(Nameless)")
	  (matrix-columns v)
	  (matrix-float-size v)
	  (sb-kernel:get-lisp-obj-address v)))

(defmethod print-object ((s scalar) stream)
  (format stream "#<~s ~a[~d-bit] {~x}>"
	  (type-of s)
	  (if (slot-boundp s 'name) (matrix-name s)
	      "(Nameless)")
	  (matrix-float-size s)
	  (sb-kernel:get-lisp-obj-address s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Matrix macro utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-matrix ((element-sym row-index col-index) matrix step &body body)
  "Applies BODY to elements of MATRIX in column-major order, with step-sizes STEP:
(ELEMENT-SYM ROW-INDEX COL-INDEX) substitutes for MATRIX elements in expression BODY."
  (with-gensyms (i j mat)
    `(let ((,mat ,matrix))
       (mapcan (lambda (,j)
		 (mapcar (lambda (,i)
			   (funcall
			    (lambda (,element-sym ,row-index ,col-index) ,@body)
			    ,mat ,i ,j))
			 (range (matrix-rows ,mat) 0 ,step)))
	       (range (matrix-columns ,mat))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Matrix operations: used to build ASTs for operations on matrix objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Declaring variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric matrix-declare (obj register-width)
  (:documentation
   "Declares matrix-object variable of OBJ for use in vector register with REGISTER-WIDTH."))

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
   "Loads matrix object into registers of width REGISTER-WIDTH. Specialises to a single broadcast operation for ROW-VECTOR objects."))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Microkernel blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod gemm-update-block ((block-matrix matrix) (col-vec column-vector) (row-vec row-vector)
			      register-width &key (column-index "j"))
  (make-block
    (matrix-load col-vec register-width)
    (interleave-lists (matrix-load row-vec register-width)
		      (matrix-fma block-matrix col-vec row-vec register-width
				  :column-index column-index)
		      (/ (* (matrix-rows block-matrix)
			    (matrix-float-size block-matrix))
			 register-width))
    (matrix-pointer-inc col-vec (matrix-rows col-vec))
    (matrix-pointer-inc row-vec (matrix-columns row-vec))))

(defmethod gemm-update-loop ((block-matrix matrix) (col-vec column-vector) (row-vec row-vector)
			     register-width
			     &key (loop-index "k") (loop-length "p") (column-index "j"))
  (let ((idx (make-var 'simple loop-index)))
	(make-loop *index-type*
		   idx
		   0
		   (make-comp 'L
			      idx
			      (make-var 'simple loop-length))
		   (make-call 'inc idx 1)
		   (gemm-update-block block-matrix col-vec row-vec register-width
				      :column-index column-index))))

(defmethod gemm-microkernel ((block-matrix matrix) (col-vec column-vector) (row-vec row-vector)
			     register-width
			     &key (loop-index "k") (loop-length "p") (column-index "j"))
  (list
   (make-block (matrix-init block-matrix register-width)
	       (matrix-declare col-vec register-width)
	       (matrix-declare row-vec register-width))
   (gemm-update-loop block-matrix col-vec row-vec register-width
		     :loop-index loop-index :loop-length loop-length :column-index column-index)
   (make-block (matrix-store block-matrix register-width))))
