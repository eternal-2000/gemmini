(require :gen-utils)
(require :representation)

(provide :matrix-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Special matrix properties
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
	       :reader matrix-triangular
	       :initform nil
	       :type triangular-type
	       :documentation
	       "Can be one of 4 values: UPPER, LOWER, DIAGONAL, or NIL."))
  (:documentation
   "Class of matrices with entries floats of fixed precision, given in bits."))

(defclass vector-mixin (matrix) ()
  (:documentation
   "Mixin intended to be a superclass of COLUMN-VECTOR and ROW-VECTOR, for convenience."))

(defclass column-vector (vector-mixin) ()
  (:documentation
   "Subclass of MATRIX which has exactly 1 column."))

(defclass row-vector (vector-mixin) ()
  (:documentation
   "Subclass of MATRIX which has exactly 1 row."))

(defclass scalar (column-vector row-vector) ()
  (:documentation
   "Subclass of COLUMN-VECTOR and ROW-VECTOR, i.e. MATRIX with exactly 1 entry."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmethod initialize-instance :after ((v column-vector) &key)
  (setf (slot-value v 'columns) 1))

(defmethod initialize-instance :after ((v row-vector) &key)
  (setf (slot-value v 'rows) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Matrix descriptions: allows clearer depictions of AST operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric emit-desc (matrix)
  (:documentation "Returns describe-object as string, i.e. with format NIL."))
(defmethod emit-desc ((obj matrix)) (print-object obj nil))

(defmethod print-object ((mat matrix) stream)
  (format stream "#<~s ~a[~a x ~a : ~d-bit]>"
	  (type-of mat)
	  (if (slot-boundp mat 'name) (matrix-name mat)
	      "(Nameless)")
	  (matrix-rows mat)
	  (matrix-columns mat)
	  (matrix-float-size mat)))

(defmethod print-object ((v column-vector) stream)
  (format stream "#<~s ~a[~a : ~d-bit]>"
	  (type-of v)
	  (if (slot-boundp v 'name) (matrix-name v)
	      "(Nameless)")
	  (matrix-rows v)
	  (matrix-float-size v)))

(defmethod print-object ((v row-vector) stream)
  (format stream "#<~s ~a[~a : ~d-bit]>"
	  (type-of v)
	  (if (slot-boundp v 'name) (matrix-name v)
	      "(Nameless)")
	  (matrix-columns v)
	  (matrix-float-size v)))

(defmethod print-object ((s scalar) stream)
  (format stream "#<~s ~a[~d-bit]>"
	  (type-of s)
	  (if (slot-boundp s 'name) (matrix-name s)
	      "(Nameless)")
	  (matrix-float-size s)))

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

(defgeneric column-stride (obj)
  (:documentation
   "Denotes column-stride of OBJ X as string ldX (leading dimension of X).
Accepts strings, and objects of MATRIX class."))
(defmethod column-stride ((str string)) (cat "ld" str))
(defmethod column-stride ((mat matrix)) (emit "ld~a" (matrix-name mat)))
