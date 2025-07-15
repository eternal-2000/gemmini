(require :gen-utils)
(require :representation)

(provide :matrix-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General matrix class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass matrix ()
  ((name :initarg :name
	 :reader matrix-name
	 :type (or null string))
   (rows :initarg :rows
	 :reader matrix-rows
	 :initform 1
	 :type (or integer string))
   (columns :initarg :columns
	    :reader matrix-columns
	    :initform 1
	    :type (or integer string))
   (float-size :initarg :float-size
	       :reader matrix-float-size
	       :initform *double-size*
	       :type (member *precisions*)))
  (:documentation "Class of matrices with entries floats of fixed precision, given in bits"))

(defclass column-vector (matrix) ()
  (:documentation "Subclass of MATRIX which has exactly 1 column"))

(defclass row-vector (matrix) ()
  (:documentation "Subclass of MATRIX which has exactly 1 row"))

(defclass scalar (column-vector row-vector) ()
  (:documentation "Subclass of COLUMN-VECTOR and ROW-VECTOR, i.e. MATRIX with exactly 1 entry."))

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

(defmethod initialize-instance :after ((mat matrix) &key &allow-other-keys)
  (flet ((valid-dimension-p (x) (compose-call (or stringp posintp) x)))
    (assert-all (valid-dimension-p (matrix-rows mat))
		(valid-dimension-p (matrix-columns mat)))))

(defmethod initialize-instance :after ((vec column-vector) &key &allow-other-keys)
  (assert (= (matrix-columns vec) 1)))

(defmethod initialize-instance :after ((vec row-vector) &key &allow-other-keys)
  (assert (= (matrix-rows vec) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Matrix descriptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric emit-desc (matrix)
  (:documentation "Returns describe-object as string, i.e. where STREAM argument is NIL"))
(defmethod emit-desc ((obj matrix)) (describe-object obj nil))

(defmethod describe-object ((s scalar) stream)
  (format stream "~d-bit scalar ~a"
	  (matrix-float-size s)
	  (matrix-name s)))

(defmethod describe-object ((v row-vector) stream)
  (format stream "~d-bit ~a-element row vector ~a"
	  (matrix-float-size v)
	  (matrix-columns v)
	  (matrix-name v)))

(defmethod describe-object ((v column-vector) stream)
  (format stream "~d-bit ~a-element column vector ~a"
	  (matrix-float-size v)
	  (matrix-rows v)
	  (matrix-name v)))

(defmethod describe-object ((mat matrix) stream)
  (format stream "~d-bit ~d x ~d matrix ~a"
	  (matrix-float-size mat)
	  (matrix-rows mat)
	  (matrix-columns mat)
	  (matrix-name mat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Operation validators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric validate-mm (left-multiplier right-multiplier product)
  (:documentation "Checks whether matrices can be multiplied and stored in product matrix.
Only performs check on matrix dimensions; allows multiplication of different precisions."))

(defmethod validate-mm ((left-multiplier scalar) (right-multiplier matrix) (product matrix))
  (unless (and (equal (matrix-rows right-multiplier) (matrix-rows product))
	       (equal (matrix-columns right-multiplier) (matrix-columns product)))
    (error "Dimension mismatch:
~a cannot be scaled by
~a
to give
~a"
	   (emit-desc right-multiplier)
	   (emit-desc left-multiplier)
	   (emit-desc product)))
  (values))

(defmethod validate-mm ((left-multiplier matrix) (right-multiplier scalar) (product matrix))
  (validate-mm right-multiplier left-multiplier product))

(defmethod validate-mm ((left-multiplier matrix) (right-multiplier matrix) (product matrix))
  (unless (and (equal (matrix-rows product) (matrix-rows left-multiplier))
	       (equal (matrix-columns product) (matrix-columns right-multiplier))
	       (equal (matrix-columns left-multiplier) (matrix-rows right-multiplier)))
    (error "Dimension mismatch:
~a cannot multiply
~a
on the left to give
~a"
	   (emit-desc left-multiplier)
	   (emit-desc right-multiplier)
	   (emit-desc product)))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Matrix macro utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro do-matrix ((element-sym row-index col-index) matrix step &body body)
  "Applies BODY to elements of MATRIX in column-major order, with step-sizes STEP:
(ELEMENT-SYM ROW-INDEX COL-INDEX) substitutes for MATRIX elements in expression BODY"
  (with-gensyms (i j mat)
    `(let ((,mat ,matrix))
       (mapcan (lambda (,j)
		 (mapcar (lambda (,i)
			   (funcall
			    (lambda (,element-sym ,row-index ,col-index) ,@body)
			    ,mat ,i ,j))
			 (range (matrix-rows ,mat) 0 ,step)))
	       (range (matrix-columns ,mat))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod matrix-declare ((mat matrix) register-size)
  (let ((data-size (matrix-float-size mat)))
    (do-matrix (a i j) mat (/ register-size data-size)
      (make-dec (make-type 'packed-float data-size register-size)
		(make-var 'matrix-element a i j)))))

(defmethod vector-single-declare ((vec row-vector) register-size &key (column-index "j"))
  (let ((data-size (matrix-float-size vec)))
    (make-dec (make-type 'packed-float data-size register-size)
	      (make-var 'matrix-element vec 0 column-index))))

(defmethod matrix-init ((mat matrix) register-size)
  (let ((data-size (matrix-float-size mat)))
    (do-matrix (a i j) mat (/ register-size data-size)
      (make-init (make-type 'packed-float data-size register-size)
		 (make-var 'matrix-element a i j)
		 (make-call 'load
			    (make-memref 'address a i j)
			    data-size register-size)))))

(defmethod matrix-pointer-inc ((mat matrix) increment)
  (make-call 'inc (make-var 'simple mat) increment))

(defmethod matrix-load ((mat matrix) register-size)
  (let ((data-size (matrix-float-size mat)))
    (do-matrix (a i j) mat (/ register-size data-size)
      (make-assign (make-var 'matrix-element a i j)
		   (make-call 'load
			      (make-memref 'address a i j)
			      data-size register-size)))))

(defmethod matrix-store ((mat matrix) register-size)
  (let ((data-size (matrix-float-size mat)))
    (do-matrix (a i j) mat (/ register-size data-size)
      (make-call 'store
		 (make-memref 'address a i j)
		 (make-var 'matrix-element a i j)
		 data-size register-size))))

(defmethod single-broadcast-into-element ((vec row-vector) column register-size
					  &key (column-index "j"))
  (let ((data-size (matrix-float-size vec)))
    (make-assign (make-var 'matrix-element vec 0 column-index)
		 (make-call 'broadcast
			    (make-memref 'address vec 0 column)
			    data-size register-size))))

(defmethod vector-single-broadcast ((vec row-vector) register-size &key (column-index "j"))
  (do-matrix (a i j) vec 1
    (declare (ignore i))
    (single-broadcast-into-element a j register-size
				   :column-index column-index)))

(defmethod matrix-fma ((block-matrix matrix) (col-vec column-vector) (row-vec row-vector)
		       register-size &key (column-index "j"))
  (let ((data-size (matrix-float-size block-matrix)))
    (do-matrix (c i j) block-matrix (/ register-size data-size)
      (make-assign (make-var 'matrix-element c i j)
		   (make-call 'fma
			      (make-var 'matrix-element col-vec i 0)
			      (make-var 'matrix-element row-vec 0 column-index)
			      (make-var 'matrix-element c i j)
			      data-size register-size)))))

(defmethod gemm-update-block ((block-matrix matrix) (col-vec column-vector) (row-vec row-vector)
			      register-size &key (column-index "j"))
  (make-block
    (matrix-load col-vec register-size)
    (interleave-lists (vector-single-broadcast row-vec register-size
					       :column-index column-index)
		      (matrix-fma block-matrix col-vec row-vec register-size
				  :column-index column-index)
		      (/ (* (matrix-rows block-matrix)
			    (matrix-float-size block-matrix))
			 register-size))
    (matrix-pointer-inc col-vec (matrix-rows col-vec))
    (matrix-pointer-inc row-vec (matrix-columns row-vec))))

(defmethod gemm-update-loop ((block-matrix matrix) (col-vec column-vector) (row-vec row-vector)
			     register-size
			     &key (loop-index "k") (loop-length "p") (column-index "j"))
  (let ((idx (make-var 'simple loop-index)))
	(make-loop *index-type*
		   idx
		   0
		   (make-comp 'L
			      idx
			      (make-var 'simple loop-length))
		   (make-call 'inc idx 1)
		   (gemm-update-block block-matrix col-vec row-vec register-size
				      :column-index column-index))))

(defmethod gemm-microkernel ((block-matrix matrix) (col-vec column-vector) (row-vec row-vector)
			     register-size
			     &key (loop-index "k") (loop-length "p") (column-index "j"))
  (list
   (make-block (matrix-init block-matrix register-size)
	       (matrix-declare col-vec register-size)
	       (vector-single-declare row-vec register-size
				      :column-index column-index))
   (gemm-update-loop block-matrix col-vec row-vec register-size
		     :loop-index loop-index :loop-length loop-length :column-index column-index)
   (make-block (matrix-store block-matrix register-size))))
