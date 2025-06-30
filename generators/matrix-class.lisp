(require :gen-utils)
(require :representation)

(provide :matrix-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General matrix class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass matrix ()
  ((name :initarg :name :reader matrix-name :initform nil :type (or null string))
   (rows :initarg :rows :reader matrix-rows :initform 1 :type integer)
   (columns :initarg :columns :reader matrix-columns :initform 1 :type integer)
   (float-size :initarg :float-size :reader matrix-float-size :initform *double-size* :type (member *precisions*)))
  (:documentation "Class of matrices with entries floats of fixed precision, given in bits"))

(defclass column-vector (matrix) ()
  (:documentation "Subclass of MATRIX which has exactly 1 column"))

(defclass row-vector (matrix) ()
  (:documentation "Subclass of MATRIX which has exactly 1 row"))

(defclass scalar (column-vector row-vector) ()
  (:documentation "Subclass of COLUMN-VECTOR and ROW-VECTOR, i.e. MATRIX with exactly 1 entry."))

(defun make-matrix (name rows columns float-size)
  (make-instance 'matrix :name name :rows rows :columns columns :float-size float-size))

(defun make-column-vector (name rows float-size)
  (make-instance 'column-vector :name name :rows rows :columns 1 :float-size float-size))

(defun make-row-vector (name columns float-size)
  (make-instance 'row-vector :name name :rows 1 :columns columns :float-size float-size))

(defun make-scalar (name float-size)
  (make-instance 'scalar :name name :rows 1 :columns 1 :float-size float-size))

(defmethod initialize-instance :after ((mat matrix) &key &allow-other-keys)
  (unless (and (posintp (matrix-rows mat)) (posintp (matrix-columns mat)))
    (error "Matrix dimensions must be positive integers")))

(defmethod initialize-instance :after ((vec column-vector) &key &allow-other-keys)
  (unless (= (matrix-columns vec) 1) (error "Column vectors must have exactly 1 column")))

(defmethod initialize-instance :after ((vec row-vector) &key &allow-other-keys)
  (unless (= (matrix-rows vec) 1) (error "Row vectors must have exactly 1 row")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Matrix utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro do-matrix ((element-sym row-index col-index) matrix step &body body)
  "Applies BODY to elements of MATRIX in column-major order, with step-sizes STEP:
(ELEMENT-SYM ROW-INDEX COL-INDEX) substitutes for elements of MATRIX in expression BODY"
  (with-gensyms (i j mat)
    `(let ((,mat ,matrix))
       (mapcan (lambda (,j)
		 (mapcar (lambda (,i)
			   (funcall (lambda (,element-sym ,row-index ,col-index)
				      ,@body)
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
  (do-matrix (a i j) vec 1 (declare (ignore i))
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
