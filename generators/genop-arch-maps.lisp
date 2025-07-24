(require :avx-ops)

(provide :genop-arch-maps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Genop-to-AVX mapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gendef-avx (x register-width)
  (make-fn-def (make-type 'void)
	       (getf x :gendef)
	       (gendef-sig x)
	       (gendef-args x)
	       (genseq-avx (getf x :body) register-width)))

(defun gendef-sig (gendef) ;; Placeholder definition before genseq
			   ;; properly handled - should iterate over
			   ;; all genops in genseq to build signature
  (let ((body (getf gendef :body)))
    (if (is-genop body) (build-sig body)
	(mapcar #'build-sig (expand-genseq gendef)))))

(defun build-sig (genop)
  (ecase (second genop)
    (outer-product
     (let ((inputs (getf genop :inputs))
	   (target (getf genop :target)))
       (append (list *index-type*)
	       (mapcar (lambda (precision)
			 (make-pointer 'float precision))
		       (mapcar (lambda (obj)
				 (matrix-float-size obj))
			       (append inputs target)))
	       (list *index-type*))))))

(defun gendef-args (gendef) ;; Placeholder definition - should build
			    ;; arg list over genseq
  (let ((body (getf gendef :body)))
    (if (is-genop body) (genop-args body)
	(mapcar #'genop-args (expand-genseq body)))))

(defun genop-args (genop)
  (flet ((get-matrix-names (matrix-objs)
	   (mapcar (lambda (obj)
		     (matrix-name obj))
		   matrix-objs)))
    (ecase (second genop)
      (outer-product
       (let ((loop-index "p")
	     (inputs (getf genop :inputs))
	     (target (getf genop :target)))
	 (mapcar (lambda (str)
		   (make-var 'simple str))
		 (append (list loop-index)
			 (get-matrix-names inputs)
			 (interleave-lists (get-matrix-names target)
					   (mapcar (lambda (obj)
						     (column-stride obj))
						   target)
					   1))))))))

(defun genseq-avx (x register-width) ;; Placeholder definition before
				     ;; genseq properly handled
  (genop-avx x register-width))

(defun genop-avx (x register-width)
  (ecase (second x)
    (outer-product
     (make-avx-outer-product (demand-list (getf x :inputs))
			     (demand-list (getf x :target))
			     register-width))))

(defun make-avx-outer-product (inputs targets register-width)
  (list
   (make-avx-initialise inputs targets register-width)
   (make-avx-fma-loop inputs targets register-width)
   (make-avx-finalise targets register-width)))

(defun make-avx-initialise (inputs targets register-width)
  (apply #'make-block (append (mapcar (lambda (x)
					(matrix-init x register-width))
				      targets)
			      (mapcar (lambda (x)
					(matrix-declare x register-width))
				      inputs))))

(defun make-avx-fma-loop (inputs targets register-width)
  (let ((loop-index (make-var 'simple "k"))
	(loop-length (make-var 'simple "p")))
    (make-loop *index-type*
	       loop-index
	       0
	       (make-comp 'L loop-index loop-length)
	       (make-call 'inc loop-index 1)
	       (make-block (mapcar (lambda (x)
				     (make-avx-fma inputs x register-width))
				   (demand-list targets))))))

(defun make-avx-fma (inputs target register-width)
  (let ((col-vec (first inputs))
	(row-vec (second inputs)))
    (make-block
     (matrix-load col-vec register-width)
     (interleave-lists (matrix-load row-vec register-width)
		       (matrix-fma target col-vec row-vec register-width)
		       (/ (* (matrix-rows col-vec)
			     (matrix-float-size target))
			  register-width))
     (matrix-pointer-inc col-vec (matrix-rows col-vec))
     (matrix-pointer-inc row-vec (matrix-columns row-vec)))))

(defun make-avx-finalise (targets register-width)
  (apply #'make-block (mapcar (lambda (x)
				(matrix-store x register-width))
			      targets)))
