(require :avx-ops)
(require :compdag)

(provide :genop-arch-maps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Analysis tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fuse-loops (genops)
  "Fuses loops in GENOPS into GENOP-DAG structures.
To be expanded as more loop operations added."
  (flet ((count-loop-tags (g)
	   (let ((loop-genops '(outer-product)) ;; To be expanded
		 (alist nil))
	     (dolist (tag loop-genops alist)
	       (push (cons tag
			   (count-if (lambda (x) (eq (second x) tag)) g))
		     alist))))
	 (fuse-loop-tag (g tag)
	   (map-group-consec (lambda (x) (make-genop-dag tag x))
			     g
			     (lambda (x) (eq (second x) tag)))))
    (reduce (lambda (g cell)
	      (fuse-loop-tag g (first cell)))
	    (remove-if (lambda (x) (< (rest x) 2)) (count-loop-tags genops))
	    :initial-value genops)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Genop-to-AVX mapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gendef-avx (gendef register-width)
  "Compiles GENDEF to C code with AVX intrinsics given REGISTER-WIDTH."
  (make-fn-def (make-type 'void) ;; Only handling void-type functions so far
	       (getf gendef :gendef)
	       (gendef-sig gendef)
	       (gendef-args gendef)
	       (genseq-avx (getf gendef :body) register-width)))

(defun gendef-sig (gendef)
  "Creates signature of function defined by GENDEF.
For now, assumes all inputs and targets are matrix objects, and only handles outer products."
  (let* ((inputs (getf gendef :inputs))
	 (targets (getf gendef :targets))
	 (body-genseq (getf gendef :body))
	 (type-list (interleave (mapcar #'matrix-type targets)
				(make-list (length targets)
					   :initial-element *index-type*)
				:initial-list (mapcar #'matrix-type
						      (filter-out-list inputs
								       targets)))))
    (if (some (lambda (x)
		(eq (first x) 'outer-product))
	      (rest body-genseq))
	(cons *index-type* type-list)
	type-list)))

(defun gendef-args (gendef)
  "Creates arguments from function GENDEF.
For now, assumes all inputs and targets are matrix objects, and only handles outer products."
  (let* ((inputs (getf gendef :inputs))
	 (targets (getf gendef :targets))
	 (body-genseq (getf gendef :body))
	 (arg-list (interleave (mapcar #'matrix-name targets)
			       (mapcar #'column-stride targets)
			       :initial-list (mapcar #'matrix-name
						     (filter-out-list inputs
								      targets)))))
    (flet ((make-simple-var-list (x)
	     (mapcar (lambda (str) (make-var 'simple str)) x)))
    (if (some (lambda (x)
		(eq (first x) 'outer-product))
	      (rest body-genseq))
	(make-simple-var-list (cons "p" arg-list))
	(make-simple-var-list arg-list)))))

(defun genseq-avx (genseq register-width)
  (let ((body (fuse-loops (expand-genseq genseq))))
    (mapcar (lambda (x)
	      (genop-avx x register-width))
	    body)))

(defun genop-avx (x register-width)
  (if (eq (first x) 'genop-dag) (genop-dag-avx x register-width)
      (ecase (second x)
	(outer-product
	 (make-avx-outer-product (demand-list (getf x :inputs))
				 (demand-list (getf x :target))
				 register-width)))))

(defun genop-dag-avx (x register-width)
  (ecase (second x)
    (outer-product
     (if (common-inputs-p x)
	 (make-avx-outer-product (cadar (cddr x))
				 (mapcar #'first (cddr x))
				 register-width)
	 (error "Only common input dags implemented for now!")))))

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
	       (make-avx-fma inputs targets register-width))))

(defun make-avx-fma (inputs targets register-width)
  (let ((col-vec (first inputs))
	(row-vec (second inputs))
	(tarpre (matrix-float-size (first targets))))
    (make-block
     (matrix-load col-vec register-width)
     (make-block (interleave (matrix-load row-vec register-width)
			     (apply #'zip-lists
			      (mapcar (lambda (microtile)
					(matrix-fma microtile col-vec row-vec register-width))
				      targets))
			     :spacing (/ (* (matrix-rows col-vec)
					    tarpre
					    (length targets))
					 register-width)))
     (matrix-pointer-inc col-vec (matrix-rows col-vec))
     (matrix-pointer-inc row-vec (matrix-columns row-vec)))))

(defun make-avx-finalise (targets register-width)
  (apply #'make-block (mapcar (lambda (x)
				(matrix-store x register-width))
			      targets)))
