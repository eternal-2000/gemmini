(require :microkernel-gen)
(require :header-gen)
(require :uiop)

(defun write-c-file (name body directory &key (filetype 'C))
  (let ((ext (ecase filetype
	       (C ".c")
	       (H ".h"))))
  (with-open-file (stream (cat directory name ext)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "~A~%" body))))

(defun write-blas ()
  "Writes microkernels to files. To be updated as BLAS and language functionalities are added"
  (let* ((raw-args (uiop:command-line-arguments))
	 (args (if (and raw-args (string= (first raw-args) "--"))
		   (rest raw-args)
		   raw-args))
	 (blas-op (first args))
	 (mr (parse-integer (second args)))
	 (nr (parse-integer (third args)))
	 (float-size (parse-integer (fourth args)))
	 (register-size (parse-integer (fifth args)))
	 (mk-fn-def-rep (make-mk-gemm (make-instance 'matrix
						     :name "C"
						     :rows mr :columns nr
						     :float-size float-size)
				      (make-instance 'column-vector
						     :name "A"
						     :rows mr :columns 1
						     :float-size float-size)
				      (make-instance 'row-vector
						     :name "B"
						     :rows 1 :columns nr
						     :float-size float-size)
				      :register-size register-size)))
    (cond ((not (= (mod mr 4) 0))
	   (error "Number of rows in microtile should be multiple of 4 (for now)"))
	  ((not (and (> mr 0) (> nr 0)))
	   (error "Negative number passed as dimension of microtile"))
	  (t (write-c-file (mk-name blas-op :float-size float-size)
			   (cat (c-include "immintrin")
				(c-fn-def mk-fn-def-rep))
			   "../src/kernels/")
	     (write-c-file (mk-name blas-op :float-size float-size)
			   (mk-header blas-op mk-fn-def-rep :float-size float-size)
			   "../include/kernels/"
			   :filetype 'H)))))
