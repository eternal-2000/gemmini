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

(defun make-blas-op (blas-sym)
  (ecase-str blas-sym
      ("gemm" #'make-gemm)))

(defun write-blas ()
  "Writes microkernels to files. To be updated as BLAS and language functionalities are added"
  (let* ((raw-args (uiop:command-line-arguments))
	 (args (if (and raw-args (string= (first raw-args) "--")) (rest raw-args)
		   raw-args))
	 (blas-class (make-blas-op (first args)))
	 (mr (parse-integer (second args)))
	 (nr (parse-integer (third args)))
	 (input-precision (parse-integer (fourth args)))
	 (target-precision input-precision) ;; To be updated
	 (register-width (parse-integer (fifth args)))
	 (blas-op (funcall blas-class
			   input-precision
			   target-precision
			   (list 4096 128 256 mr nr))) ;; Temporary parameters - to be updated
	 (mk-name (microkernel-name blas-op)))
    (cond ((not (= (mod mr 4) 0))
	   (error "Number of rows in microtile should be multiple of 4 (for now)"))
	  ((not (and (> mr 0) (> nr 0)))
	   (error "Negative number passed as dimension of microtile"))
	  (t (write-c-file mk-name
			   (cat (c-include "immintrin")
				(c-fn-def (make-avx-microkernel blas-op register-width)))
			   "../src/kernels/")
	     (write-c-file mk-name
			   (mk-header blas-op register-width)
			   "../include/kernels/"
			   :filetype 'H)))))
