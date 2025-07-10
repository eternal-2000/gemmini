(require :representation)
(require :gen-utils)
(require :matrix-class)

(provide :c-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; C syntax - general
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun close-statement () (emit ";"))

(defun open-scope () (emit "{"))

(defun close-scope () (emit "}"))

(defun c-directive (x) (emit "#~a" x))

(defun c-indent (lines &optional (level 0))
  (assert (compose-call (or stringp consp) lines))
  (flet ((indent-string (line level)
	   (cat (make-string (* 4 level)
			     :initial-element #\Space)
		line)))
    (if (stringp lines) (indent-string lines level)
	(mapcar (lambda (x)
		  (indent-string x level))
		lines))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BLAS-specific C notation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod column-stride ((mat matrix)) (emit "ld~a" (matrix-name mat)))

(defmethod matrix-entry ((mat matrix) row column)
  (let ((mat-name (matrix-name mat)) (stride (column-stride mat)))
    (if (numberp row) (assert (member row (range (matrix-rows mat)))))
    (if (numberp column) (assert (member column (range (matrix-columns mat)))))
    (ecase (type-of mat)
      (column-vector (emit "~a[~a]" mat-name row))
      (row-vector (emit "~a[~a]" mat-name column))
      (matrix (if (and (numberp column) (= column 1))
		  (if (eq row 0) (emit "~a[~a]" mat-name stride)
		      (emit "~a[~a + ~a]" mat-name row stride))
		  (cond ((and (eq row 0) (eq column 0)) (emit "~a[0]" mat-name))
			((eq column 0) (emit "~a[~a]" mat-name row))
			((eq row 0) (emit "~a[~a * ~a]" mat-name column stride))
			(t (emit "~a[~a + ~a * ~a]" mat-name row column stride))))))))

(defmethod matrix-address ((mat matrix) row column) (emit "&~a" (matrix-entry mat row column)))

(defgeneric matrix-element-var (matrix row column)
  (:documentation "Denotes matrix elements: Matrices have two subscripts, unless they are
vectors, which will only have one index. For example, c_i_j may denote
elements of a matrix with more than one row and column, whereas a_i
may denote elements of a column vector."))

(defmethod matrix-element-var ((mat matrix) row column)
  (emit "~a_~a_~a" (string-downcase (matrix-name mat)) row column))

(defmethod matrix-element-var ((mat column-vector) row column)
  (assert (= column 0))
  (emit "~a_~a" (string-downcase (matrix-name mat)) row))

(defmethod matrix-element-var ((mat row-vector) row column)
  (assert (= row 0))
  (emit "~a_~a" (string-downcase (matrix-name mat)) column))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; From AST IR to C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c-comparison (comp-rep)
  (let ((x (c-var (third comp-rep)))
	(y (c-var (fourth comp-rep))))
    (ecase (second comp-rep)
      (L (emit "~a < ~a" x y))
      (LE (emit "~a <= ~a" x y))
      (G (emit "~a > ~a" x y))
      (GE (emit "~a >= ~a" x y))
      (E (emit "~a == ~a" x y)))))

(defun c-type (type-rep)
  (let ((name (second type-rep))
	(args (third type-rep)))
    (ecase name
      (void "void")
      (int (ecase (first args)
	     (32 "int")))
      (float (ecase (first args)
	       (32 "float")
	       (64 "double")))
      (packed-float (avx-intrinsic-type (first args) (second args))))))

(defun c-var (var-rep)
  (let ((name (second var-rep))
	(args (third var-rep)))
    (ecase name
      (simple (cond ((typep (first args) 'matrix) (matrix-name (first args)))
		    ((stringp (first args)) (emit (first args)))
		    (t (error "Simple variable of unimplemented type"))))
      (matrix-element (matrix-element-var (first args)
					  (second args)
					  (third args))))))

(defun c-dec (dec-rep &optional (level 0))
  (let ((type (c-type (second dec-rep)))
	(var (c-var (third dec-rep))))
    (c-indent (emit "~a ~a;" type var) level)))

(defun c-init (init-rep &optional (level 0))
  (let* ((type (c-type (second init-rep)))
	 (var (c-var (third init-rep)))
	 (val-rep (fourth init-rep))
	 (val (if (is-rep val-rep 'call) (c-function-call val-rep t)
		  (c-translate val-rep))))
    (c-indent (emit "~a ~a = ~a;" type var val) level)))

(defun c-memref (memref-rep)
  (let* ((name (second memref-rep))
	 (args (third memref-rep))
	 (mem-obj (first args)))
    (ecase name
      (address (if (typep mem-obj 'matrix)
		   (matrix-address mem-obj (second args) (third args))
		   (error "Addresses of type ~a not implemented" (type-of mem-obj))))
      (dereference (emit "~a*" (first args))))))

(defun c-pointer (pointer-rep) (emit "~a*" (c-type (cons 'type (cdr pointer-rep)))))

(defun c-assign (assign-rep &optional (level 0))
  (let* ((var (c-var (second assign-rep)))
	 (val-rep (third assign-rep))
	 (val (if (is-rep val-rep 'call) (c-function-call val-rep t)
		  (c-translate val-rep level))))
    (c-indent (emit "~a = ~a;" var val) level)))

(defun c-function-call (call-rep &optional nested (level 0))
  (labels ((nested-call (rep)
	     (let* ((name (second rep))
		    (args (third rep))
		    (args-no-memdata (butlast args 2))
		    (data-size (first (nthcdr (length args-no-memdata) args)))
		    (register-size (first (last args))))	       
	       (if (eq name 'inc) (let ((var (c-var (first args)))
					(increment (second args)))
				    (if (= increment 1) (emit "++~a" var)
					(emit "~a += ~a" var increment)))
		   (emit "~a(~{~a~^, ~})"
			 (cond ((member name *avx-dict*)
				(avx-intrinsic-op name data-size register-size))
			       (t (error "Function ~a not implemented yet" name)))
			 (mapcar (lambda (x)
				   (cond ((is-rep x 'call) (nested-call x))
					 ((is-rep x 'memref) (c-memref x))
					 ((is-rep x 'var) (c-var x))
					 (t x)))
				 args-no-memdata))))))
    (if nested (nested-call call-rep)
	(c-indent (cat (nested-call call-rep) ";") level))))

(defun c-block (block-rep &optional (level 0))
  (cat (reduce-string-list
	(flatten
	 (mapcar (lambda (x)
		   (cond ((is-instruction x) (c-translate x level))
			 ((and (consp x)
			       (every #'is-instruction x))
			  (mapcar (lambda (y)
				    (c-translate y level))
				  x))
			 (t (error "Received invalid op-block form"))))
		 (rest block-rep))))
       (line-break)))

(defun c-build-for-loop (loop-rep &optional (level 0))
  (let ((type (second loop-rep))
	(index (third loop-rep))
	(init-value (fourth loop-rep))
	(stop-cond (first (nthcdr 4 loop-rep)))
	(update (first (nthcdr 5 loop-rep)))
	(loop-body (first (nthcdr 6 loop-rep))))
    (c-indent (emit "for (~a ~a = ~a; ~a; ~a)~a~%~a~%~a"
		    (c-type type)
		    (c-var index)
		    init-value
		    (c-comparison stop-cond)
		    (c-function-call update t)
		    (open-scope)
		    (c-translate loop-body (+ 1 level))
		    (c-indent (close-scope) level))
	      level)))

(defun c-translate (form &optional (level 0) nested)
  "Maps AST from list to C code string"
  (if (is-instruction form)
      (ecase (first form)
	(comp (c-comparison form))
	(type (c-type form))
	(pointer (c-pointer form))
	(dec (c-dec form level))
	(assign (c-assign form level))
	(init (c-init form level))
	(call (c-function-call form nested level))
	(for-loop (c-build-for-loop form level))
	(op-block (c-block form level)))
      (mapcar (lambda (x)
		(c-translate x (+ 1 level) nested))
	      form)))

(defun c-fn-def (fn-def-rep)
  (let ((return-type (c-type (second fn-def-rep)))
	(fn-name (third fn-def-rep))
	(signature (mapcar (lambda (x)
			     (cond ((is-rep x 'type) (c-type x))
				   ((is-rep x 'pointer) (c-pointer x))
				   (t (error "Signature has invalid element ~a" x))))
			   (fourth fn-def-rep)))
	(arg-list (mapcar #'c-var (fifth fn-def-rep)))
	(body (reduce-string-list
	       (flatten
		(c-translate (sixth fn-def-rep))))))
    (emit "~a ~a(~{~a ~a~^, ~}){~%~a~%}"
	  return-type
	  fn-name
	  (mapcan #'list signature arg-list)
	  body)))

(defun c-prototype (fn-def-rep)
  (let ((return-type (c-type (second fn-def-rep)))
	(fn-name (third fn-def-rep))
	(signature (mapcar (lambda (x)
			     (cond ((is-rep x 'type) (c-type x))
				   ((is-rep x 'pointer) (c-pointer x))
				   (t (error "Signature has invalid element ~a" x))))
			   (fourth fn-def-rep))))
    (emit "~a ~a(~{~a~^, ~});" return-type fn-name signature)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Headers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun def-directive (variable &optional value)
  (let ((rval (if value value "")))
    (c-directive (cat "define " variable " " rval))))

(defun c-include (header &optional (angle t))
  (let ((include-dir (c-directive "include"))
	(lib (if angle (emit "<~a.h>~%" header)
		 (emit " \"~a.h\"~%" header))))
  (cat include-dir lib)))

(defun c-headers (header-pairs)
  "Generates #include directives from list of dotted pairs (header-name . angle-bool)"
  (mapcar (lambda (pair)
	    (c-include (first pair) (rest pair)))
	  header-pairs))

(defun c-include-guard (header-name file-body)
  (let ((guard (emit "~a_H" (string-upcase header-name))))
    (cat
     (c-directive "ifndef ") guard (line-break)
     (def-directive guard) (line-break)
     file-body (line-break)
     (c-directive "endif"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; AVX conventions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun avx-intrinsic-op (op data-size register-size)
  (let ((prefix (cond ((= register-size *ymm-size*) *avx2-prefix*)
		      ((= register-size *zmm-size*) *avx512-prefix*)
		      (t (error "Registers of size ~d not supported" register-size))))
        (suffix (cond ((and (= data-size *double-size*) (eq op 'broadcast)) *sd-suffix*)
		      ((= data-size *double-size*) *pd-suffix*)
		      ((and (= data-size *single-size*) (eq op 'broadcast)) *ss-suffix*)
		      ((= data-size *single-size*) *ps-suffix*)
		      (t (error "Data size ~d not supported" data-size)))))
    (cat prefix (symbol-to-parameter op) suffix)))

(defun avx-intrinsic-type (data-size register-size)
  (cond ((and (= register-size *ymm-size*) (= data-size *double-size*)) *ymm-pd*)
	((and (= register-size *ymm-size*) (= data-size *single-size*)) *ymm-ps*)
	((and (= register-size *zmm-size*) (= data-size *double-size*)) *zmm-pd*)
	((and (= register-size *zmm-size*) (= data-size *single-size*)) *zmm-ps*)
	(t (error "Only supports YMM or ZMM registers, and single or double precision floats"))))
