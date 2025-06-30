(provide :macro-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General macro utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-gensyms (syms &body body)
  "Binds SYMS to gensyms in BODY"
  `(let ,(mapcar (lambda (s) `(,s (gensym))) syms) ,@body))

(defmacro ecase-str (keyform &body cases)
  "Variant of ECASE which matches string (instead of symbol) KEYFORM"
  (with-gensyms (k)
    `(let ((,k ,keyform))
       (cond ,@(mapcar (lambda (c)
			 `((string= ,k ,(first c)) ,@(rest c)))
		       cases)
	     (t (error "~S fell through ECASE-STR expression. Wanted one of ~{~S~^, ~}"
		       ,k ',(mapcar #'first cases)))))))

;;;; Function composition

(defmacro compose-call (expr args)
  "Calls (COMPOSE (EXPR)) on ARGS"
  `(funcall ,(macroexpand `(compose ,expr)) ,args))

(defmacro compose (expr)
  "Returns a function defined as the composition of functions in EXPR"
  `#',(build-recurse expr))

(defun build-recurse (expr)
  (if (or (atom expr) (eq (first expr) 'lambda)) expr
      (build-call (first expr) (rest expr))))

(defun build-call (op fnlist)
  (with-gensyms (g)
    `(lambda (,g)
       (,op ,@(mapcar (lambda (f)
			`(,(build-recurse f) ,g))
		      fnlist)))))

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
