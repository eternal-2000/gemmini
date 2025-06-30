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
