(require :gen-utils)

(provide :representation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Intermediate representations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-block (&rest ops)
  (assert (every (lambda (x)
                   (or (and (is-instruction x)
			    (not (is-special-form x)))
                       (and (consp x)
                            (every #'is-instruction x))))
                 ops))
  (if (and (= (length ops) 1)
	   (is-rep (first ops) 'op-block))
      (first ops)
      (cons 'op-block ops)))

(defun make-init (type var val)
  (assert-all (is-rep type 'type)
	      (is-identifier var)
	      (compose-call (or numberp is-funarg) val))
  (list 'init type var val))

(defun make-dec (type var)
  (assert-all (is-rep type 'type)
	      (is-identifier var))
  (list 'dec type var))

(defun make-assign (var val)
  (assert-all (is-identifier var)
	      (compose-call (or numberp is-funarg) val))
  (list 'assign var val))

(defun make-comp (comparison x y)
  (assert (member comparison '(L G LE GE E)))
  (list 'comp comparison x y))

(defun make-type (type &rest args)
  (assert (member (cons type args) *type-dict* :test #'equal))
  (list 'type type args))

(defun make-pointer (type &rest args)
  (assert (member (cons type args) *type-dict* :test #'equal))
  (list 'pointer type args))

(defun make-var (var &rest args)
  (assert (member (cons var (length args)) *var-dict* :test #'equal))
  (list 'var var args))

(defun make-call (fn &rest args)
  (assert-all (member (cons fn (length args)) *fn-dict* :test #'equal)
	      (mapcar (compose (or numberp is-funarg)) args))
  (list 'call fn args))

(defun make-memref (op &rest args)
  (assert (member (cons op (length args)) *memref-dict* :test #'equal))
  (list 'memref op args))

(defun make-fn-def (return-type name signature arg-list body)
  (assert-all (or (is-rep return-type 'type)
		  (is-rep return-type 'pointer))
	      (stringp name)
	      (and (listp signature)
		   (every (lambda (x)
			    (or (is-rep x 'type)
				(is-rep x 'pointer)))
			  signature))
	      (and (listp arg-list)
		   (= (length signature) (length arg-list))
		   (every #'is-funarg arg-list))
	      (or (is-instruction body)
		  (every (lambda (x)
			   (or (is-instruction x)
			       (and (consp x)
				    (every #'is-instruction x))))
			 body)))
  (list 'fn-def return-type name signature arg-list body))

(defun make-loop (type index init-value stop-cond update body)
  (assert-all (is-rep type 'type)
	      (or (stringp index) (is-rep index 'var))
	      (integerp init-value)
	      (is-rep stop-cond 'comp)
	      (is-rep update 'call)
	      (or (is-instruction body)
		  (every #'is-instruction body)))
  (list 'for-loop type index init-value stop-cond update body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Genops:
;;;;
;;;; A generic operation (genop) is defined here as a list, tagged
;;;; with GENOP, which contains the minimal amount of information
;;;; needed such that if given a device architecture, it could be
;;;; efficiently implemented. It can be thought of as a kind of 'lazy'
;;;; representation in the sense that it describes what to do, but not
;;;; how to do it yet; we defer the specific details until we are
;;;; forced to choose a specific architecture.
;;;;
;;;; A genseq is a sequence of genops (to be implemented) for
;;;; notational convenience.
;;;;
;;;; Gendef will denote a generic definition of a function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-gendef (fn-name inputs targets &rest body)
  (list :gendef fn-name
	:inputs (demand-list inputs)
	:targets (demand-list targets)
	:body (apply #'make-genseq body)))

(defun make-genseq (genops)
  (cons 'genseq genops))

(defun expand-genseq (genseq)
  (mapcar (lambda (x)
	    (apply #'make-genop x))
	 (rest genseq)))

(defun make-genop (name &optional inputs target)
  (list 'genop name
	:inputs (demand-list inputs)
	:target target))

(defun demand-list (x)
  (if (listp x) x
      (list x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Identifying representations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-rep (x tag) (and (consp x) (eq (first x) tag)))

(defun is-genop (x) (is-rep x 'genop))

(defun is-genseq (x) (is-rep x 'genseq))

(defun is-genop-dag (x) (is-rep x 'genop-dag))

(defun is-instruction (x) (is-rep-one-of x *op-dict*))

(defun is-funarg (x) (is-rep-one-of x *funarg-options*))

(defun is-identifier (x)
  (or (is-rep x 'var)
      (and (is-rep x 'memref) (eq (second x) 'dereference))))

(defun is-rep-one-of (x tag-list) (and (consp x) (member (first x) tag-list)))

(defun is-special-form (x) (member (first x) *special-tags*))
