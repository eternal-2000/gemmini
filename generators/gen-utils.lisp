(require :datatypes)
(require :macro-utils)
(provide :gen-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun range (end &optional (start 0) (step 1))
  (if (< start end)
      (cons start (range end (+ step start) step))))
  
(defun interleave (a b &key (spacing 1) (initial-list nil))
  "Interleaves elements of a and b with a given spacing and appends to optional initial list.
Creates new list by taking one element of a, then SPACING elements of b,
 and iterating until all elements of a and b have been used."
  (labels ((alternate-append (x y accumulator)
             (cond ((and (null x) (null y)) accumulator)
		   ((null y) (append accumulator x))
		   ((null x) (append accumulator y))
		   (t (alternate-append (rest x) (rest y)
					(append accumulator (cons (first x)
								  (first y))))))))
    (alternate-append a (group b spacing) initial-list)))

(defun group (x n)
  (assert (posintp n))
  (labels ((recurse (x &optional accumulator)
	     (let ((rem (nthcdr n x)))
	       (if (consp rem) (recurse rem (cons (subseq x 0 n) accumulator))
		   (nreverse (cons x accumulator))))))
    (if x (recurse x) nil)))

(defun flatten (x)
  (labels ((recurse (x &optional accumulator)
	     (cond ((null x) accumulator)
		   ((atom x) (cons x accumulator))
		   (t (recurse (first x)
			       (recurse (rest x) accumulator))))))
    (recurse x)))

(defun mapzip (fn-list arg-list) (mapcar #'funcall fn-list arg-list))

(defun total-length (&rest lists) (reduce #'+ (mapcar #'length lists)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Type and value checking 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun positivep (x) (> x 0))

(defun posintp (x) (and (integerp x) (positivep x)))

(defun every-typep (list type) (every (lambda (x) (typep x type)) list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Strings and symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit (format-pattern &rest args) (apply #'format nil format-pattern args))

(defun cat (&rest args) (apply #'concatenate 'string args))

(defun reduce-string-list (x)
  (reduce (lambda (a b)
	    (cat a
		 (line-break)
		 b))
	  x))

(defun flatten-to-string (x) (reduce-string-list (flatten x)))

(defun line-break () (string #\Newline))

(defun symbol-to-parameter (sym) (symbol-value (intern (emit "*~a*" (symbol-name sym)))))
