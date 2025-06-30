(require :datatypes)
(require :macro-utils)
(provide :gen-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun range (end &optional (start 0) (step 1))
  (if (< start end) (cons start (range end (+ step start) step))))

(defun emit (format-pattern &rest args) (apply #'format nil format-pattern args))

(defun cat (&rest args) (apply #'concatenate 'string args))
  
(defun interleave-lists (a b spacing &optional initial-list)
  "Interleaves elements of a and b with a given spacing and appends to optional initial list.
Creates new list by taking one element of a, then SPACING elements of b,
 and iterating until all elements of a and b have been used."
  (labels ((interleave-iterator (a b count-b accumulator)
             (cond ((and (null a) (null b)) accumulator)
		   ((null b) (append accumulator a))
		   ((null a) (append accumulator b))
		   ((= count-b 0)
		    (interleave-iterator (rest a) b spacing
					 (append accumulator (list (first a)))))
		   (t (interleave-iterator a (rest b) (- count-b 1)
					   (append accumulator (list (first b))))))))
    (interleave-iterator a b 0 initial-list)))

(defun positivep (x) (and (realp x) (> x 0)))

(defun posintp (x) (compose-call (and positivep integerp) x))

(defun symbol-to-parameter (sym) (symbol-value (intern (emit "*~a*" (symbol-name sym)))))

(defun flatten (x)
  (labels ((recurse (x accumulator)
	     (cond ((null x) accumulator)
		   ((atom x) (cons x accumulator))
		   (t (recurse (first x) (recurse (rest x) accumulator))))))
    (recurse x nil)))

(defun reduce-string-list (x) (reduce (lambda (a b) (cat a (line-break) b)) x))

(defun line-break () (string #\Newline))
