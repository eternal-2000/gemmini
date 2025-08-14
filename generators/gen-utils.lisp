(require :datatypes)
(require :macro-utils)
(provide :gen-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun range (end &optional (start 0) (step 1))
  "Makes arithmetic sequence of integers with initial element START, final element END, and
constant difference STEP."
  (if (< start end)
      (cons start (range end (+ step start) step))))

(defun zip-lists (&rest lists)
  "Merges LISTS into new list by taking first element of each list in order, then second,
and so on until at least one of LISTS is NIL."
  (if (some #'null lists) nil
      (append (mapcar #'first lists)
	      (apply #'zip-lists (mapcar #'rest lists)))))

(defun interleave (a b &key (spacing 1) (initial-list nil))
  "Interleaves elements of A and B with a given spacing and appends to optional initial list.
Creates new list by taking one element of A, then SPACING elements of B, and iterating until
all elements of A and B have been used."
  (labels ((alternate-append (x y accumulator)
             (cond ((and (null x) (null y)) accumulator)
		   ((null y) (append accumulator x))
		   ((null x) (append accumulator y))
		   (t (alternate-append (rest x) (rest y)
					(append accumulator (cons (first x)
								  (first y))))))))
    (alternate-append a (group b spacing) initial-list)))

(defun group (list n)
  "Groups consecutive elements of LIST into nested sublists of length N."
  (assert (posintp n))
  (labels ((recurse (list &optional accumulator)
	     (let ((rem (nthcdr n list)))
	       (if (consp rem) (recurse rem (cons (subseq list 0 n) accumulator))
		   (nreverse (cons list accumulator))))))
    (if list (recurse list) nil)))

(defun group-consec (list pred)
  "Groups all consecutive elements of LIST which satisfy PRED into sublists."
  (labels ((collect-group (accumulator rem)
             (if (and rem (funcall pred (first rem)))
                 (collect-group (cons (first rem) accumulator) (rest rem))
                 (values (nreverse accumulator) rem))))
    (when list
      (if (funcall pred (first list))
          (multiple-value-bind (group rem)
	      (collect-group (list (first list)) (rest list))
            (cons (if (rest group) group
		      (first group))
		  (group-consec rem pred)))
          (cons (first list) (group-consec (rest list) pred))))))

(defun map-group-consec (fn list pred)
  "Groups all consecutive elements of LIST which satisfy PRED into sublists.
Then applies FN to sublists constructed by GROUP-CONSEC."
  (mapcar (lambda (x)
	    (if (and (consp x)
		     (every pred x))
		(funcall fn x)
		x))
	  (group-consec list pred)))

(defun flatten (x)
  "Returns a flat list X from any list X, i.e. any nested lists will be denested."
  (labels ((recurse (x &optional accumulator)
	     (cond ((null x) accumulator)
		   ((atom x) (cons x accumulator))
		   (t (recurse (first x)
			       (recurse (rest x) accumulator))))))
    (recurse x)))

(defun filter (fn list)
  "Applies filter FN to LIST."
  (let ((accumulator nil))
    (dolist (element list)
      (let ((value (funcall fn element)))
	(if value (push value accumulator))))
    (nreverse accumulator)))

(defun filter-out-list (list filter-list)
  "Returns list of all elements in LIST except those which appear in FILTER-LIST."
  (remove-if (lambda (x)
	       (member x filter-list))
	     list))

(defun mapzip (fn-list arg-list)
  "Applies the n-th element of FN-LIST to the n-th element of ARG-LIST until the shorter
list is exhausted."
  (mapcar #'funcall fn-list arg-list))

(defun total-length (&rest lists)
  "Returns the sum of the lengths of all lists in argument LISTS."
  (reduce #'+ (mapcar #'length lists)))

(defun all-identical (list &key (test #'equal))
  "Checks whether every element in LIST is identical according to TEST."
  (or (null list)
      (every (lambda (x) (funcall test x (first list))) (rest list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Type and value checking 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun positivep (x)
  "For real X, returns T if X is positive, or NIL otherwise."
  (> x 0))

(defun posintp (x)
  "Returns T if X is a positive integer, NIL otherwise."
  (and (integerp x) (positivep x)))

(defun every-typep (list type)
  "Checks whether every element in LIST is of given TYPE."
  (every (lambda (x) (typep x type)) list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Strings and symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit (format-pattern &rest args)
  "Applies FORMAT with NIL stream to FORMAT-PATTERN with arguments ARGS."
  (apply #'format nil format-pattern args))

(defun cat (&rest args)
  "Concatenates ARGS as STRINGS."
  (apply #'concatenate 'string args))

(defun reduce-string-list (x)
  "Reduces list X of strings to a single concatenated string."
  (reduce (lambda (a b)
	    (cat a
		 (line-break)
		 b))
	  x))

(defun flatten-to-string (x)
  "Flattens list X, then attempts to reduce to a single concatenated string."
  (reduce-string-list (flatten x)))

(defun line-break ()
  "A line break."
  (string #\Newline))

(defun symbol-to-parameter (sym)
  "Converts symbol SYM to parameter *SYM*."
  (symbol-value (intern (emit "*~a*" (symbol-name sym)))))
