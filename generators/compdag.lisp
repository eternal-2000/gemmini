(require :gen-utils)
(require :matrix-class)

(provide :compdag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Computational dag structures
;;;;
;;;; A computation will be represented by a directed acyclic graph
;;;; (dag). E.g. the operation C += AB will be represented by a graph
;;;; with vertices A, B, C, and edges AC, BC.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-genop-dag (op-tag genops)
  "Makes GENOP-DAG representation from GENOPS with common OP-TAG.
Used in IR for optimisations, e.g. loop fusion."
  (cons 'genop-dag
	(cons op-tag
	      (row-sort-dag (make-dag op-tag
				      genops)))))

(defun make-dag (op-tag genops)
  "List representation of computational dag of all OP-TAGs in GENOPS.
CAR of elements in result are outputs, CADR of elements are inputs."
  (filter (lambda (x)
	    (if (eq (second x) op-tag)
		(list (getf x :target) (getf x :inputs))))
	  genops))

(defun row-sort-dag (dag)
  "Sorts DAG argument by number of rows in matrix output, from highest to lowest.
In the case of ties, sorts by columns, then sorts by alphabetical order.
Non-destructive."
  (sort (copy-list dag)
	(lambda (x y)
	  (let* ((x-target (first x))
		 (y-target (first y))
		 (x-rows (matrix-rows x-target))
		 (x-cols (matrix-columns x-target))
		 (y-rows (matrix-rows y-target))
		 (y-cols (matrix-columns y-target)))
	    (if (> x-rows y-rows) t
		(if (> x-cols y-cols) t
		    (string< (matrix-name x-target)
			     (matrix-name y-target))))))))

(defun edge-list (dag)
  "Edge list of DAG.
CAR elements are target vertices. CDR elements are source vertices."
  (cddr dag))

(defun common-inputs-p (dag)
  "Checks whether DAG has a set of targets which all share a common input set."
  (all-identical (mapcar #'second (edge-list dag))))

;; (defun dag-shared-inputs (dag)
;;   "From DAG, constructs association list where CAR is a target or list of targets
;;  and CDR is an input or list of inputs."
;;   (error "Not implemented yet!"))
