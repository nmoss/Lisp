;;;; Search.lisp
;;;; Nicholas Moss
;;;; Implementation of Binary Search and Quick Sort 

(defvar *x* (vector 4 3 2 6 7))
(defvar *Y* (vector 7 6 3 2 1))
(defvar *W* (vector 9 8 7 6 3))
(defvar *Z* (vector 0 9 8 7 6 5 2 1 3 4))
(defvar *D* (vector 0 2 2 2 3 2 2 1)) ; not sure what's going on with this guy

(defun binary-search (vect key left right)
	(if (< right left)(return-from binary-search nil))
	(let ((mid (midpoint left right)))
		(cond ((> (elt vect mid) key)(binary-search vect key left (1- mid)))
					((< (elt vect mid) key)(binary-search vect key (1+ mid) right))
					(t mid))))

(defun midpoint (left right)
	(ceiling (/ (+ left right) 2)))

(defun swap (lst left right)
	(let ((x (elt lst left)))
		(setf (elt lst left) (elt lst right))
		(setf (elt lst right) x)))

;;; sort vect[left..right]
;;; In-place version of the algorithm 
(defun quick-sort (vect left right)
	(when (> right left)
		(let ((q (partition vect left right)))
			(quick-sort vect left (1- q))
			(quick-sort vect (1+ q) right))) vect)

(defun partition (vect left right)
	(let ((p (elt vect left)) (i left) (j right))
		(loop
			(loop
				(if (>= (elt vect i) p)
					(return))
				(if (>= i right)
					(return))
				(setf i (1+ i))) ; loop
			(loop 
				(if (<= (elt vect j) p)
					(return))
				(if (<= j left)
					(return))
				(setf j (1- j))) ; loop
			(if (>= i j)(return) (swap vect i j)))
		(if (eql j left)
			(return-from partition j)) j))


(quick-sort *x* 0 4)
