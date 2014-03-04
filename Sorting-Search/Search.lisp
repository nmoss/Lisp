;;; Search.lisp
;;; Nicholas Moss
;;; Implementation of Binary Search and Quick Sort 

(defvar *x* (vector 4 3 2 6 7))
(defvar *Y* (vector 7 6 3 2 1))
(defvar *W* (vector 9 8 7 6 3))

(defun swap (lst left right)
	(let ((x (elt lst left)))
		(setf (elt lst left) (elt lst right))
		(setf (elt lst right) x)))

;;; sort vect[left..right]
(defun quick-sort (vect left right)
	(when (>= right left)
		(let ((q (partition vect left right)))
			(format t "~d ~%" q)
			(quick-sort vect left (1- q))
			(quick-sort vect (1+ q) right))) vect)

(defun partition (vect left right)
	(let ((p (elt vect left)) (i left) (j right))
		(format t "~d ~d ~d ~%" p i j)
		(loop
			(loop
				(if (>= (elt vect i) p)
					(return))
				(if (> i right)
					(return))
				(setf i (1+ i))) ; loop
			(loop 
				(if (<= (elt vect j) p)
					(return))
				(if (< j left)
					(return))
				(setf j (1- j))) ; loop
			(if (>= i j)(return) (swap vect i j))
			(format t "Swapped in the loop called ~%")
			(format t "~d ~d ~%" i j))
		(if (eql j left)
			(return-from partition j))
		(format t "Swapped at the end called ~%")
		(format t "~d ~d ~%" left j)
		i))


(quick-sort *x* 0 4)
