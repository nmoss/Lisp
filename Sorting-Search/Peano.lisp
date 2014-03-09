;;;; Peano Arithmetic

;;; iterative recursion
;;; time: O(n) space: O(1)
(defun plus (x y)
	(if (eql x 0)
		y
		(plus (1- x) (1+ y))))

;;; linear recursion
;;; time: O(n) space: O(n)
(defun plus (x y)
	(if (eql x 0)
		y
		(1+ (plus (1- x) y))))
