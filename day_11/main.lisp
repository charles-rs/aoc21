(defun get-nums ()
  (with-open-file (stream "input")
    (let ((lst (loop :for line = (read-line stream nil)
		     :while line
		     :collect (mapcar (lambda (x) (list (digit-char-p x))) (coerce line 'list)))))
      (make-array (list (length lst)
			(length (car lst)))
		  :initial-contents lst))))

(defun clear-flashed (arr)
  (let* ((dims (array-dimensions arr))
	 (sum (loop :for i from 0 below (first dims)
		    :sum (loop :for j from 0 below (second dims)
			       :sum (if (cdr (aref arr i j))
					(progn (setf (aref arr i j) (cons 1 nil)) 1)
					(progn (setf (aref arr i j)
						     (cons (1+ (car (aref arr i j))) nil)) 0))))))
    (= sum (* (first dims) (second dims)))))


(defun neighbors (i j dims)
  (labels ((in-range (x y)
	     (and (>= x 0)
		  (>= y 0)
		  (< x (first dims))
		  (< y (second dims)))))
    (remove-if-not (lambda (pt) (in-range (car pt) (cdr pt)))
		   (list (cons (1+ i) j)
			 (cons (1- i) j)
			 (cons (1+ i) (1+ j))
			 (cons (1- i) (1+ j))
			 (cons (1+ i) (1- j))
			 (cons (1- i) (1- j))
			 (cons i (1+ j))
			 (cons i (1- j))))))

(defun step-1 (arr idx)
  (let ((dims (array-dimensions arr))
	(to-process nil))
    (labels ((handle-pt (i j)
	       (if (and (not (cdr (aref arr i j)))
			(> (car (aref arr i j)) 9))
		   (progn
		     (setf (aref arr i j) (cons 0 T))
		     (let ((new-pts
			     (remove-if (lambda (pt) (cdr (aref arr (car pt) (cdr pt))))
					(append to-process (neighbors i j dims)))))
		       (loop :for pt in new-pts
			     :do (setf (aref arr (car pt) (cdr pt))
				       (cons (1+ (car (aref arr (car pt) (cdr pt)))) NIL)))
		       (let ((num (loop :for pt in new-pts
					:sum (let ((tmp (handle-pt (car pt) (cdr pt))))
					       tmp))))
			 (1+ num))))
		   0)))
      (if (clear-flashed arr) (progn (format T "WINNER: ~d" (1- idx)) (terpri)))
      (loop :for i from 0 below (first dims)
	    :sum (loop :for j from 0 below (second dims)
		       :sum (handle-pt i j))))))



(defun parta (r)
  (let* ((arr (get-nums)))
    (loop :for i from 1 to r
	  :sum (step-1 arr i))))
