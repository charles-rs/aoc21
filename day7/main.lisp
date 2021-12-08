(ql:quickload "cl-ppcre")

(defun get-nums ()
  (with-open-file (stream "input")
      (mapcar 'parse-integer (cl-ppcre:split "," (read-line stream nil)))))

(defun parta (nums)
  (reduce 'min (loop :for i from 0 to (reduce 'max nums)
		     :collect (loop :for n in nums
				    :sum (abs (- i n))))))

(defun partb (nums)
  (reduce 'min (loop :for i from 0 to (reduce 'max nums)
		     :collect (loop :for n in nums
				    :sum (let ((tmp (abs (- i n))))
					   (/ (* tmp (1+ tmp)) 2))))))

(defun parta-fast (nums)
  (let* ((len (length nums))
	 (idx (aref (sort (make-array len :initial-contents nums) '<) (floor (/ len 2)))))
    (loop for n in nums sum (abs (- n idx)))))

(defun partb-fast (nums)
  (let ((idx (floor (/ (reduce '+ nums) (length nums)))))
    (loop for n in nums sum (let ((tmp (abs (- n idx)))) (/ (* tmp (1+ tmp)) 2)))))
