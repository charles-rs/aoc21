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
