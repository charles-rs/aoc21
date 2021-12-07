(ql:quickload "cl-ppcre")

(defun get-nums ()
  (with-open-file (stream "input")
    (let ((ln (read-line stream nil))
	  (vec (make-array 0 :fill-pointer 0 :adjustable t)))
      (mapcar (lambda (x) (vector-push-extend (parse-integer x) vec))
	      (cl-ppcre:split "," ln))
      vec)))

;;stupid stupid stupid lol
(defun step-1 (vec)
  (let ((len (length vec)))
    (loop :for i from 0 below len
	  :do (if (= 0 (aref vec i))
		  (progn (setf (aref vec i) 6)
			 (vector-push-extend 8 vec))
		  (setf (aref vec i) (1- (aref vec i)))))))

(defun parta (vec)
  (loop for i from 1 to 80
	do (step-1 vec))
  (length vec))

(defun partb (vec)
  (let ((arr (make-array 9 :initial-element 0 :adjustable t)))
    (loop :for i across vec
	  :do (setf (aref arr i) (1+ (aref arr i))))
    (loop :for i from 1 to 256
	  :do (let ((head (aref arr 0)))
		(setq arr (make-array 8 :adjustable t :initial-contents
			   (remove-if (lambda (x) t) arr :start 0 :end 1)))
		(setf (aref arr 6) (+ (aref arr 6) head))
	        (vector-push-extend head arr)))
    (loop :for i across arr
	  :sum i)))
