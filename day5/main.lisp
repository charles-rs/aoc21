(ql:quickload "cl-ppcre")

(defun delimiterp (c) (char= c #\Space))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))


(defun get-nums ()
  (with-open-file (stream "mine")
    (loop :for line = (let ((ln (read-line stream nil)))
			(if ln (setq width (length ln)))
			ln)
	  :while line
	  :collect (mapcar (lambda (p) (mapcar 'parse-integer (cl-ppcre:split "," p)))
			   (cl-ppcre:split " -> " line)))))

(defvar size 1000)

(defun sign (num)
  (/ num (abs num)))

(defun parta (nums)
  (let* ((arr (make-array (list size size) :initial-element 0))
	 (count 0))
    (loop :for line in nums
	  :do (cond ((= (caar line) (caadr line))
		     (progn
		       (loop :for i from (min (cadar line) (cadadr line)) to (max (cadar line) (cadadr line))
			     :do (setf (aref arr i (caar line))
				       (1+ (aref arr i (caar line)))))))
		    ((= (cadar line) (cadadr line))
		     (progn
		       (loop :for i from (min (caar line) (caadr line)) to (max (caar line) (caadr line))
			     :do (setf (aref arr (cadar line) i)
				       (1+ (aref arr (cadar line) i))))))))
    (loop :for i from 0 to (1- size)
	  :do (loop :for j from 0 to (1- size)
		    :do (if (> (aref arr i j) 1)
			    (setf count (1+ count)))))
    count))

(defun partb (nums)
  (let* ((arr (make-array (list size size) :initial-element 0))
	 (count 0))
    (loop :for line in nums
	  :do (cond ((= (caar line) (caadr line))
		     (progn
		       (loop :for i from (min (cadar line) (cadadr line)) to (max (cadar line) (cadadr line))
			     :do (setf (aref arr i (caar line))
				       (1+ (aref arr i (caar line)))))))
		    ((= (cadar line) (cadadr line))
		     (progn
		       (loop :for i from (min (caar line) (caadr line)) to (max (caar line) (caadr line))
			     :do (setf (aref arr (cadar line) i)
				       (1+ (aref arr (cadar line) i))))))
		    ((let ((sum (mapcar (lambda (x y) (abs (- x y))) (car line) (cadr line))))
		       (= (car sum) (cadr sum)))
		     (progn
		       (let ((cnt (copy-list (car line)))
			     (xincr (sign (- (caadr line) (caar line))))
			     (yincr (sign (- (cadadr line) (cadar line)))))
			 (loop :while (not (equal cnt (cadr line)))
			       :do (progn
				     (setf (aref arr (cadr cnt) (car cnt))
					   (1+ (aref arr (cadr cnt) (car cnt))))
				     (setf (car cnt) (+ xincr (car cnt)))
				     (setf (cadr cnt) (+ yincr (cadr cnt)))))
			 (setf (aref arr (cadadr line) (caadr line))
			       (1+ (aref arr (cadadr line) (caadr line)))))))))

    (loop :for i from 0 to (1- size)
	  :do (loop :for j from 0 to (1- size)
		    :do (if (> (aref arr i j) 1)
			    (setf count (1+ count)))))
    count))
