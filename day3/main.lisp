(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (let ((ln (read-line stream nil)))
			(if ln (setq width (length ln)))
			ln)
	  :while line
	  :collect line)))


(defun parta (nums)
  (setq eps (make-array width :initial-element 0))
  (setq len (length nums))
  (loop :for i in nums
	:do (loop :for j from 0 to (1- width)
		  :do (setf (aref eps j)
			    (+ (aref eps j) (- (char-int (char i j)) 48)))))
  (setq reseps 0)
  (setq resdel 0)
  (print eps)
  (setq eps (map 'vector (lambda (i) (if (< 500 i) 0 1)) eps))
  (print eps)
  (loop for i across eps
	:do
	   (progn (setq reseps (+ (* 2 reseps) i))
		  (setq resdel (+ (* 2 resdel) (abs (1- i))))))
  (* reseps resdel))

(defun most-common (nums pos)
  (let ((ones (loop :for i in nums
		    :sum (- (char-int (char i pos)) 48))))
    (print ones)
    (print (/ (length nums) 2))
    (if (<= (/ (length nums) 2) ones)
	#\1 #\0)))

(defun oxy (nums)
  (let* ((tmp nums)
	 (oxy (loop :named outer
		    :for i from 0 to (1- width)
		    :do (let ((most (most-common tmp i)))
			  (setq tmp (remove-if-not (lambda (n) (char= (char n i) most)) tmp))
			  (print tmp)
			  (print most)
			  (if (= 1 (length tmp))
			      (return-from outer tmp)))))
	 (tmp nums)
	 (co2 (loop :named outer
		    :for i from 0 to (1- width)
		    :do (let ((most (most-common tmp i)))
			  (setq tmp (remove-if-not (lambda (n) (char/= (char n i) most)) tmp))
			  (if (= 1 (length tmp))
			      (return-from outer tmp))))))
    (setq oxytmp 0)
    (print oxy)
    (print co2)
    (loop for i across (car oxy)
	  :do
	     (progn (setq oxytmp (+ (* 2 oxytmp) (- (char-int i) 48)))))
    (setq co2tmp 0)
    (loop for i across (car co2)
	  :do
	     (progn (setq co2tmp (+ (* 2 co2tmp) (- (char-int i) 48)))))
    (* oxytmp co2tmp)))
