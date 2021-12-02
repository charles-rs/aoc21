(defun delimiterp (c) (char= c #\Space))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))



(defun calc-pos ()
  (let ((dir (with-open-file (stream "input")
	       (loop :for line = (read-line stream nil)
		     :while line
		     :collect (let ((lst (my-split line)))
				(cons (read-from-string (car lst))
				      (parse-integer (cadr lst)))))))
	(pos (cons 0 0)))
    (loop :for com in dir
	  :do (cond ((equal (car com) 'forward) (setf (car pos) (+ (car pos) (cdr com))))
		    ((equal (car com) 'up) (setf (cdr pos) (- (cdr pos) (cdr com))))
		    ((equal (car com) 'down) (setf (cdr pos) (+ (cdr pos) (cdr com))))))
    (* (car pos) (cdr pos))))



(defun calc-aim ()
  (let ((dir (with-open-file (stream "input")
	       (loop :for line = (read-line stream nil)
		     :while line
		     :collect (let ((lst (my-split line)))
				(cons (read-from-string (car lst))
				      (parse-integer (cadr lst)))))))
	(pos (list 0 0 0)))
    (loop :for com in dir
	  :do  (cond ((equal (car com) 'forward)
		     (progn (setf (car pos) (+ (car pos) (cdr com)))
			    (setf (cadr pos) (+ (cadr pos) (* (cdr com) (caddr pos))))))
		    ((equal (car com) 'up) (setf (caddr pos) (- (caddr pos) (cdr com))))
		    ((equal (car com) 'down) (setf (caddr pos) (+ (caddr pos) (cdr com))))))
    (* (car pos) (cadr pos))))
