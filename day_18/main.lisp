(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (remove-if
		    (lambda (c) (if (numberp c)
				    nil
				    (char= c #\,)))
		    (mapcar (lambda (x)
			      (if (digit-char-p x)
				  (digit-char-p x)
				  x)) (coerce line 'list))))))

(defun explode (snail)
  (let ((stack nil) (done nil)
	(depth 0) (lst (copy-list snail)))
    (loop :while (and lst (not done))
	  :do (let ((head (pop lst)))
		(cond ((equal head #\[)
		       (progn (setq depth (1+ depth))
			      (push head stack)))
		      ((equal head #\])
		       (progn (if (> depth 4)
				  (progn (setq done T)
					 (let* ((pair-right (pop stack))
						(pair-left (pop stack))
						(left (find-if 'numberp stack))
						(right (find-if 'numberp lst)))
					   (pop stack)
					   (if left
					       (nsubstitute-if (+ left pair-left) 'numberp stack :count 1))
					   (if right
					       (nsubstitute-if (+ right pair-right) 'numberp lst :count 1))
					   (push 0 stack)))
				  (push head stack))
			      (setq depth (1- depth))))
		      (T (push head stack)))))
    (values (append (reverse stack) lst) (not (not lst)))))

(defun over-10 (n)
  (if (numberp n) (>= n 10)
      nil))

(defun split (snail)
  (labels ((split-back (lst acc)
	     (if lst
		 (let ((head (car lst)))
		   (if (over-10 head)
		       (let ((pair (list #\[ (values (floor (/ head 2)))
					 (values (ceiling (/ head 2))) #\])))
			 (values (append (reverse acc) pair (cdr lst)) T))
		       (split-back (cdr lst) (cons head acc))))
		 (values (reverse acc) nil))))
    (split-back snail nil)))

(defun snail-reduce (snail)
  (multiple-value-bind (exp did-exp) (explode snail)
    (if (not did-exp)
	(multiple-value-bind (spl did-spl) (split snail)
	  (if did-spl (snail-reduce spl)
	      snail))
	(snail-reduce exp))))

(defun snail-add (snail-a snail-b)
  (snail-reduce (append (cons #\[ snail-a) snail-b (list #\]))))

(defun read-snail (snail)
  (labels ((every-other-space (lst acc)
	     (if lst
		 (every-other-space (cdr lst) (cons (car lst) (cons #\Space acc)))
		 (reverse acc))))
    (let* ((str (coerce (every-other-space
			 (mapcar (lambda (c) (cond ((numberp c)
						    (code-char (+ c (char-code #\0))))
						   ((char= c #\[) #\()
						   ((char= c #\]) #\))
						   (T c))) snail) nil) 'string))
	   (s (make-string-input-stream str)))
      (read s))))

(defun snail-magnitude (snail)
  (if (numberp snail) snail
      (+ (* 3 (snail-magnitude (first snail)))
	 (* 2 (snail-magnitude (second snail))))))

(defun parta ()
  (snail-magnitude (read-snail (reduce #'snail-add (get-nums)))))

(defun partb ()
  (let ((nums (get-nums))
	(max 0))
    (loop :for a in nums
	  :do (loop :for b in nums
		    :do (setq max (max max
				       (snail-magnitude (read-snail (snail-add a b)))
				       (snail-magnitude (read-snail (snail-add b a)))))))
    max))
