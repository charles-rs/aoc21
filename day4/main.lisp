(defun delimiterp (c) (char= c #\,))


(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))




(defun get-nums ()
  (with-open-file (stream "input")
    (let ((lines (loop :for line = (let ((ln (read-line stream nil)))
				     (if ln (setq width (length ln)))
				     ln)
		       :while line
		       :collect line)))
      (setq input (mapcar 'parse-integer (my-split (car lines))))
      (setq board-line 0)
      (setq board (make-array '(5 5) :initial-element (cons 0 nil)))
      (setq boards (loop :for i in (cdr lines)
			 :if (= board-line 5)
			   :collect board
			   :and :do
			     (progn (setq board (make-array '(5 5) :initial-element (cons 0 nil)))
				    (setq board-line 0))
			 :else
			   :do
			      (if (/= (length i) 0)
				  (progn
				    (loop :for j from 0 to 4
					  :do (setf (aref board board-line j)
						    (cons (parse-integer (subseq i (* j 3) (+ (* j 3) 2)))
							  nil)))
				    (setq board-line (1+ board-line)))))))))




(defun check-win (board)
  (setq won nil)
  (loop :for i from 0 to 4
	:do
	   (progn (setq row T)
		  (setq col T)
		  (loop :for j from 0 to 4
			:do (progn (setq row (and row (cdr (aref board i j))))
				   (setq col (and col (cdr (aref board j i))))))
		  (setq won (or won (or row col)))))
  won)


(defun apply-guess (num board)
  (loop for i from 0 to 4
	do (loop for j from 0 to 4
		 do (if (= (car (aref board i j)) num)
			(setf (cdr (aref board i j)) T)))))

(defun parta ()
  (get-nums)
  (let* ((winning-board
	  (car (loop named outer for guess in input
		     :do (if (remove-if-not 'check-win boards)
			     (progn
				    (return-from outer (remove-if-not 'check-win boards)))
			     (progn (setq winner guess)
				    (mapcar (lambda (bd) (apply-guess guess bd)) boards))))))
	 (sum (loop for i from 0 to 4
		    sum (loop for j from 0 to 4
			      sum (if (cdr (aref winning-board i j))
				      0
				      (car (aref winning-board i j)))))))
    (print winning-board)
    (print winner)
    (* sum winner)))


(defun partb ()
  (get-nums)
  (let* ((winning-board
	  (car (loop named outer for guess in input
		     :do (if (= 1 (length (remove-if-not (lambda (bd) (not (check-win bd))) boards)))
			     (progn (setq winner guess)
				    (return-from outer
				      (remove-if-not (lambda (bd) (not (check-win bd))) boards)))
			     (progn
			       (mapcar (lambda (bd) (apply-guess guess bd)) boards))))))
	 (dontcare (apply-guess winner winning-board))
	 (sum (loop for i from 0 to 4
		    sum (loop for j from 0 to 4
			      sum (if (cdr (aref winning-board i j))
				      0
				      (car (aref winning-board i j)))))))
    (* sum winner)))

