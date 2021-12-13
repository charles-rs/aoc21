(ql:quickload "cl-ppcre")

(defun get-nums ()
  (with-open-file (stream "input")
    (let* ((lst (loop :for line = (read-line stream nil)
		      :while (> (length line) 0)
		      :collect (mapcar 'parse-integer (cl-ppcre:split "," line))))
	   (tbl (make-hash-table :test 'equal))
	   (folds (loop :for line = (read-line stream nil)
			:while line
			:collect (mapcar (lambda (str) (with-input-from-string (in str) (read in)))
					 (cl-ppcre:split "=" (third (cl-ppcre:split " " line)))))))
      (loop :for pair in lst :do (setf (gethash pair tbl) T))
      (cons tbl folds))))

(defun fold (pt dir num)
  (if (equal dir 'X)
      (if (< (first pt) num) pt
	  (cons (+ (* 2 num) (- (car pt))) (cdr pt)))
      (if (< (second pt) num) pt
	  (list (first pt) (+ (* 2 num) (- (second pt)))))))


(defun process-fold (tbl dir num)
  (let ((res (make-hash-table :test 'equal)))
    (loop :for pair being the hash-keys of tbl
	  :do (setf (gethash (fold pair dir num) res) T))
    res))

(defun parta ()
  (let* ((input (get-nums))
	 (folded (process-fold (car input) (caadr input) (cadadr input))))
    (hash-table-count folded)))

(defun simulate (input)
  (let ((tbl (car input))
	(folds (cdr input)))
    (loop :for f in folds
	  :do (setq tbl (process-fold tbl (car f) (cadr f))))
    tbl))

(defun partb ()
  (let* ((input (get-nums)) (folded (simulate input)))
    (loop :for i from 0 to 5
	  :do (progn (loop :for j from 0 to 40
			   :do (if (gethash (list j i) folded)
				   (princ #\#)
				   (princ #\Space)))
		     (terpri)))))
