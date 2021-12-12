(ql:quickload "cl-ppcre")
(ql:quickload "alexandria")

(defun get-nums ()
  (with-open-file (stream "input")
    (let ((lst (loop :for line = (read-line stream nil)
		     :while line
		     :collect (cl-ppcre:split "-" line)))
	  (tbl (make-hash-table :test 'equal)))
      (loop :for pair in lst
	    :do (progn (setf (gethash (first pair) tbl)
			     (cons (second pair) (gethash (first pair) tbl)))
		       (setf (gethash (second pair) tbl)
			     (cons (first pair) (gethash (second pair) tbl)))))
      tbl)))

(defun find-paths (graph start visited)
  (if (string= start "end")
      (cons "end" nil)
      (let* ((neighbors (remove-if (lambda (elem) (and (string= elem (string-downcase elem))
						      (member elem visited :test 'equal)))
				  (gethash start graph)))


	     (tails (loop :for n in neighbors
			  :append
			  (find-paths graph n (cons start visited)))))


	(mapcar (lambda (x) (cons start x)) tails))))

(defun parta ()
  (let ((graph (get-nums)))
    (length (find-paths graph "start" nil))))

(defun find-paths-b (graph start visited used-once)
  (if (string= start "end")
      (cons "end" nil)
      (let* ((neighbors (remove-if (lambda (elem) (or
						   (and (string= elem (string-downcase elem))
							(member elem visited :test 'equal))
						   (string= elem "start")))
				  (gethash start graph)))
	     (tails (loop :for n in neighbors
			  :append
			  (if used-once (find-paths-b graph n (cons start visited) T)
			      (append (find-paths-b graph n visited T)
				      (find-paths-b graph n (cons start visited) NIL))))))


	(mapcar (lambda (x) (cons start x)) tails))))

(defun partb ()
  (let ((graph (get-nums)))
    (length (remove-duplicates (find-paths-b graph "start" nil nil) :test 'equal))))
