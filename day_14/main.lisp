(ql:quickload "cl-ppcre")

(defun get-nums ()
  (with-open-file (stream "input")
    (let* ((template (coerce (read-line stream nil) 'list))
	   (lst (progn (read-line stream nil)
		       (loop :for line = (read-line stream nil)
			     :while (> (length line) 0)
			     :collect (cl-ppcre:split " -> " line))))
	   (tbl (make-hash-table :test 'equal)))
      (loop :for pair in lst
	    :do (setf (gethash (coerce (car pair) 'list) tbl) (char (cadr pair) 0)))
      (cons template tbl))))

(defun step-once (seq tbl)
  (labels ((step-helper (seq acc)
	     (if seq
		 (let* ((pair (list (car seq) (cadr seq)))
			(lookup (gethash pair tbl)))
		   (if lookup
		       (step-helper (cdr seq) (cons lookup (cons (car seq) acc)))
		       (step-helper (cdr seq) (cons (car seq) acc))))
		 (reverse acc))))
    (step-helper seq nil)))

(defun parta ()
  (let* ((input (get-nums))
	 (seq (car input))
	 (cnt (make-hash-table)))
    (loop :for i from 1 to 10
	  :do (setq seq (step-once seq (cdr input))))
    (loop :for c in seq
	  :do (if (gethash c cnt)
		  (setf (gethash c cnt) (1+ (gethash c cnt)))
		  (setf (gethash c cnt) 1)))
    (let ((lst (sort (loop :for c being the hash-keys of cnt
			   :collect (cons c (gethash c cnt)))
		     (lambda (a b) (> (cdr a) (cdr b))))))
      (- (cdr (first lst)) (cdar (last lst))))))

(defun digraphs (lst)
  (labels ((helper (lst acc)
	     (if (second lst)
		 (helper (cdr lst) (cons (list (car lst) (cadr lst)) acc))
		 acc)))
    (helper lst nil)))

(defun partb (n)
  (let* ((input (get-nums))
	 (trans (cdr input))
	 (seq (car input))
	 (cnt (make-hash-table :test 'equal))
	 (old-cnt nil)
	 (res-cnt (make-hash-table))
	 (first-pair nil))
    (labels ((inc-hash (dg )
	       (if (gethash dg cnt)
		   (setf (gethash dg cnt) (1+ (gethash dg cnt)))
		   (setf (gethash dg cnt) 1)))
	     (add-hash (key amt table)
	       (if (gethash key table)
		   (setf (gethash key table) (+ amt (gethash key table)))
		   (setf (gethash key table) amt))))
      (loop :for dg in (digraphs seq)
	    :do (inc-hash dg))
      (setq first-pair (car (last (digraphs seq))))
      (loop :for i from 1 to n
	    :do (progn
		  (setq old-cnt cnt)
		  (setq cnt (make-hash-table :test 'equal))
		  (loop :for dg being the hash-keys of old-cnt
			:do (let ((nw (gethash dg trans)))
			      (if nw
				  (let ((amt (gethash dg old-cnt)))
				    (if (equal dg first-pair)
					(setq first-pair (list (first dg) nw)))
				    (add-hash (list (first dg) nw) amt cnt)
				    (add-hash (list nw (second dg)) amt cnt)))))))
      (loop :for dg being the hash-keys of cnt
	    :do (let ((val (gethash dg cnt)))
		  (if (equal dg first-pair)
		      (add-hash (car dg) val res-cnt))
		  (add-hash (cadr dg) val res-cnt)))

      (let ((lst (sort (loop :for c being the hash-keys of res-cnt
			     :collect (cons c (gethash c res-cnt)))
		       (lambda (a b) (> (cdr a) (cdr b))))))
	(print lst)
	(- (cdr (first lst)) (cdar (last lst)))))))
