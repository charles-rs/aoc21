(ql:quickload "cl-ppcre")
(ql:quickload "cl-utilities")
(ql:quickload "lla")
(ql:quickload "alexandria")
(ql:quickload "array-operations")
(use-package :lla)
(use-package :alexandria)


(defun get-nums ()
  (with-open-file (stream "input")
    (let* ((lines (cl-utilities:split-sequence-if (lambda (x) (equal x ""))
						  (loop :for line = (read-line stream nil)
							:while line
							:collect line))))
      (loop :for scanner in lines
	    :collect (mapcar (lambda (ln)
			       (concatenate 'vector
					    (coerce (mapcar #'parse-integer
							    (cl-ppcre:split "," ln)) 'vector)
					    #(1))) (cdr scanner))))))




(defun enum-prods (mat num)
  (labels ((enum-prods-back (mat acc num)
	     (if (= 0 num) acc
		 (enum-prods-back mat (if acc
					  (cons (mm mat (car acc)) acc)
					  (list mat)) (1- num)))))
    (enum-prods-back mat nil num)))

(defun all-rotates ()
  (multiple-value-bind (rotate-x rotate-y rotate-z)
      (values #2A((1 0 0 0)
		  (0 0 -1 0)
		  (0 1 0 0)
		  (0 0 0 1))
	      #2A((0 0 1 0)
		  (0 1 0 0)
		  (-1 0 0 0)
		  (0 0 0 1))
	      #2A((0 -1 0 0)
		  (1 0 0 0)
		  (0 0 1 0)
		  (0 0 0 1)))
    (let* ((all-y (enum-prods rotate-y 4))
	   (all-x (enum-prods rotate-x 4))
	   (all-z (list rotate-z (mm rotate-z (mm rotate-z rotate-z))))
	   (x-and-z (append all-x all-z))
	   (rots (alexandria:flatten (loop :for m in x-and-z
					   :collect (mapcar (lambda (x) (mm m x)) all-y)))))
      rots)))

(defvar rotates (all-rotates))

(defun check-transform (init unknown mat)
  (let* ((transformed (mapcar (lambda (x) (mm mat x)) unknown))
	 (overlap (intersection init transformed :test 'equalp)))
    (>= (length overlap) 12)))

(defun add (v1 _v2)
  (let ((v2 (copy-array _v2)))
    (axpy! 1 v1 v2)))

(defun sub (_v1 v2)
  (let ((v1 (copy-array _v1)))
    (lla:axpy! -1 v2 v1)))

(defun to-mat-trans (diff)
  (let ((mat (make-array '(4 4)
			 :initial-contents '((1 0 0 0)
					     (0 1 0 0)
					     (0 0 1 0)
					     (0 0 0 1)))))
    (loop :for i from 0 to 2
	  :do (setf (aref mat i 3) (aref diff i)))
    mat))

(defun find-transform (first unknown)
  (loop :named outer
	:for r in rotates
	:do (loop :for a in first
		  :do (loop :for b in unknown
			    :do (let* ((diff (sub a (mm r b)))
				       (trans (mm (to-mat-trans diff) r)))
				  (if (check-transform first unknown trans)
				      (return-from outer trans)))))))
(defun parta ()
  (let* ((inputs (get-nums))
	 (final-list nil)
	 (count-map (make-hash-table :test 'equalp))
	 (to-process (list (car inputs)))
	 (remaining (cdr inputs)))
    (loop :while remaining
	  :do (let* ((lst (pop to-process))
		     (transforms (loop :for other in remaining
				       :collect (find-transform lst other))))
		(print lst)
		(setq remaining
		      (loop :for trans in transforms
			    :for to-trans in remaining
			    :if trans
			      :do (push (mapcar (lambda (vec) (mm trans vec)) to-trans) to-process)
			    :else
			      :collect to-trans))
		(push lst final-list)))
    (print "-------------------------")
    (let ((all-points (flatten (append final-list to-process))))
      (loop :for pt in all-points
	    :do (setf (gethash pt count-map) T))
      (hash-table-count count-map))))

(defun l1-dist (pt1 pt2)
  (loop :for x across (sub pt1 pt2)
	:sum (abs x)))
;(reduce '+ (mapcar #'abs (coerce (copy-array (sub pt1 pt2)) 'list))))

(defun max-l1 (pts)
  (let ((max 0))
    (loop :for a in pts
	  :do (loop :for b in pts
		    :do (setq max (max max (l1-dist a b)))))
    max))


(defun trans-mat-to-pt (mat)
  (let ((pt (make-array 3 :initial-element 0)))
    (loop :for i from 0 to 2
	  :do (setf (aref pt i) (aref mat i 3)))
    pt))

(defun partb ()
  (let* ((inputs (get-nums))
	 (final-list nil)
	 (to-process (list (car inputs)))
	 (remaining (cdr inputs))
	 (transes nil))
    (loop :while remaining
	  :do (let* ((lst (pop to-process))
		     (transforms (loop :for other in remaining
				       :collect (find-transform lst other))))
		(print lst)
		(setq remaining
		      (loop :for trans in transforms
			    :for to-trans in remaining
			    :if trans
			      :do (progn
				    (push trans transes)
				    (push (mapcar (lambda (vec) (mm trans vec)) to-trans) to-process))
			    :else
			      :collect to-trans))
		(push lst final-list)))
    (print "-------------------------")
    (max-l1 (cons #(0 0 0) (mapcar 'trans-mat-to-pt transes)))))
