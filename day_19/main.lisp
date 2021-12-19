(ql:quickload "cl-ppcre")
(ql:quickload "cl-utilities")
(ql:quickload "lla")
(use-package :cl-utilities)
(use-package :cl-ppcre)
(use-package :lla)


(defun get-nums ()
  (with-open-file (stream "input")
    (let* ((lines (split-sequence-if (lambda (x) (equal x ""))
				     (loop :for line = (read-line stream nil)
					   :while line
					   :collect line))))
      (loop :for scanner in lines
	    :collect (mapcar (lambda (ln)
			       (mapcar #'parse-integer (split "," ln))) (cdr scanner))))))




(defun all-rotate (lst)
  (let ((pt (make-array 3 :initial-contents lst)))
  (multiple-value-bind (rotate-x rotate-y rotate-z)
      (values #2A((1 0 0)
		  (0 0 -1)
		  (0 1 0))
	      #2A((0 0 1)
		  (0 1 0)
		  (-1 0 0))
	      #2A((0 -1 0)
		  (1 0 0)
		  (0 0 1)))
      (labels ((rot-x (v) (lla:mm rotate-x v))
	       (rot-y (v) (lla:mm rotate-y v))
	       (rot-z (v) (lla:mm rotate-z v)))
	(list pt (rot-x pt) (rot-y pt) (rot-z pt))))))

(defvar rotate-left)
(setq rotate-left
      #2A((1 0 0)
	  (0 0 -1)
	  (0 1 0)))


(defun all-permute (pt)
  (labels ((sign-permutes (lst)
	     (if lst
		 (let ((tail (sign-permutes (cdr lst))))
		   (if tail
		       (append (mapcar (lambda (x) (cons (car lst) x)) tail)
			       (mapcar (lambda (x) (cons (- (car lst)) x)) tail))
		       (list (list (car lst)) (list (- (car lst))))))
		 nil))
	   (all-permutations (list)
	     (cond ((null list) nil)
		   ((null (cdr list)) (list list))
		   ((null (cddr list)) (list list))
		   (t (loop for element in list
			    append (mapcar (lambda (l) (cons element l))
					   (all-permutations (remove element list))))))))
    (cond ((null pt) nil)
	  ((null (cdr pt) (list pt)))
	  ((
    (loop :for p in (sign-permutes pt)
	  :append (all-permutations p))))
