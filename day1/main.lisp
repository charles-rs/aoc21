(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (parse-integer line))))



(defun calc-gt (nums)
  (reduce '+ (maplist (lambda (lst)
			(if (cdr lst)
			    (if (< (car lst) (cadr lst)) 1 0)
			    0))
		      nums)))


(defun calc-gt-window (nums)
  (reduce '+ (maplist (lambda (lst)
			(if (cadddr lst)
			    (if (< (+ (car lst) (cadr lst) (caddr lst))
				   (+ (cadr lst) (caddr lst) (cadddr lst))) 1 0)
			    0))
		      nums)))
