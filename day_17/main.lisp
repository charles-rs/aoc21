(defvar xmax)
(defvar xmin)
(defvar ymax)
(defvar ymin)
(multiple-value-setq (xmin xmax ymin ymax) (values 257 286 -101 -57))
;example
(multiple-value-setq (xmin xmax ymin ymax) (values 20 30 -10 -5))

(defun check-hit (x y)
  (and (>= x xmin)
       (<= x xmax)
       (>= y ymin)
       (<= y ymax)))

(defun sign (n)
  (if (= n 0) 0 (/ n (abs n))))

(defun step-once (x y xv yv)
  (values (+ x xv) (+ y yv) (- xv (sign xv)) (- yv 1)))

(defun simulate (xv yv)
  (let ((max-height 0) (x 0) (y 0) (hit nil))
    (loop :while (and (<= x xmax) (>= y ymin))
	  :do (progn
		(multiple-value-setq (x y xv yv) (step-once x y xv yv))
		(if (check-hit x y)
		    (setq hit t))
		(setq max-height (max max-height y))))
    (and hit max-height)))

(defun parta ()
  (let ((maxh 0))
    (loop :for x from 0 to 300
	  :do (loop :for y from 0 to 1000
		    :do (let ((sim (simulate x y)))
			  (if sim (setq maxh (max maxh sim))))))
    maxh))

(defun partb ()
  (let ((cnt 0))
    (loop :for x from 0 to 300
	  :do (loop :for y from -300 to 1000
		    :do (if (simulate x y)
			    (setq cnt (1+ cnt)))))
    cnt))
