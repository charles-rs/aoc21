(ql:quickload "cl-ppcre")

(defun get-nums ()
  (with-open-file (stream "input")
    (flet ((parse-point (str)
	     (mapcar 'parse-integer (cl-ppcre:split "\\.\\." (subseq str 2)))))
      (let* ((lst (loop :for line = (read-line stream nil)
			:while line
			:collect (destructuring-bind (command cuboid)
				     (cl-ppcre:split #\Space line)
				   (list (read-from-string command)
					 (mapcar #'parse-point (cl-ppcre:split #\, cuboid)))))))
	lst))))

(defun clamp (n min max)
  (min (max n min) max))

(defun parta ()
  (let ((commands (get-nums))
	(result (make-array '(103 103 103) :initial-element 'off)))
    (flet ((check-array (x y z)
	     (let ((fixed-x (+ x 51))
		   (fixed-y (+ y 51))
		   (fixed-z (+ z 51)))
	       (aref result fixed-x fixed-y fixed-z)))
	   (set-array (x y z val)
	     (let ((fixed-x (+ x 51))
		   (fixed-y (+ y 51))
		   (fixed-z (+ z 51)))
	       (setf (aref result fixed-x fixed-y fixed-z) val)))
	   (my-clamp (n)
	     (clamp n -51 51)))
      (loop :for cmd in commands
	    :do (destructuring-bind
		    (val ((xmin xmax) (ymin ymax) (zmin zmax)))
		    cmd
		  (loop :for x from (my-clamp xmin) to (my-clamp xmax)
			:do (loop :for y from (my-clamp ymin) to (my-clamp ymax)
				  :do (loop :for z from (my-clamp zmin) to (my-clamp zmax)
					    :do (set-array x y z val))))))
      (loop :for x from -50 to 50
	    :sum (progn
		   (loop :for y from -50 to 50
			 :sum (loop :for z from -50 to 50
				    :sum (if (eql 'on (check-array x y z))
					     1
					     0))))))))

(defun interval-isect (i1 i2)
  (if (and (>= (cadr i2) (car i1))
	   (<= (car i2) (cadr i1)))
      (list (max (car i1) (car i2)) (min (cadr i1) (cadr i2)))
      nil))

(defun cube-isect (cube1 cube2)
  (let ((isect (mapcar 'interval-isect cube1 cube2)))
    (if (some 'not isect)
	nil
	isect)))

(defun sub-interval (init to-sub)
  (let ((isect (interval-isect init to-sub)))
    (if isect
	(list isect
	      (list (car init) (1- (car isect)))
	      (list (1+ (cadr isect)) (cadr init)))
	init)))

(defun cart-prod (l1 l2 l3)
  (loop :for i in l1 :append (loop :for j in l2 :append (loop :for k in l3 :collect (list i j k)))))

(defun sub-cube (init-cube to-sub)
  (let ((isect (cube-isect init-cube to-sub))
	(int-subs (mapcar 'sub-interval init-cube to-sub)))
    (if isect
	(remove-if (lambda (cube) (some (lambda (ivl) (< (cadr ivl) (car ivl))) cube))
		   (cdr (apply 'cart-prod int-subs)))
	init-cube)))

(defun cube-volume (cube)
  (reduce (lambda (acc rest) (* acc (1+ (- (cadr rest) (car rest))))) cube :initial-value 1))

(defun partb ()
  (let ((cmds (get-nums)))
    (labels ((calc-new-lst (cube lst acc)
	       (if (null lst) acc
		   (let ((sm (car lst)))
		     (if (cube-isect cube sm)
			 (calc-new-lst cube (cdr lst) (revappend (sub-cube sm cube) acc))
			 (calc-new-lst cube (cdr lst) (cons sm acc))))))
	     (evaluate (cmds pos-cubes)
	       (if (null cmds)
		   pos-cubes
		   (let* ((cmd (car cmds)) (dir (car cmd)) (curr (cadr cmd)))
		     (if (eql dir 'on)
			 (evaluate (cdr cmds) (cons curr (calc-new-lst curr pos-cubes nil)))
			 (evaluate (cdr cmds) (calc-new-lst curr pos-cubes nil)))))))
      (reduce (lambda (acc cube)
		(+ acc (cube-volume cube)))
	      (evaluate cmds nil) :initial-value 0))))
