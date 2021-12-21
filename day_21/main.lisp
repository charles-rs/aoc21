;;; example: '(4 8)
;;; mine: '(3 4)

(defclass die ()
    ((value :accessor value :initform 0)
     (roll :reader roll)))

(defmethod roll ((object die))
  (setf (value object) (1+ (value object)))
  (value object))

(defun new-die ()
  (make-instance 'die))

(defclass player ()
  ((score :accessor score :initform 0)
   (pos :accessor pos :initarg :pos)))

(defgeneric add-score (player))

(defmethod add-score ((object player))
  (setf (score object) (+ (1+ (pos object)) (score object))))

(defun new-player (pos)
  (make-instance 'player :pos (1- pos)))

(defun turn (player die)
  (let ((advance (reduce #'+ (list (roll die) (roll die) (roll die)))))
    (setf (pos player) (mod (+ (pos player) advance) 10))
    (add-score player)))

(defun parta (init-pos)
  (destructuring-bind (p1pos p2pos) init-pos
    (let ((die (new-die))
	  (p1 (new-player p1pos))
	  (p2 (new-player p2pos)))
      (labels ((other-player (p)
		 (if (eql p p1)
		     p2
		     p1))
	       (run-game (p)
		 (turn p die)
		 (if (>= (score p) 1000)
		     p
		     (run-game (other-player p)))))
	(let ((winner (run-game p1)))
	  (* (value die) (score (other-player winner))))))))

(defun partb (init-pos)
  (let ((memoize (make-hash-table :test 'equal))
	(outcomes '(3 4 4 4 5 5 5 5 5 5 6 6 6 6 6 6 6 7 7 7 7 7 7 8 8 8 9)))
    (labels ((other-pl (which)
	       (if (= 0 which) 1 0))
	     (reorder (players which)
	       (destructuring-bind (playing sitting)
		   players
		 (if (= 0 which)
		     players
		     (list sitting playing))))
	     (count-unis (p1 p2 which)
	       (if (gethash (list p1 p2 which) memoize)
		   (gethash (list p1 p2 which) memoize)
		   (let ((res (count-unis-exp p1 p2 which)))
		     (setf (gethash (list p1 p2 which) memoize) res)
		     res)))
	     (count-unis-exp (p1 p2 which)
	       (cond ((>= (car p1) 21) (list 1 0))
		     ((>= (car p2) 21) (list 0 1))
		     (T
		      (let* ((players (if (= 0 which)
					  (list p1 p2)
					  (list p2 p1))))
			(destructuring-bind (playing sitting)
			    players
			  (reduce (lambda (acc die)
				    (let* ((dest (mod (+ (cdr playing) die) 10))
					   (played (cons (+ (car playing) dest 1) dest))
					   (reordered (reorder (list played sitting) which))
					   (rec-count (count-unis (car reordered)
								  (cadr reordered)
								  (other-pl which))))
				      (list (+ (car acc) (car rec-count))
					    (+ (cadr acc) (cadr rec-count)))))
				  outcomes :initial-value (list 0 0))))))))
      (destructuring-bind (p1pos p2pos) init-pos
	(reduce #'max (count-unis (cons 0 (1- p1pos)) (cons 0 (1- p2pos)) 0))))))
