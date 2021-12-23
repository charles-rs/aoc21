(ql:quickload "cl-ppcre")
(ql:quickload "alexandria")
(ql:quickload "priority-queue")
(use-package :priority-queue)

(defun get-nums (&optional partb)
  (let ((fname (if partb "inputb" "inputa")))
    (with-open-file (stream fname)
      (labels ((get-rooms (acc)
		 (let ((ln (to-list (read-line stream nil))))
		   (if ln
		       (get-rooms (cons ln acc))
		       (reverse acc))))
	       (to-list (str)
		 (mapcar 'read-from-string
			 (cl-ppcre:split "#" (string-trim '(#\Space #\#) str)))))
	(read-line stream nil)
	(read-line stream nil)
	(let ((rooms (get-rooms nil)))
	  (make-gamestate :rooms
			  (make-array (list 4 (length rooms))
				      :initial-contents (apply #'mapcar #'list rooms))
			  :toprow (make-array 11 :initial-element nil)))))))

(defstruct gamestate
  (toprow)
  (rooms))

(defmethod print-object ((gs gamestate) stream)
  (format stream "~%#############~%#")
  (format stream "~{~:[.~:;~:*~A~]~}#~%###" (coerce (gamestate-toprow gs) 'list))
  (let* ((rooms (gamestate-rooms gs))
	 (dims (array-dimensions rooms)))
    (loop :for i from 0 to 3
    	  :do (format stream "~A#" (if (aref rooms i 0) (aref rooms i 0) ".")))
    (format stream "##~%")
    (loop :for row from 1 below (second dims)
	  :do (progn
		(format stream "  #")
		(loop :for col from 0 to 3
		      :do (format stream "~A#" (if (aref rooms col row) (aref rooms col row) ".")))
		(format stream "~%")))
    (format stream "  #########~%")))

(defun assert-valid-state (gs)
  "checks that a games state is valid, that is, it has the correct number of each letter"
  (let* ((depth (second (array-dimensions (gamestate-rooms gs))))
	 (letters
	   (revappend
	    (loop :for i across (gamestate-toprow gs)
		  :collect i)
	    (loop :for i from 0 to 3
		  :append (loop :for j from 0 below depth
				:collect (aref (gamestate-rooms gs) i j))))))
    (mapc (lambda (l) (assert (= depth (count l letters)))) '(a b c d))))

(defun above (idx)
  "converts room into index in hall array"
  (* (1+ idx) 2))

(defun can-stop-at (idx gs from &optional room)
  (and (or room (/= idx 2 4 6 8))
       (every #'null (loop :for i from (min idx from) to (max idx from)
			   :if (or (null room)
				   (/= i from))
			     :collect (aref (gamestate-toprow gs) i)))))

(defun my-room (tp)
  (case tp
    (a 0)
    (b 1)
    (c 2)
    (d 3)))

(defun cost (tp)
  (case tp
    (a 1)
    (b 10)
    (c 100)
    (d 1000)))

(defun nil-or-eq (a b)
  "a should be the constant"
  (if b (eql a b)
      t))

(defun depth (gs room)
  (let* ((rooms (gamestate-rooms gs))
	 (dims (array-dimensions rooms))
	 (tmp (loop :for i from 0 below (cadr dims)
		    :when (aref rooms room i)
		      :return (list (aref rooms room i) i))))
    (if tmp (values-list tmp)
	(values nil (cadr dims)))))

(defun out-moves (gs room)
  (multiple-value-bind (tp how-deep) (depth gs room)
    (let* ((rooms (gamestate-rooms gs))
	   (hall-idx (above room)))
      (flet ((state-of-pair (pr)
	       (destructuring-bind (dest cost) pr
		 (let ((new-rooms (alexandria:copy-array rooms))
		       (hall (alexandria:copy-array (gamestate-toprow gs))))
		   (setf (aref new-rooms room how-deep) nil)
		   (setf (aref hall dest) tp)
		   (cons (make-gamestate :rooms new-rooms :toprow hall) (* (cost tp) cost))))))
	(mapcar
	 #'state-of-pair
	 (revappend (loop :for i from (1- hall-idx) downto 0
			  :if (can-stop-at i gs hall-idx)
			    :collect (list i (+ 1 how-deep (- hall-idx i))))
		    (loop :for i from (1+ hall-idx) below 11
			  :if (can-stop-at i gs hall-idx)
			    :collect (list i (+ 1 how-deep (- i hall-idx))))))))))

(defun room-list (gs room)
  (loop :for i from 0 below (cadr (array-dimensions (gamestate-rooms gs)))
	:collect (aref (gamestate-rooms gs) room i)))

(defun in-moves (gs)
  (flet ((state-of-trio (trio)
	   (destructuring-bind (dest orig cost) trio
	     (let ((new-rooms (alexandria:copy-array (gamestate-rooms gs)))
		   (hall (alexandria:copy-array (gamestate-toprow gs))))
	       (multiple-value-bind (_ how-deep) (depth gs (my-room dest))
		 (declare (ignore _))
		 (setf (aref hall orig) nil)
		 (setf (aref new-rooms (my-room dest) (1- how-deep)) dest)
		 (cons (make-gamestate :rooms new-rooms :toprow hall)
		       (* cost (cost dest))))))))
    (let ((hall (gamestate-toprow gs)))
      (mapcar #'state-of-trio
	      (loop :for i from 0 below 11
		    :for tp = (aref hall i)
		    :if (if tp
			    (and (every (lambda (v) (nil-or-eq tp v))
					(room-list gs (my-room tp)))
				 (can-stop-at (above (my-room tp)) gs i t))
			    nil)
		      :collect (list tp i
				     (multiple-value-bind (_ depth) (depth gs (my-room tp))
				       (declare (ignore _))
				       (+ depth (abs (- i (above (my-room tp))))))))))))


(defun all-valid-moves (gs)
  (let* ((moves (revappend
		 (in-moves gs)
		 (loop :for i from 0 to 3
		       :if (some #'identity (room-list gs i))
			 :append (out-moves gs i)))))
    (mapc #'assert-valid-state (mapcar #'car moves))
    moves))

(defun shortest-path (start end)
  (let ((dists (make-hash-table :test 'equalp))
	(back-map (make-hash-table :test 'equalp))
	(frontier (make-pqueue '<)))
    (pqueue-push start 0 frontier)
    (setf (gethash start dists) 0)
    (let ((ret
	    (loop :named outer
		  :while (not (pqueue-empty-p frontier))
		  :do (multiple-value-bind (nd dist) (pqueue-pop frontier)
			(when (equalp nd end)
			  (return-from outer dist))
			(loop :for n in (all-valid-moves nd)
			      :do (let ((ndist (+ dist (cdr n))))
				    (if (gethash (car n) dists)
					(if (< (+ dist (cdr n)) (gethash (car n) dists))
					    (progn
					      (setf (gethash (car n) back-map) nd)
					      (pqueue-push (car n) ndist frontier)
					      (setf (gethash (car n) dists) ndist)))
					(progn
					  (setf (gethash (car n) back-map) nd)
					  (pqueue-push (car n) ndist frontier)
					  (setf (gethash (car n) dists) ndist))))))))
	  (iter end))
      (when ret
      (mapc (lambda (s)
	      (princ s))(reverse (loop :while (not (equalp iter start))
					:collect (progn (setq iter (gethash iter back-map))
							iter)))))
      (princ end)
      ret)))

(defun parta ()
  (let ((start (get-nums))
	(final (make-gamestate :rooms (make-array '(4 2) :initial-contents '((a a) (b b) (c c) (d d)))
			       :toprow (make-array 11 :initial-element nil))))
    (shortest-path start final)))

(defun partb ()
  (let ((start (get-nums T))
	(final (make-gamestate
		:rooms (make-array '(4 4) :initial-contents '((a a a a) (b b b b) (c c c c) (d d d d)))
		:toprow (make-array 11 :initial-element nil))))
    (shortest-path start final)))
