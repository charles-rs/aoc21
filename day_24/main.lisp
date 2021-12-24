;; im just gonna push this anyways even though it's not how i solved the problem
;; in the end i just used pen an paper and the one function at the bottom
;; ig this is admitting defeat for lisp and the superiority of human computation? idk


(ql:quickload "cl-ppcre")

(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (mapcar #'read-from-string (cl-ppcre:split " " line)))))

	  (mapcar 'digit-char-p (coerce line 'list))


(defun replace-mapping (var nval lst)
  (acons var nval (remove-if (lambda (mp) (eql (car mp) var)) lst)))

(defun alist-get (key alist)
  (cdr (assoc key alist)))

(defun mk-mul (a b)
  (cond ((and (numberp a) (numberp b)) (* a b))
	((or (eql a 0) (eql b 0)) 0)
	((eql a 1) b)
	((eql b 1) a)
	(t (list 'mul a b))))

(defun mk-add (a b)
  (cond ((and (numberp a) (numberp b)) (+ a b))
	((eql a 0) b)
	((eql b 0) a)
	(t (list 'add a b))))

(defun mk-div (a b)
  (cond ((and (numberp a) (numberp b)) (values (floor (/ a b))))
	((eql a 0) 0)
	((eql b 1) a)
	(t (list 'div a b))))

(defun mk-mod (a b)
  (cond ((and (numberp a) (numberp b)) (mod a b))
	((eql a 0) 0)
	(t (list 'mod a b))))

(defun mk-eql (a b)
  (cond ((and (numberp a) (numberp b)) (if (= a b) 1 0))
	((and (numberp a) (or (< a 0) (> a 9)) (is-input b)) 0)
	((and (numberp b) (or (< b 0) (> b 9)) (is-input a)) 0)
	((equal a b) 1)
	(t (list 'eql a b))))

(defun get-same-op (expr op)
  (if (or (not (listp expr)) (not (eql (car expr) op)))
      (list expr)
      (revappend (get-same-op (second expr) op)
		 (get-same-op (third expr) op))))

(defun simp (expr)
  (cond ((numberp expr) expr)
	((eql (car expr) 'add)
	 (let* ((all-adds (get-same-op expr 'add))
		(num (reduce '+ (remove-if-not #'numberp all-adds))))
	   (reduce (lambda (a b) (mk-add a b)) (remove-if #'numberp all-adds) :initial-value num)))
	((eql (car expr) 'mul)
	 (let* ((all-muls (get-same-op expr 'mul))
		(num (reduce '* (remove-if-not #'numberp all-muls))))
	   (reduce (lambda (a b) (mk-mul a b)) (remove-if #'numberp all-muls) :initial-value num)))
	(t (list (car expr) (simp (second expr)) (simp (third expr))))))



(defun interpret-one (insn inpt mappings)
  "MAPPINGS is an assoc list. INSN is the instruction, and INPT is the input to be read
returns values of mappings and inpt after execing insn"
  (flet ((read-b (v)
	   (if (eql (type-of v) 'symbol)
	       (alist-get v mappings)
	       v)))
    (if (eql (car insn) 'inp)
	(values (replace-mapping (cadr insn) (car inpt) mappings) (cdr inpt))
	(let ((a (second insn))
	      (b (third insn))
	      (fn  (case (car insn)
		     ((add) '+)
		     ((mul) '*)
		     ((div) (lambda (a b) (floor (/ a b))))
		     ((mod) 'mod)
		     ((eql) (lambda (a b) (if (= a b) 1 0))))))
;	  (print mappings)
;	  (print insn)
	  (values
	   (replace-mapping a (apply fn (list (alist-get a mappings)
					      (read-b b))) mappings)
	   inpt)))))



(defun from-int (i)
  (mapcar 'digit-char-p (coerce (format nil "~d" i) 'list)))

(defun interpret (model-num insns)
  "MODEL-NUM should be a list of numbers"
  (labels ((interpret-helper (insns inpt mappings)
	     (if (null insns)
		 mappings
		 (multiple-value-bind (new-map new-in)
		     (interpret-one (car insns) inpt mappings)
		   (interpret-helper (cdr insns) new-in new-map)))))
    (alist-get 'z (interpret-helper insns model-num '((x . 0) (y . 0) (z . 0) (w . 0))))))




(defun interpret-one (insn inpt mappings)
  (flet ((read-b (v)
	   (if (eql (type-of v) 'symbol)
	       (alist-get v mappings)
	       v)))
    (if (eql (car insn) 'inp)
	(values (replace-mapping (cadr insn) (car inpt) mappings) (cdr inpt))
	(let ((a (second insn))
	      (b (third insn))
	      (fn  (case (car insn)
		     ((add) 'mk-add)
		     ((mul) 'mk-mul)
		     ((div) 'mk-div)
		     ((mod) 'mk-mod)
		     ((eql) 'mk-eql))))
					;	  (print mappings)
					;	  (print insn)
	  (values
	   (replace-mapping a (apply fn (list (alist-get a mappings)
					      (read-b b))) mappings)
	   inpt)))))

(defparameter abs-mod-num '(i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14))

(defun is-input (expr) (member expr abs-mod-num))

(defun interpret (insns)
  "MODEL-NUM should be a list of numbers"
  (labels ((interpret-helper (insns inpt mappings)
	     (if (null insns)
		 mappings
		 (multiple-value-bind (new-map new-in)
		     (interpret-one (car insns) inpt mappings)
		   (interpret-helper (cdr insns) new-in new-map)))))
    (print abs-mod-num)
  ;  (alist-get 'z
	       (interpret-helper insns abs-mod-num '((x . xinit) (y . yinit) (z . zinit) (w . winit)))));)



(defun parta ()
  (let ((insns (get-nums)))
    (loop :for i from 0 to 13
	  :collect (mapcar 'third
			   (list (nth (+ (* 18 i) 4) insns)
				 (nth (+ (* 18 i) 5) insns)
				 (nth (+ (* 18 i) 15) insns))))))
