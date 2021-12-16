(defun get-nums ()
  (with-open-file (stream "input")
    (let* ((hex (read-line stream nil))
	   (bin (mapcar (lambda (c) (- (char-int c) 48))
			(coerce (format nil "~b" (parse-integer hex :radix 16)) 'list))))
      (loop :for i from (length bin) below (* 4 (length hex))
	    :do (setq bin (cons 0 bin)))
      bin)))

(defstruct literal
  (version -1 :type integer)
  (value -1 :type integer))
(defun new-literal (version value)
  (make-literal :version version :value value))

(defstruct operator
  (version -1 :type integer)
  (subpackets nil :type cons)
  (op -1 :type integer))
(defun new-operator (version subpackets op)
  (make-operator :version version :subpackets subpackets :op op))

(defun to-int (lst)
  (labels
      ((help (acc l)
	 (if l (help (+ (ash acc 1) (car l)) (cdr l)) acc)))
    (help 0 lst)))

(defun parse-literal (instream)
  (labels
      ((helper (is acc consumed)
	 (let ((first-five (subseq is 0 5))
	       (rest (nthcdr 5 is)))
	   (if (= 0 (car first-five))
	       (values (+ (ash acc 4) (to-int (cdr first-five))) rest (+ 5 consumed))
	       (helper rest (+ (ash acc 4) (to-int (cdr first-five))) (+ 5 consumed))))))
    (helper instream 0 0)))

(defun parse-packet (instream)
  (let* ((version (to-int (subseq instream 0 3)))
	 (after-version (nthcdr 3 instream))
	 (type-id (to-int (subseq after-version 0 3)))
	 (after-type-id (nthcdr 3 after-version)))
    (if (= 4 type-id)
	(multiple-value-bind (lit after-lit consumed) (parse-literal after-type-id)
	  (values (new-literal version lit) after-lit (+ consumed 6)))
	(let ((len-type (car after-type-id))
	      (op-rest (cdr after-type-id)))
	  (if (= len-type 0)
	      (let ((len (to-int (subseq op-rest 0 15)))
		    (rest (nthcdr 15 op-rest))
		    (cons-so-far 0))
		(values (new-operator
			 version
			 (loop :while (< cons-so-far len)
			       :collect
			       (multiple-value-bind (packet remain consumed) (parse-packet rest)
				 (progn
				   (setq cons-so-far (+ cons-so-far consumed))
				   (setq rest remain)
				   packet))) type-id)
			rest (+ cons-so-far 22)))
	      (let ((len (to-int (subseq op-rest 0 11)))
		      (rest (nthcdr 11 op-rest))
		      (cons-so-far 0))
		  (values (new-operator
			   version
			   (loop :for i from 1 to len
				 :collect
				 (multiple-value-bind (packet remain consumed) (parse-packet rest)
				   (progn
				     (setq cons-so-far (+ cons-so-far consumed))
				     (setq rest remain)
				     packet))) type-id)
			  rest (+ cons-so-far 18))))))))

(defun parta ()
  (labels ((count-version (pkg)
	     (cond ((equal 'operator (type-of pkg))
		    (reduce (lambda (a b) (+ a (count-version b)))
			    (operator-subpackets pkg)
			    :initial-value (operator-version pkg)))
		   ((equal 'literal (type-of pkg))
		    (literal-version pkg)))))
    (let* ((input (get-nums))
	   (parsed (parse-packet input)))
      (count-version parsed))))

(defun interpret (pkg)
  (if (equal 'literal (type-of pkg))
      (literal-value pkg)
      (let ((children (mapcar 'interpret (operator-subpackets pkg)))
	    (op (operator-op pkg)))
	(cond ((= op 0) (reduce '+ children))
	      ((= op 1) (reduce '* children))
	      ((= op 2) (reduce 'min children))
	      ((= op 3) (reduce 'max children))
	      ((= op 5) (if (> (car children) (cadr children)) 1 0))
	      ((= op 6) (if (< (car children) (cadr children)) 1 0))
	      ((= op 7) (if (= (car children) (cadr children)) 1 0))))))

(defun partb ()
  (interpret (parse-packet (get-nums))))
