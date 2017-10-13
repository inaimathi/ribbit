;;;; ribbit.lisp
(in-package #:ribbit)

(defun test-things ()
  (list
   (let ((r (ribbit 1 2 3 4)))
     (eq r (aref (ribbit-vec (cat r (ribbit 1 2))) 0)))
   (let ((r (ribbit 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
     (eq r (aref (ribbit-vec (cat r (ribbit 1 2 3 4))) 0)))
   (let ((r (ribbit 1))
	 (r2 (cat (ribbit 1 2 3 4 5 6 7) (ribbit 8 9 10 11 12 13 14))))
     (eq (aref (ribbit-vec r2) 2)
	 (aref (ribbit-vec (cat r r2)) 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Basics and literal notation
(defparameter *m* 4)
(defparameter *n* (- *m* 1))

(defstruct ribbit (size-table nil) (depth 0) (vec #()))

(defun take-across (depth rbs)
  (if (not (cdr rbs))
      (values (first rbs) nil)
      (let ((ct *m*)
	    (head nil)
	    (tail nil))
	(loop for rs on rbs while (> ct 1)
	   for r = (first rs)
	   for v = (ribbit-vec r)
	   do (cond ((> (length v) ct)
		     (push (subseq v 0 ct) head)
		     (setf tail (cons (mk-ribbit depth (subseq v ct)) (rest rs))
			   ct 0))
		    (t (push v head)
		       (decf ct (length v))))
	   finally (unless tail (setf tail rs)))
	(values
	 (mk-ribbit depth (apply #'concatenate 'vector (reverse head)))
	 tail))))

(defun repartition (depth rbs)
  (let ((rest rbs))
    (loop while rest for r = (pop rest)
       if (reusable? r) collect r
       else collect (multiple-value-bind (next rst) (take-across depth (cons r rest))
		      (setf rest rst)
		      next))))

(defun full-level? (ribbit) (= *m* (length (ribbit-vec ribbit))))

(defun reusable? (ribbit)
  (let* ((d (ribbit-depth ribbit))
	 (ct (length (ribbit-vec ribbit)))
	 (reusable-lv (or (= *m* ct) (= *n* ct))))
    (or (and (zerop d) reusable-lv)
	(let ((max (expt *m* (+ 1 d)))
	      (l (len ribbit)))
	  (or (= l max) (= l (- max 1))))
	(and reusable-lv
	     (every #'reusable? (ribbit-vec ribbit))))))

(defun reusable? (ribbit)
  (let* ((d (ribbit-depth ribbit))
	 (ct (length (ribbit-vec ribbit)))
	 (reusable-lv (or (= *m* ct) (= *n* ct))))
    (or (and (zerop d) reusable-lv)
	(let ((max (expt *m* (+ 1 d)))
	      (l (len ribbit)))
	  (or (= l max) (= l (- max 1))))
	(and reusable-lv
	     (every #'reusable? (ribbit-vec ribbit))))))

(defun compute-size-table (depth vec)
  (unless (or (zerop depth) (and (= *m* (length vec)) (every #'full-level? vec)))
    (coerce
     (let ((s 0))
       (loop for e across vec
	  do (incf s (len e)) collect s))
     'vector)))

(defun mk-ribbit (depth vec)
  (make-ribbit :size-table (compute-size-table depth vec) :depth depth :vec vec))

(defun ribbit-level (depth elems)
  (let ((es elems))
    (loop while es
       collect (mk-ribbit
		depth (coerce
		       (loop repeat *m* while es
			  for e = (pop es) collect e)
		       'vector)))))

(defun ribbit-from (depth elems)
  (let ((rbs (ribbit-level depth elems))
	(d depth))
    (loop while (cdr rbs)
       do (setf rbs (ribbit-level (incf d) rbs)))
    (first rbs)))

(defun ribbit (&rest elems)
  (if elems
      (ribbit-from 0 elems)
      (make-ribbit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; cat and associated plumbing
(defun raise-to (depth ribbit)
  (if (>= (ribbit-depth ribbit) depth)
      ribbit
      (let ((r ribbit)
	    (d (ribbit-depth ribbit)))
	(loop until (= d depth)
	   do (setf r (mk-ribbit (incf d) (vector r))))
	r)))

(defun prune-to (depth ribbit)
  (if (= depth (ribbit-depth ribbit))
      (list ribbit)
      (let ((rbs (list ribbit)))
	(loop until (= depth (ribbit-depth (first rbs)))
	   do (setf rbs (loop for r in rbs append (coerce (ribbit-vec ribbit) 'list))))
	rbs)))

(defun max-reusable-level (ribbit)
  (cond ((reusable? ribbit) (ribbit-depth ribbit))
	((zerop (ribbit-depth ribbit)) nil)
	(t (loop for r across (ribbit-vec ribbit)
	      for d = (max-reusable-level r)
	      if d do (return d)))))

(defun cat (a b)
  (let* ((d (ribbit-depth a))
	 (max-d (or (max-reusable-level a) 0)))
    (cond ((and (reusable? a) (>= d (ribbit-depth b)))
	   (mk-ribbit (+ d 1) (vector a (raise-to d b))))
	  ((reusable? a)
	   (ribbit-from (+ d 1) (cons a (prune-to d b))))
	  (t
	   (ribbit-from
	    (+ max-d 1)
	    (repartition
	     max-d (append
		    (prune-to max-d a)
		    (prune-to max-d (raise-to max-d b)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Other external methods
(defun len (rb)
  (cond ((zerop (ribbit-depth rb))
	 (length (ribbit-vec rb)))
	((null (ribbit-size-table rb))
	 (expt *m* (+ 1 (ribbit-depth rb))))
	(t (let ((szs (ribbit-size-table rb)))
	     (aref szs (- (length szs) 1))))))

(defun set! (rb ix val) :todo)
(defun slice (rb &key from to) :todo)
(defun insert-at (rb ix val) :todo)
(defun split-at (rb ix) :todo)
(defun ix (rb ix) :todo)
(defun traverse (rb fn) :todo)
