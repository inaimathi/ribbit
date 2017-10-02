;;;; ribbit.lisp
(in-package #:ribbit)

(defun test-things (cat-fn)
  (list
   (let ((r (ribbit 1 2 3 4)))
     (eq r (aref (ribbit-vec (funcall cat-fn r (ribbit 1 2))) 0)))
   (let ((r (ribbit 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
     (eq r (aref (ribbit-vec (funcall cat-fn r (ribbit 1 2 3 4))) 0)))
   (let ((r (ribbit 1))
	 (r2 (smartish-cat (ribbit 1 2 3 4 5 6 7) (ribbit 8 9 10 11 12 13 14))))
     (eq (aref (ribbit-vec r2) 2)
	 (aref (ribbit-vec (funcall cat-fn r r2)) 2)))))

(defconstant +size+ 4)

;; Basics and internals
(defstruct ribbit (size-table nil) (depth 0) (vec #()))

(defun take-across (n vecs)
  (let ((ct n)
	(head nil)
	(tail nil))
    (loop for vs on vecs while (> ct 0)
       for v = (typecase (first vs)
		 (ribbit (ribbit-vec (first vs)))
		 (t (first vs)))
       do (cond ((> (length v) ct)
		 (push (subseq v 0 ct) head)
		 (setf tail (cons (subseq v ct) (rest vs))
		       ct 0))
		(t (push v head)
		   (decf ct (length v))))
       finally (unless tail (setf tail vs)))
    (values
     (let ((v (first head)))
       (cond ((and (not (cdr head))
		   (or (ribbit-p v) (vectorp v)))
	      v)
	     ((not (cdr head))
	      (coerce v 'vector))
	     (t
	      (apply #'concatenate 'vector (reverse head)))))
     tail)))

(defun ribbpartition (ct rbs)
  (let ((rest rbs))
    (loop while rest
       collect (multiple-value-bind (next rst) (take-across ct rest)
		 (setf rest rst)
		 (if (ribbit-p next)
		     next
		     (mk-ribbit next))))))

;; Literal ribbit notation
(defun full-level? (ribbit)
  (= +size+ (length (ribbit-vec ribbit))))

(defun mk-ribbit (vec)
  (let* ((depth (if (ribbit-p (aref vec 0))
		    (+ 1 (ribbit-depth (aref vec 0)))
		    0))
	 (size-table (unless (or (zerop depth) (and (= +size+ (length vec)) (every #'full-level? vec)))
		       (coerce
			(let ((s 0))
			  (loop for e across vec
			     do (incf s (len e)) collect s))
			'vector))))
    (make-ribbit :depth depth :vec vec :size-table size-table)))

(defun vecs->ribbit (vs)
  (let ((rb vs))
    (loop for next = (ribbpartition +size+ rb)
       if (not (cdr next)) return (first next)
       else if (>= +size+ (length next)) return (mk-ribbit (coerce next 'vector))
       else do (setf rb (list (coerce next 'vector))))))

(defun ribbit (&rest elems)
  (if elems
      (vecs->ribbit (list elems))
      (make-ribbit)))

;; External interface
;; (these should probably just be hooked into native operations where possible. In particular index and concatenate)
(defun len (rb)
  (cond ((vectorp rb) (length rb))
	((zerop (ribbit-depth rb))
	 (length (ribbit-vec rb)))
	((null (ribbit-size-table rb))
	 (expt +size+ (+ 1 (ribbit-depth rb))))
	(t (let ((szs (ribbit-size-table rb)))
	     (aref szs (- (length szs) 1))))))

(defun ix (rb index) :todo)
(defun split (rb index) :todo)
(defun insert-at (rb index val) :todo)

;;;;; Smart CAT stuff
(defun stupid-cat (a b)
  (let ((zeros nil))
    (labels ((find-zeros (rb)
	       (if (zerop (ribbit-depth rb))
		   (push rb zeros)
		   (loop for v across (ribbit-vec rb) do (find-zeros v)))))
      (mapc #'find-zeros (list a b))
      (vecs->ribbit (ribbpartition +size+ (reverse zeros))))))

(defun reusable? (ribbit)
  (let ((l (len ribbit))
	(max (expt +size+ (+ 1 (ribbit-depth ribbit)))))
    (or (= l max)
	(= l (- max 1))
	(and (= l (- +size+ 1))
	     (every #'reusable? (ribbit-vec ribbit))))))

(defun raise-to (depth ribbit)
  (if (>= (ribbit-depth ribbit) depth)
      ribbit
      (let ((r ribbit))
	(loop until (= (ribbit-depth r) depth)
	   do (setf r (mk-ribbit (vector r))))
	r)))

(defun prune-to (level ribbit)
  (if (= level (ribbit-depth ribbit))
      (list ribbit)
      (let ((rbs (list ribbit)))
	(loop until (= level (ribbit-depth (first rbs)))
	   do (setf rbs (loop for r in rbs
			   append (coerce (ribbit-vec ribbit) 'list))))
	rbs)))

(defun max-reusable-level (ribbit)
  (cond ((reusable? ribbit) (ribbit-depth ribbit))
	((zerop (ribbit-depth ribbit)) nil)
	(t (loop for r across (ribbit-vec ribbit)
	      for d = (max-reusable-level r)
	      if d do (return d)))))

(defun smartish-cat (a b)
  (let ((d (ribbit-depth a)))
    (cond ((and (reusable? a) (>= d (ribbit-depth b)))
	   (mk-ribbit (vector a (raise-to d b))))
	  ((reusable? a)
	   (apply #'ribbit a (prune-to d b)))
	  ((not (zerop d))
	   (let ((lv (max-reusable-level a)))
	     (if lv
		 (loop for r on (prune-to lv a)
		    when (reusable? (first r)) collect (first r) into reusables
		    finally (return (apply
				     #'ribbit
				     (append
				      reusables
				      (ribbpartition +size+ (append r (prune-to lv (raise-to lv b))))))))
		 (stupid-cat a b))))
	  (t (stupid-cat a b)))))
