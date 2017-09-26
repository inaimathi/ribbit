;;;; ribbit.lisp
(in-package #:ribbit)

(defconstant +size+ 4)

;; Basics and internals
(defun make-vec (&rest elems)
  (make-array `(,+size+) :initial-contents (or elems (loop repeat +size+ collect 0))))
(defstruct ribbit (size-table nil) (depth 0) (vec #()))

(defun take-across (n vecs)
  (let ((ct n)
	(head nil)
	(tail nil))
    (loop for vs on vecs while (> ct 0)
       for v = (typecase (first vs)
		 (ribbit (ribbit-vec (first vs)))
		 (t (first vs)))
       do (cond ((and (null head)
		      (or (= ct (length v))
			  (= ct (+ 1 (length v)))))
		 (push (first vs) head)
		 (setf ct 0
		       tail (rest vs)))
		((> (length v) ct)
		    (push (subseq v 0 ct) head)
		    (setf tail (cons (subseq v ct) (rest vs))
			  ct 0))
		(t (push v head)
		   (decf ct (length v))))
       finally (unless tail (setf tail vs)))
    (if (cdr head)
	(values (apply #'concatenate 'vector (reverse head)) tail)
	(let ((v (first head)))
	  (values (if (or (ribbit-p v) (vectorp v))
		      v
		      (coerce v 'vector))
		  tail)))))

(defun repartition (ct vecs)
  (let ((rest vecs))
    (loop while rest
       collect (multiple-value-bind (next rst) (take-across ct rest)
		 (setf rest rst)
		 next))))

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
  (= +size+ (length (typecase ribbit
		      (ribbit (ribbit-vec ribbit))
		      (t ribbit)))))

(defun mk-ribbit (vec)
  (let* ((depth (if (ribbit-p (aref vec 0))
		    (+ 1 (ribbit-depth (aref vec 0)))
		    0))
	 (size-table (unless (or (zerop depth) (and (full-level? vec) (every #'full-level? vec)))
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
  (vecs->ribbit (list elems)))

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

(defun cat (&rest rbs)
  (let ((zeros nil))
    (labels ((find-zeros (rb)
	       (if (zerop (ribbit-depth rb))
		   (push rb zeros)
		   (loop for v across (ribbit-vec rb) do (find-zeros v)))))
      (mapc #'find-zeros rbs)
      (vecs->ribbit (ribbpartition +size+ (reverse zeros))))))

(defun split (rb index) :todo)
(defun insert-at (rb index val) :todo)
