;;;; ribbit.lisp
(in-package #:ribbit)

(defconstant +size+ 4)

;; Basics and internals
;; (defun make-vec (&rest elems)
;;   (make-array `(,+size+) :initial-contents (or elems (loop repeat +size+ collect 0))))
;; (defstruct ribbit (size-table (make-vec)) (depth 0) (vec (make-vec)))

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
    (values (apply #'concatenate 'vector (reverse head)) tail)))

(defun repartition (ct vecs)
  (let ((rest vecs))
    (loop while rest
       collect (multiple-value-bind (next rst) (take-across ct rest)
		 (setf rest rst)
		 next))))

;; Literal ribbit notation
(defun ribbit (&rest elems)
  (let ((rb (list elems)))
    (loop for next = (repartition +size+ rb)
       if (not (cdr next)) return (first next)
       else if (>= +size+ (length next)) return (coerce next 'vector)
       else do (setf rb (list (coerce next 'vector))))))

;; External interface
;; (these should probably just be hooked into native operations where possible. In particular index and concatenate)
(defun len (rb) :todo)
(defun ix (rb index) :todo)
(defun cat (&rest rbs) :todo)
(defun split (rb index) :todo)
(defun insert-at (rb index val) :todo)
