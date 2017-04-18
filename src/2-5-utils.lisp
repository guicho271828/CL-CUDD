
(in-package :cudd)

(defun map-ones (node fn)
  "Runs a DFS on a ZDD. It calls the given callback function when it reaches the 1-node.
The callback is called with an argument containing a bit vector which stores 1-bit.
Returns the node."
  (let ((bv (make-array (zdd-max-variables) :element-type 'bit))
        (one (cudd-read-one %mp%))
        (zero (cudd-read-zero %mp%)))
    (labels ((rec (p)
               (cond
                 ((pointer-eq p one)
                  (funcall fn bv))
                 ((pointer-eq p zero)
                  ;; do nothing
                  )
                 ((cudd-is-non-constant p)
                  (let ((index (cudd-node-read-index p)))
                    (setf (aref bv index) 1)
                    (rec (cudd-node-then p))
                    (setf (aref bv index) 0)
                    (rec (cudd-node-else p)))))))
      (rec (node-pointer node))))
  node)

(defmacro do-ones ((var dd) &body body)
  "Runs a DFS on a ZDD. BODY is executed for each path in a ZDD to the constant 1-node.
Entire body is wrapped in a block NIL.
Symbol VAR is lexically bound to a bit vector which stores 1-bit when a zdd variable is true on the path.
Returns the node."
  `(block nil
     (map-ones ,dd (lambda (,var) ,@body))))


(defun integer->zdd-unate (int)
  "Converts an integer to a zdd bit-by-bit."
  (declare (integer int))
  (let ((zdd (zdd-set-of-emptyset)))
    (dotimes (i (integer-length int) zdd)
      (when (logbitp i int)
        (setf zdd (zdd-change zdd i))))))

(defun integer->zdd-binate (int)
  "Converts an integer to a zdd bit-by-bit, in a binate representation. (ith bit is encoded into 2i and 2i+1 bits)"
  (declare (integer int))
  (let ((zdd (zdd-set-of-emptyset)))
    (dotimes (i (integer-length int) zdd)
      (setf zdd (zdd-change zdd (if (logbitp i int)
                                    (* i 2)
                                    (1+ (* i 2))))))))

(setf (fdefinition 'integer->zdd) #'integer->zdd-unate)

(defun bitvector->zdd (bv)
  "Converts a bit-vector to a zdd bit-by-bit."
  (declare (bit-vector bv))
  (let ((zdd (zdd-set-of-emptyset)))
    (dotimes (i (length bv) zdd)
      (when (= 1 (aref bv i))
        (setf zdd (zdd-change zdd i))))))
