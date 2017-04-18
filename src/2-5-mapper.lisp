
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
