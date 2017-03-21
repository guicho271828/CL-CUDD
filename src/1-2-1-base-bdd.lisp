;;; BDD : Binary Decision Diagram
(in-package :cl-cudd.baseapi)

(declaim (ftype managed-node-operation
                cudd-bdd-not))
(defun cudd-bdd-not (manager node)
  "Mark the node as a negated node. See CUDD documentation Sec.4.3 Complement Arc.
This function is not imported from CFFI because the corresponding implementation in CUDD
 is written as a macro. See cudd.h l.334"
  (declare (ignore manager))
  ;; TODO What happens on big-endian machines?
  (let ((result (make-pointer (logxor 1 (pointer-address node)))))
    (cudd-ref result)
    result))

(defun cudd-bdd-cube (manager vars)
  "Build an bdd cube out of the variables."
  (declare (foreign-pointer manager)
           (list vars))
  (let ((n (length vars)))
    (with-foreign-object (array :pointer n)
      (loop :for v :in vars
            :for i :from 0
            :do (setf (mem-aref array :pointer i) v))
      (cudd-bdd-compute-cube manager array (null-pointer) n))))

(defun bdd-var (manager &key index level)
  "Creates a new BDD variable. At most one of INDEX and LEVEL may be given.

If neither INDEX nor LEVEL are given, then the new variable has an index equal
to the largest previous index plus 1.

If index is given, then retrieves the BDD variable with the index if it already exists,
or creates a new BDD variable

If level is given, then the new variable has an index equal to the largest
previous index plus 1 and is positioned at the specified level in the order.

Returns a pointer to the new variable if successful;
invokes a signal otherwise.

The returned node is an internal node with both outgoing arcs
pointing to the constant 1. The else arc is complemented."
  (declare (foreign-pointer manager))
  (when (and index level)
    (error "BDD-VAR accepts at most one of INDEX and LEVEL"))
  (cond
    (index (cudd-bdd-ith-var manager index))
    (level (cudd-bdd-new-var-at-level manager level))
    (t (cudd-bdd-new-var manager))))





