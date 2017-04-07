;;; ADD: Arithmetic DD
(in-package :cl-cudd.baseapi)

(defun cudd-add-cube (manager vars)
  "Build an add cube out of the variables."
  (declare (foreign-pointer manager)
           (list vars))
  (let ((n (length vars)))
    (with-foreign-object (array :pointer n)
      (loop :for v :in vars
            :for i :from 0
            :do (setf (mem-aref array :pointer i) v))
      (cudd-add-compute-cube manager array (null-pointer) n))))

(defun add-var (manager &key index level)
  "Creates a new ADD variable. At most one of index and level may be given.

If neither index nor level are given, then the new variable has an index equal
to the largest previous index plus 1.

If index is given, then retrieves the ADD variable with the index if it already exists,
or creates a new ADD variable

If level is given, then the new variable has an index equal to the largest
previous index plus 1 and is positioned at the specified level in the order.

Returns a pointer to the new variable if successful;
invokes a signal otherwise.

The returned node is an internal node with THEN arc pointing to the constant 1
and ELSE arc pointing to the arithmetic zero."
  (declare (foreign-pointer manager))
  (when (and index level)
    (error "ADD-VAR accepts at most one of INDEX and LEVEL"))
  (cond
    (index (cudd-add-ith-var manager index))
    (level (cudd-add-new-var-at-level manager level))
    (t (cudd-add-new-var manager))))



