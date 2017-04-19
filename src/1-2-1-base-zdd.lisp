;;; ZDD : Zero-suppressed Decision Diagram
(in-package :cl-cudd.baseapi)

(defun zdd-var (manager &key index level)
  "Creates a new ZDD variable. At most one of INDEX and LEVEL may be given.

If neither INDEX nor LEVEL are given, then the new variable has an index equal
to the largest previous index plus 1.

If index is given, then retrieves the BDD variable with the index if it already exists,
or creates a new BDD variable

If level is given, then the new variable has an index equal to the largest
previous index plus 1 and is positioned at the specified level in the order.

Returns a pointer to the new variable if successful;
invokes a signal otherwise.

type = ZDD-NODE: The returned node is the root (index 0) of N+1 nodes,
where N is the maximum number of variables currently recognized by the manager.
This is because that's the way ZDD represents a projection function of a single variable.
When index = 2 and N = 4, the resulting ZDD is as follows:

                then
(root)-(0)=(1)=(2)-(3)=(4)=[1]
                +----------[0]
                else
"
  (declare (foreign-pointer manager))
  (when (and index level)
    (error "BDD-VAR accepts at most one of I and LEVEL"))
  (cond
    (index (cudd-zdd-ith-var manager index))
    ;; this is not a typo: zdd uses bddNewVar to create variable; its node is a bdd node
    (level (cudd-bdd-new-var-at-level manager level))
    (t (cudd-bdd-new-var manager))))


(defun cudd-zdd-empty-belongs (dd node-ptr)
  (do ()
      ((cudd-node-is-constant node-ptr))
    (setf node-ptr (cudd-node-else node-ptr)))
  (pointer-eq (cudd-read-one dd) node-ptr))


