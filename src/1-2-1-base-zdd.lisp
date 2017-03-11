;;; BDD : Binary Decision Diagram
(in-package :cl-cudd.baseapi)

;; cudd-bdd-not
;; cudd-bdd-cube

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

An ADD variable differs from a BDD variable because it points to the arithmetic zero,
instead of having a complement pointer to 1."
  (when (and index level)
    (error "BDD-VAR accepts at most one of I and LEVEL"))
  (cond
    (index (cudd-zdd-ith-var manager index))
    ;; this is not a typo: zdd uses bddNewVar to create variable; its node is a bdd node
    (level (cudd-bdd-new-var-at-level manager level))
    (t (cudd-bdd-new-var manager))))





