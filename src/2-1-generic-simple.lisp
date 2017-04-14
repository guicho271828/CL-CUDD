(in-package :cudd)

;; high-level APIs that require CLOS-based dispatching for bdds and adds
;; This file contains only the simple operators

(def-cudd-call node-or ((:add (lambda (mgr f g) (cudd-add-apply mgr +or+ f g))
                         :bdd cudd-bdd-or) (f :node) (g :node))
  :generic "Disjunction of two 0-1 ADDs or two BDDs."
  :add     "Disjunction of two 0-1 ADDs."
  :bdd     "Disjunction of two BDDs.")

(def-cudd-call node-and ((:add (lambda (mgr f g) (cudd-add-apply mgr +times+ f g))
                         :bdd cudd-bdd-and) (f :node) (g :node))
  :generic "Conjunction of two 0-1 ADDs or two BDDs."
  :add     "Conjunction of two 0-1 ADDs."
  :bdd     "Conjunction of two BDDs.")

(def-cudd-call node-complement ((:add cudd-add-cmpl :bdd cudd-bdd-not
                                 :zdd cudd-zdd-complement)
                                (node :node))
  :generic "Computes the complement of a node a la C language:
The complement of 0 is 1 and the complement of everything else is 0."
  :add "Computes the complement of an ADD a la C language:
The complement of 0 is 1 and the complement of everything else is 0."
  :bdd "Complements a DD by flipping the complement attribute of the
pointer (the least significant bit)."
  :zdd "Complements a unate ZDD.")

(def-cudd-call if-then-else ((:add cudd-add-ite :bdd cudd-bdd-ite :zdd cudd-zdd-ite)
                             (f :node) (g :node) (h :node))
  :generic "Return a new DD-node for with F being the top-node, G being the then-branch
and H being the else branch"
  :add "Implements ITE(f,g,h). This procedure assumes that f is a 0-1 ADD."
  :bdd "Implements ITE(f,g,h)."
  :zdd "Implements ITE(f,g,h).")

(defun cube (nodes type)
  "
A cube, or product, is a boolean product of literals.
Build a cube from a list of nodes. TYPE defines which nodes we have
in the list of nodes: ADD-NODE or BDD-NODE"
  (wrap-and-finalize
   (ecase type
     (bdd-node (cudd-bdd-cube %mp% (map 'list #'node-pointer nodes)))
     (add-node (cudd-add-cube %mp% (map 'list #'node-pointer nodes))))
   (or type (type-of (first-elt nodes)))))

(defun make-var (type &key level index)
  "Creates a new DD variable (projection function). At most one of index and level may be given.

If neither index nor level are given, then the new variable has an index equal
to the largest previous index plus 1.

If index is given, then retrieves the DD variable with the index if it already exists,
or creates a new DD variable.

If level is given, then the new variable has an index equal to the largest
previous index plus 1 and is positioned at the specified level in the order.

Returns a pointer to the new variable if successful;
invokes a signal otherwise.
The returned node has the following properties depending on the type:

type = BDD-NODE: The returned node is an internal node with both outgoing arcs
pointing to the constant 1. The else arc is complemented.

type = ADD-NODE: The returned node is an internal node with THEN arc pointing to the constant 1
and ELSE arc pointing to the arithmetic zero.

type = ZDD-NODE: The returned node is the root of N+1 nodes,
where N is the maximum number of variables currently recognized by the manager.
This is because that's the way ZDD represents a projection function of a single variable.
When index = 2 and N = 4, the resulting ZDD is as follows:

                then
 (root)-(0)=(1)=(2)-(3)=(4)=[1]
                +----------[0]
                else "
  (declare (node-type type))
  (ecase type
    (bdd-node (wrap-and-finalize (bdd-var %mp% :index index :level level) type nil nil))
    (add-node (wrap-and-finalize (add-var %mp% :index index :level level) type t t))
    (zdd-node (wrap-and-finalize (zdd-var %mp% :index index :level level) type t t))))

(defun node-then (type node)
  "Return the then child of an inner node"
  (declare (node-type type))
  (assert (not (node-constant-p node)))
  (wrap-and-finalize (cudd-node-then %mp% (node-pointer node)) type))

(defun node-else (type node)
  "Return the else child of an inner node"
  (declare (node-type type))
  (assert (not (node-constant-p node)))
  (wrap-and-finalize (cudd-node-else %mp% (node-pointer node)) type))

#|

 Although the manual says that the constant 1 is same across A/B/ZDDs and
  constant 0 is obtained by Cudd_ReadZero,
 somehow, CUDD-READ-ZERO and CUDD-READ-ONE did not work for ADDs.

|#

(defun zero-node (type)
  "Return the zero node for the correspoinding type.
BDD: the logical zero node (boolean 0).
ADD: the arithmetic zero node (0.0d0).
ZDD: the arithmetic zero node (0.0d0). (Same as ADD)"
  (declare (node-type type))
  (wrap-and-finalize
   (ecase type
     (bdd-node (cudd-read-logic-zero %mp%))
     (add-node (cudd-read-zero %mp%))
     (zdd-node (cudd-read-zero %mp%))) type))

(defun one-node (type)
  "return the one node."
  (declare (node-type type))
  (wrap-and-finalize (cudd-read-one %mp%) type))

