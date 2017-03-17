(in-package :cudd)

(defun bdd->add (bdd)
  "Converts a BDD to a 0-1 ADD"
  (declare (bdd-node bdd))
  (wrap-and-finalize
   (cudd-bdd-to-add (manager-pointer *manager*) (node-pointer bdd))
   'add-node))

(defun add->bdd (add)
  "Converts an ADD to a BDD by replacing all discriminants different from 0 with 1."
  (declare (add-node add))
  (wrap-and-finalize
   (cudd-add-bdd-pattern (manager-pointer *manager*) (node-pointer add))
   'bdd-node))

(defun add->bdd-interval (add lower upper)
  "Converts an ADD to a BDD by replacing all discriminants greater than or equal to lower and less
  than or equal to upper with 1, and all other discriminants with 0."
  (declare (add-node add))
  (wrap-and-finalize (cudd-add-bdd-interval
                      (manager-pointer *manager*)
                      (node-pointer add)
                      lower upper)
                     'bdd-node))

(defun add->bdd-strict-threshold (add threshold)
  "Converts an ADD to a BDD by replacing all discriminants STRICTLY greater than value with 1, and
  all other discriminants with 0."
  (declare (add-node add))
  (wrap-and-finalize (cudd-add-bdd-strict-threshold
                      (manager-pointer *manager*)
                      (node-pointer add)
                      threshold)
                     'bdd-node))

(defun add->bdd-threshold (add threshold)
  "Converts an ADD to a BDD by replacing all discriminants greater than or equal to value with 1, and
  all other discriminants with 0."
  (declare (add-node add))
  (wrap-and-finalize (cudd-add-bdd-threshold
                      (manager-pointer *manager*)
                      (node-pointer add)
                      threshold)
                     'bdd-node))

