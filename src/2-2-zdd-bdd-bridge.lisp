(in-package :cudd)

;; CUDD-ZDD-VARS-FROM-BDD-VARS
;; CUDD-ZDD-PORT-TO-BDD
;; CUDD-ZDD-PORT-FROM-BDD

;; From the definition of DdManager, it seems like the BDD variables and the ZDD
;; variables are managed separately. In other words, they have sort-of
;; separate namespaces.

(defun bdd->zdd-simple (bdd)
  "Converts a BDD to a ZDD via simple 1-to-1 variable conversion.
This function internally calls cudd-zdd-vars-from-bdd-vars and increases the ZDD variable table size as necessary."
  (assert (typep bdd 'bdd-node))
  (let ((dd %mp%))
    ;; TODO check if there are zdd vars?
    (assert (= 1 (cudd-zdd-vars-from-bdd-vars dd 1)))
    (wrap-and-finalize
     (cudd-zdd-port-from-bdd dd (node-pointer bdd))
     'zdd-node)))

(defun zdd->bdd-simple (zdd)
  "Converts a ZDD to a BDD via simple 1-to-1 variable conversion."
  (assert (typep zdd 'zdd-node))
  (let ((dd %mp%))
    (wrap-and-finalize
     (cudd-zdd-port-to-bdd dd (node-pointer zdd))
     'bdd-node)))


(defun bdd->zdd-cover (bdd)
  "Converts a BDD to a ZDD via 1-to-2 conversion for cover representation.
This function internally calls cudd-zdd-vars-from-bdd-vars and increases the ZDD variable table size as necessary."
  (assert (typep bdd 'bdd-node))
  (let ((dd %mp%))
    (assert (= 1 (cudd-zdd-vars-from-bdd-vars dd 2)))
    (wrap-and-finalize
     (cudd-zdd-port-from-bdd dd (node-pointer bdd))
     'zdd-node)))

(defun zdd->bdd-cover (zdd)
  "Converts a BDD to a ZDD via 1-to-2 conversion for cover representation."
  (assert (typep zdd 'zdd-node))
  (let ((dd %mp%))
    (wrap-and-finalize
     (cudd-zdd-port-to-bdd dd (node-pointer zdd))
     'bdd-node)))

