
(in-package :cudd)

;; elementary sets

(defun zdd-emptyset ()
  "Returns an empty set {}."
  (zero-node 'zdd-node))

(defun zdd-set-of-emptyset ()
  "Returns a set of an empty set {{}}."
  (one-node 'zdd-node))

(defun zdd-singleton (var)
  "Returns {{var}}."
  (make-var 'zdd-node :index var))

;; between a ZDD and a single variable

(defun zdd-subset-0 (zdd var)
  "Computes the subset of S that does not contain element VAR (integer)."
  (wrap-and-finalize
   (cudd-zdd-subset-0 %mp% (node-pointer zdd) var)
   'zdd-node))
(defun zdd-subset-1 (zdd var)
  "Computes the subset of S that contains element VAR (integer)."
  (wrap-and-finalize
   (cudd-zdd-subset-1 %mp% (node-pointer zdd) var)
   'zdd-node))
(defun zdd-change (zdd var)
  "Computes the subset of S that contains element VAR (integer)."
  (wrap-and-finalize
   (cudd-zdd-change %mp% (node-pointer zdd) var)
   'zdd-node))

;; between 2 ZDDs

(defun zdd-union (f g)
  "Computes the union of F and G."
  (wrap-and-finalize
   (cudd-zdd-union %mp% (node-pointer f) (node-pointer g))
   'zdd-node))
(defun zdd-intersection (f g)
  "Computes the intersection of F and G."
  (wrap-and-finalize
   (cudd-zdd-intersect %mp% (node-pointer f) (node-pointer g))
   'zdd-node))
(defun zdd-difference (f g)
  "Computes the difference of F and G."
  (wrap-and-finalize
   (cudd-zdd-diff %mp% (node-pointer f) (node-pointer g))
   'zdd-node))

(defun zdd-divide-unate (f g)
  "Computes the weak division of F by G (assumes unate representation)."
  (wrap-and-finalize
   (cudd-zdd-divide %mp% (node-pointer f) (node-pointer g))
   'zdd-node))
(defun zdd-divide-binate (f g)
  "Computes the weak division of F by G (assumes binate representation)."
  (wrap-and-finalize
   (cudd-zdd-weak-div %mp% (node-pointer f) (node-pointer g))
   'zdd-node))
(defun zdd-product-unate (f g)
  "Computes the product of F by G (assumes unate representation)."
  (wrap-and-finalize
   (cudd-zdd-unate-product %mp% (node-pointer f) (node-pointer g))
   'zdd-node))
(defun zdd-product-binate (f g)
  "Computes the product of F by G (assumes binate representation)."
  (wrap-and-finalize
   (cudd-zdd-product %mp% (node-pointer f) (node-pointer g))
   'zdd-node))


(defun zdd-remainder-unate (f g)
  "Computes the remainder of division of F by G (assumes unate representation)."
  (zdd-difference f (zdd-product-unate f (zdd-divide-unate f g))))
(defun zdd-remainder-binate (f g)
  "Computes the remainder of division of F by G (assumes binate representation)."
  (zdd-difference f (zdd-product-binate f (zdd-divide-binate f g))))

(defun zdd-count-minterm (f &optional support-size)
  "Computes the number of minterms in f.
SUPPORT-SIZE specifies the number of variables in the support of f, i.e.,
the number of the variables that F essentially depends on."
  (if support-size
      (cudd-zdd-count-minterm %mp% (node-pointer f) support-size)
      (cudd-zdd-count %mp% (node-pointer f))))

