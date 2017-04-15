
(in-package :cudd)

;; elementary sets

(defun zdd-emptyset ()
  "Returns an empty set {}."
  (zero-node 'zdd-node))

(defun zdd-set-of-emptyset ()
  "Returns a set of an empty set {{}}."
  (one-node 'zdd-node))

(defun zdd-singleton (var)
  "Returns {{var}}. This is not equivalent to (make-var 'zdd-node :index var), see make-var documentation."
  (zdd-change (zdd-set-of-emptyset) var))

;; between a ZDD and a single variable

(defun zdd-subset-0 (zdd var)
  "Computes the subset of S that does not contain element VAR (integer)."
  (wrap-and-finalize
   (cudd-zdd-subset-0 %mp% (node-pointer zdd) var)
   'zdd-node))
(defun zdd-subset-1 (zdd var)
  "Computes the subset of S that contains element VAR (integer), and remove VAR from each combination."
  (wrap-and-finalize
   (cudd-zdd-subset-1 %mp% (node-pointer zdd) var)
   'zdd-node))
(defun zdd-change (zdd var)
  "Flip the membership of variable VAR in ZDD."
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

;; unate operations

(setf (fdefinition 'zdd-onset) #'zdd-subset-1
      (documentation 'zdd-onset 'function)
      "Computes the subset of S that contains element VAR (integer), and remove VAR from each combination. (same as zdd-subset-1)")

(setf (fdefinition 'zdd-offset) #'zdd-subset-0
      (documentation 'zdd-offset 'function)
      "selects the subset of the combinations each of which does not include var. (same as zdd-subset-1)")

(defun zdd-divide-unate (f g)
  "Computes the weak division of F by G (assumes unate representation).
cf. Shin-ichi Minato: Zero-Suppressed BDDs and Their Applications"
  (wrap-and-finalize
   (cudd-zdd-divide %mp% (node-pointer f) (node-pointer g))
   'zdd-node))
(defun zdd-product-unate (f g)
  "Computes the product of F by G (assumes unate representation).
cf. Shin-ichi Minato: Zero-Suppressed BDDs and Their Applications"
  (wrap-and-finalize
   (cudd-zdd-unate-product %mp% (node-pointer f) (node-pointer g))
   'zdd-node))

;; binate operations

(defun zdd-divide-binate (f g)
  "Computes the weak division of F by G (assumes binate representation).
cf. Shin-ichi Minato: Zero-Suppressed BDDs and Their Applications"
  (wrap-and-finalize
   (cudd-zdd-weak-div %mp% (node-pointer f) (node-pointer g))
   'zdd-node))
(defun zdd-product-binate (f g)
  "Computes the product of F by G (assumes binate representation).
cf. Shin-ichi Minato: Zero-Suppressed BDDs and Their Applications"
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
      (cudd-zdd-count-double %mp% (node-pointer f))))

