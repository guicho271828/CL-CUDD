
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

(defun zdd-set (zdd var)
  "Add a variable VAR; i.e. force the value of VAR to be true"
  (wrap-and-finalize
   (let ((then (cudd-zdd-subset-1 %mp% (node-pointer zdd) var)))
     (cudd-ref then)
     (let ((else (cudd-zdd-subset-0 %mp% (node-pointer zdd) var)))
       (cudd-ref else)
       (let ((union (cudd-zdd-union %mp% then else)))
         (cudd-ref union)
         (cudd-recursive-deref-zdd %mp% then)
         (cudd-recursive-deref-zdd %mp% else)
         (let ((result (cudd-zdd-change %mp% union var)))
           (cudd-ref result)
           (cudd-recursive-deref-zdd %mp% union)
           result))))
   'zdd-node t nil))

(defun zdd-unset (zdd var)
  "Remove a variable VAR; i.e. force the value of VAR to be false"
  (wrap-and-finalize
   (let ((then (cudd-zdd-subset-1 %mp% (node-pointer zdd) var)))
     (cudd-ref then)
     (let ((else (cudd-zdd-subset-0 %mp% (node-pointer zdd) var)))
       (cudd-ref else)
       (let ((union (cudd-zdd-union %mp% then else)))
         (cudd-ref union)
         (cudd-recursive-deref-zdd %mp% then)
         (cudd-recursive-deref-zdd %mp% else)
         union)))
   'zdd-node t nil))

(defun zdd-dont-care (zdd var)
  "Direct the both arcs of the VAR'th node to the next index.
If it does not exist (i.e. then-arc points to 0 and zero-suppressed) creates a new node."
  (wrap-and-finalize
   (let ((flipped (cudd-zdd-change %mp% (node-pointer zdd) var)))
     (cudd-ref flipped)
     (let ((union (cudd-zdd-union %mp% (node-pointer zdd) flipped)))
       (cudd-ref union)
       (cudd-recursive-deref-zdd %mp% flipped)
       union))
   'zdd-node t nil))

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

(defun zdd-union* (&rest args)
  "Performs zdd-union on all variables."
  (cond
    ((second args)
     (reduce #'zdd-union args))
    (args
     (first args))
    ((null args)
     (zdd-emptyset))))

(defun zdd-intersection* (first &rest args)
  "Performs zdd-intersection on all variables.
Null intersection (union of all combinations) is undefined because
ZDD has no upper limit on the number of variables."
  (if args
      (reduce #'zdd-intersection args :initial-value first)
      first))


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
(defun zdd-remainder-unate (f g)
  "Computes the remainder of division of F by G (assumes unate representation)."
  (wrap-and-finalize
   (let ((p1 (cudd-zdd-divide %mp% (node-pointer f) (node-pointer g))))
     (cudd-ref p1)
     (let ((p2 (cudd-zdd-unate-product %mp% (node-pointer f) p1)))
       (cudd-ref p2)
       (cudd-recursive-deref-zdd %mp% p1)
       (let ((p3 (cudd-zdd-diff %mp% (node-pointer f) p2)))
         (cudd-ref p3)
         (cudd-recursive-deref-zdd %mp% p2)
         p3)))
   'zdd-node t nil))

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
(defun zdd-remainder-binate (f g)
  "Computes the remainder of division of F by G (assumes binate representation)."
  (wrap-and-finalize
   (let ((p1 (cudd-zdd-weak-div %mp% (node-pointer f) (node-pointer g))))
     (cudd-ref p1)
     (let ((p2 (cudd-zdd-product %mp% (node-pointer f) p1)))
       (cudd-ref p2)
       (cudd-recursive-deref-zdd %mp% p1)
       (let ((p3 (cudd-zdd-diff %mp% (node-pointer f) p2)))
         (cudd-ref p3)
         (cudd-recursive-deref-zdd %mp% p2)
         p3)))
   'zdd-node t nil))

(defun zdd-count-minterm (f &optional support-size)
  "Computes the number of minterms in f.
SUPPORT-SIZE specifies the number of variables in the support of f, i.e.,
the number of the variables that F essentially depends on."
  (if support-size
      (cudd-zdd-count-minterm %mp% (node-pointer f) support-size)
      (cudd-zdd-count-double %mp% (node-pointer f))))

