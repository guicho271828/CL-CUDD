
(in-package :cudd)

(defmacro zdd-ref-let* (bindings &body body)
  "For internal use only. Bind variables like let*, call cudd-ref, execute body,
then calls cudd-recursive-deref-zdd in unwind-protect.

However each binding may have an optional third element i.e. (var val no-deref),
where no-deref, when evaluates to non-nil, disables the call to cudd-recursive-deref-zdd.
"
  (ematch bindings
    (nil `(progn ,@body))
    ((list* (list var form) rest)
     `(let ((,var ,form))
        (cudd-ref ,var)
        (unwind-protect (zdd-ref-let* ,rest ,@body)
          (cudd-recursive-deref-zdd %mp% ,var))))
    ((list* (list var form) rest)
     `(let ((,var ,form))
        (cudd-ref ,var)
        (unwind-protect (zdd-ref-let* ,rest ,@body)
          (cudd-recursive-deref-zdd %mp% ,var))))
    ((list* (list var form t) rest)
     `(let ((,var ,form))
        (cudd-ref ,var)
        (zdd-ref-let* ,rest ,@body)))
    ((list* (list var form no-deref) rest)
     `(let ((,var ,form))
        (cudd-ref ,var)
        (if ,no-deref
            (zdd-ref-let* ,rest ,@body)
            (unwind-protect (zdd-ref-let* ,rest ,@body)
              (cudd-recursive-deref-zdd %mp% ,var)))))))

;;;; elementary sets

(defun zdd-emptyset ()
  "Returns an empty set {}."
  (zero-node 'zdd-node))

(defun zdd-set-of-emptyset ()
  "Returns a set of an empty set {{}}."
  (one-node 'zdd-node))

(defun zdd-singleton (var)
  "Returns {{var}}. This is not equivalent to (make-var 'zdd-node :index var), see make-var documentation."
  (zdd-change (zdd-set-of-emptyset) var))

;;;; between a ZDD and a single variable

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
   (zdd-ref-let* ((then (cudd-zdd-subset-1 %mp% (node-pointer zdd) var))
                  (else (cudd-zdd-subset-0 %mp% (node-pointer zdd) var))
                  (union (cudd-zdd-union %mp% then else))
                  (result (cudd-zdd-change %mp% union var) t))
     result)
   'zdd-node t nil))

(defun zdd-unset (zdd var)
  "Remove a variable VAR; i.e. force the value of VAR to be false"
  (wrap-and-finalize
   (zdd-ref-let* ((then (cudd-zdd-subset-1 %mp% (node-pointer zdd) var))
                  (else (cudd-zdd-subset-0 %mp% (node-pointer zdd) var))
                  (union (cudd-zdd-union %mp% then else) t))
     union)
   'zdd-node t nil))

(defun zdd-dont-care (zdd var)
  "Direct the both arcs of the VAR'th node to the next index.
If it does not exist (i.e. then-arc points to 0 and zero-suppressed) creates a new node."
  (wrap-and-finalize
   (zdd-ref-let* ((flipped (cudd-zdd-change %mp% (node-pointer zdd) var))
                  (union (cudd-zdd-union %mp% (node-pointer zdd) flipped) t))
     union)
   'zdd-node t nil))

;;;; between 2 ZDDs

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

;;;; unate operations

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
   (zdd-ref-let* ((p1 (cudd-zdd-divide %mp% (node-pointer f) (node-pointer g)))
                  (p2 (cudd-zdd-unate-product %mp% (node-pointer f) p1))
                  (p3 (cudd-zdd-diff %mp% (node-pointer f) p2) t))
     p3)
   'zdd-node t nil))

;; aliasing
(setf (fdefinition 'zdd-product) #'zdd-product-unate)
(setf (fdefinition 'zdd-divide) #'zdd-divide-unate)
(setf (fdefinition 'zdd-remainder) #'zdd-remainder-unate)

;;;; binate operations

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
   (zdd-ref-let* ((p1 (cudd-zdd-weak-div %mp% (node-pointer f) (node-pointer g)))
                  (p2 (cudd-zdd-product %mp% (node-pointer f) p1))
                  (p3 (cudd-zdd-diff %mp% (node-pointer f) p2) t))
     p3)
   'zdd-node t nil))

(defun zdd-count-minterm (f &optional support-size)
  "Computes the number of minterms in f.
SUPPORT-SIZE specifies the number of variables in the support of f, i.e.,
the number of the variables that F essentially depends on."
  (if support-size
      (cudd-zdd-count-minterm %mp% (node-pointer f) support-size)
      (cudd-zdd-count-double %mp% (node-pointer f))))


;;;; reimplementing set operations in Extra package

#|

NOTE: use the native CUDD cache.

|#

(let ((op (new-cached-operator 2)))
  (defun cudd-zdd-supset (dd f g)
    (let* ((one (cudd-read-one dd))
           (zero (cudd-read-zero dd)))
      (labels ((rec (f g)
                 (macrolet ((retnull (x)
                              `(when (null-pointer-p ,x) (return-from rec ,x))))
                   (cond
                     ((pointer-eq f g)    f)
                     ((pointer-eq zero f) zero)
                     ((pointer-eq zero g) zero)
                     ((pointer-eq one f)  one)
                     ((pointer-eq one g)
                      (if (cudd-zdd-empty-belongs dd f) one zero))
                     (t
                      (let ((zres (cudd-cache-lookup-2-zdd dd op f g)))
                        (when (not (null-pointer-p zres))
                          (return-from rec zres)))
                      (let ((i1 (cudd-node-level-zdd dd f))
                            (i2 (cudd-node-level-zdd dd g)))
                        (cond
                          ((< i1 i2)
                           ;; all NULLs are derived from external function calls, but they
                           ;; are already handled by type translators.
                           (rec (cudd-node-else f) g))
                          ((= i1 i2)
                           (zdd-ref-let* ((ztmp (cudd-zdd-union dd (cudd-node-then g) (cudd-node-else g)))
                                          (zres0 (rec (cudd-node-else f) ztmp))
                                          (zres1 (rec (cudd-node-then f) (cudd-node-then g)))
                                          (zres (cudd-zdd-get-node dd (cudd-node-index f) zres1 zres0) t))
                             ;; cudd-zdd-get-node / cudd-zdd-unique-inter increases the zres0/zres1 refcount.
                             ;; since we already called cudd_ref on them, this effect should be cancelled
                             (cudd-deref zres0)
                             (cudd-deref zres1)
                             (cudd-cache-insert-2 dd op f g zres)
                             zres))      ;is returned referenced
                          (t
                           (zdd-ref-let* ((ztmp (cudd-zdd-union dd (cudd-node-then g) (cudd-node-else g)))
                                          (zres (rec f ztmp) t))
                             (cudd-cache-insert-2 dd op f g zres)
                             zres)))))))))      ;is returned referenced
        (rec f g)))))


(defun zdd-supset (f g)
  "Returns the subset of F whose element is a superset of at least one element of G. {p ∈ P | ∃q ∈ Q p ⊇ q}

Coudert, Olivier, Jean Christophe Madre, and Henri Fraisse. \"A new viewpoint on two-level logic minimization.\"
Design Automation, 1993. 30th Conference on.

Reference implementation is available in Extra libnrary by Alan Mishchenko.
https://people.eecs.berkeley.edu/~alanmi/research/extra/
"
  (wrap-and-finalize
   (cudd-zdd-supset %mp% (node-pointer f) (node-pointer g))
   'zdd-node))

;; (defun zdd-maximal (f)
;;   (wrap-and-finalize
;;    'zdd-node t nil))

;; (defun zdd-minimal (f)
;;   (wrap-and-finalize
;;    'zdd-node t nil))

;; (defun zdd-subset (f)
;;   (wrap-and-finalize
;;    'zdd-node t nil))



