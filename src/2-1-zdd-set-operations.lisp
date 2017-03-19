
(in-package :cudd)

(defvar *unimplemented*
    '(
      ;; CUDD-ZDD-CHANGE
      ;; CUDD-ZDD-COMPLEMENT
      ;; CUDD-ZDD-COUNT      ------- returns int, internal function
      ;; CUDD-ZDD-COUNT-DOUBLE ------- returns double, internal function
      CUDD-ZDD-COUNT-MINTERM
      CUDD-ZDD-COVER-PATH-TO-STRING
      ;; CUDD-ZDD-DAG-SIZE
      ;; CUDD-ZDD-DIFF
      CUDD-ZDD-DIFF-CONST
      ;; CUDD-ZDD-DIVIDE                   ; unate version of CUDD-ZDD-WEAK-DIV
      ;; CUDD-ZDD-DIVIDE-F                 ; obsoleted version of CUDD-ZDD-DIVIDE
      ;; CUDD-ZDD-DUMP-DOT --- test
      CUDD-ZDD-FIRST-PATH
      ;; CUDD-ZDD-INTERSECT
      CUDD-ZDD-ISOP
      ;; CUDD-ZDD-ITE
      ;; CUDD-ZDD-ITH-VAR
      CUDD-ZDD-NEXT-PATH
      ;; CUDD-ZDD-PORT-FROM-BDD
      ;; CUDD-ZDD-PORT-TO-BDD
      CUDD-ZDD-PRINT-COVER
      CUDD-ZDD-PRINT-DEBUG
      CUDD-ZDD-PRINT-MINTERM
      CUDD-ZDD-PRINT-SUBTABLE
      ;; CUDD-ZDD-PRODUCT                  ; binate version of CUDD-ZDD-UNATE-PRODUCT
      ;; CUDD-ZDD-READ-NODE-COUNT
      CUDD-ZDD-REALIGN-DISABLE
      CUDD-ZDD-REALIGN-ENABLE
      CUDD-ZDD-REALIGNMENT-ENABLED
      CUDD-ZDD-REDUCE-HEAP
      CUDD-ZDD-SHUFFLE-HEAP
      ;; CUDD-ZDD-SUBSET-0
      ;; CUDD-ZDD-SUBSET-1
      CUDD-ZDD-SYMM-PROFILE
      ;; CUDD-ZDD-UNATE-PRODUCT            ; unate version of CUDD-ZDD-PRODUCT
      ;; CUDD-ZDD-UNION
      ;; CUDD-ZDD-VARS-FROM-BDD-VARS
      ;; CUDD-ZDD-WEAK-DIV                 ; binate version of CUDD-ZDD-DIVIDE
      ;; CUDD-ZDD-WEAK-DIV-F               ; obsoleted version of CUDD-ZDD-WEAK-DIV
      ))

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
   (cudd-zdd-subset-0 (manager-pointer *manager*) (node-pointer zdd) var)
   'zdd-node))
(defun zdd-subset-1 (zdd var)
  "Computes the subset of S that contains element VAR (integer)."
  (wrap-and-finalize
   (cudd-zdd-subset-1 (manager-pointer *manager*) (node-pointer zdd) var)
   'zdd-node))
(defun zdd-change (zdd var)
  "Computes the subset of S that contains element VAR (integer)."
  (wrap-and-finalize
   (cudd-zdd-change (manager-pointer *manager*) (node-pointer zdd) var)
   'zdd-node))

;; between 2 ZDDs

(defun zdd-union (f g)
  "Computes the union of F and G."
  (wrap-and-finalize
   (cudd-zdd-union (manager-pointer *manager*) (node-pointer f) (node-pointer g))
   'zdd-node))
(defun zdd-intersection (f g)
  "Computes the intersection of F and G."
  (wrap-and-finalize
   (cudd-zdd-intersect (manager-pointer *manager*) (node-pointer f) (node-pointer g))
   'zdd-node))
(defun zdd-difference (f g)
  "Computes the difference of F and G."
  (wrap-and-finalize
   (cudd-zdd-diff (manager-pointer *manager*) (node-pointer f) (node-pointer g))
   'zdd-node))

(defun zdd-divide-unate (f g)
  "Computes the weak division of F by G (assumes unate representation)."
  (wrap-and-finalize
   (cudd-zdd-divide (manager-pointer *manager*) (node-pointer f) (node-pointer g))
   'zdd-node))
(defun zdd-divide-binate (f g)
  "Computes the weak division of F by G (assumes binate representation)."
  (wrap-and-finalize
   (cudd-zdd-weak-div (manager-pointer *manager*) (node-pointer f) (node-pointer g))
   'zdd-node))
(defun zdd-product-unate (f g)
  "Computes the product of F by G (assumes unate representation)."
  (wrap-and-finalize
   (cudd-zdd-unate-product (manager-pointer *manager*) (node-pointer f) (node-pointer g))
   'zdd-node))
(defun zdd-product-binate (f g)
  "Computes the product of F by G (assumes binate representation)."
  (wrap-and-finalize
   (cudd-zdd-product (manager-pointer *manager*) (node-pointer f) (node-pointer g))
   'zdd-node))


