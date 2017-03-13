
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
      CUDD-ZDD-READ-NODE-COUNT
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

(def-cudd-call zdd-subset-0 ((:zdd cudd-zdd-subset-0) (s :node) var)
  :zdd "Computes the subset of S that does not contain element VAR (integer).")
(def-cudd-call zdd-subset-1 ((:zdd cudd-zdd-subset-1) (s :node) var)
  :zdd "Computes the subset of S that contains element VAR (integer).")
(def-cudd-call zdd-change ((:zdd cudd-zdd-change) (s :node) var)
  :zdd "Computes the subset of S that contains element VAR (integer).")

;; between 2 ZDDs

(def-cudd-call zdd-union ((:zdd cudd-zdd-union) (f :node) (g :node))
  :zdd "Computes the union of F and G.")
(def-cudd-call zdd-intersection ((:zdd cudd-zdd-intersect) (f :node) (g :node))
  :zdd "Computes the intersection of F and G.")
(def-cudd-call zdd-difference ((:zdd cudd-zdd-diff) (f :node) (g :node))
  :zdd "Computes the difference of F and G.")

(def-cudd-call zdd-divide-unate ((:zdd cudd-zdd-divide) (f :node) (g :node))
  :zdd "Computes the weak division of F by G (assumes unate representation).")
(def-cudd-call zdd-divide-binate ((:zdd cudd-zdd-weak-div) (f :node) (g :node))
  :zdd "Computes the weak division of F by G (assumes binate representation).")
(def-cudd-call zdd-product-unate ((:zdd cudd-zdd-unate-product) (f :node) (g :node))
  :zdd "Computes the weak division of F by G (assumes binate representation).")
(def-cudd-call zdd-product-binate ((:zdd cudd-zdd-product) (f :node) (g :node))
  :zdd "Computes the weak division of F by G (assumes binate representation).")


