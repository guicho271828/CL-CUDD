
(in-package :cudd)


(defvar *unimplemented*
    '(
      CUDD-ADD-AGREEMENT
      CUDD-ADD-APPLY
      CUDD-ADD-BDD-INTERVAL
      CUDD-ADD-BDD-ITH-BIT
      CUDD-ADD-BDD-PATTERN
      CUDD-ADD-BDD-THRESHOLD
      CUDD-ADD-CMPL
      CUDD-ADD-COMPOSE
      CUDD-ADD-COMPUTE-CUBE
      CUDD-ADD-CONST
      CUDD-ADD-CONSTRAIN
      CUDD-ADD-CUBE
      CUDD-ADD-DIFF
      CUDD-ADD-DIVIDE
      CUDD-ADD-EVAL-CONST
      CUDD-ADD-FIND-MAX
      CUDD-ADD-FIND-MIN
      CUDD-ADD-HAMMING
      CUDD-ADD-HARWELL
      CUDD-ADD-HOOK
      CUDD-ADD-ITE
      CUDD-ADD-ITE-CONSTANT
      CUDD-ADD-ITH-BIT
      CUDD-ADD-ITH-VAR
      CUDD-ADD-LEQ
      CUDD-ADD-LOG
      CUDD-ADD-MAXIMUM
      CUDD-ADD-MINIMUM
      CUDD-ADD-MINUS
      CUDD-ADD-MONADIC-APPLY
      CUDD-ADD-NAND
      CUDD-ADD-NEGATE
      CUDD-ADD-NEW-VAR
      CUDD-ADD-NOR
      CUDD-ADD-OR
      CUDD-ADD-OR-ABSTRACT
      CUDD-ADD-OUTER-SUM
      CUDD-ADD-PERMUTE
      CUDD-ADD-PLUS
      CUDD-ADD-READ
      CUDD-ADD-RESIDUE
      CUDD-ADD-RESTRICT
      CUDD-ADD-ROUND-OFF
      CUDD-ADD-SET-NZ
      CUDD-ADD-THRESHOLD
      CUDD-ADD-TIMES
      CUDD-ADD-TIMES-PLUS
      CUDD-ADD-TRIANGLE
      CUDD-ADD-UNIV-ABSTRACT
      CUDD-ADD-WALSH
      CUDD-ADD-XEQY
      CUDD-ADD-XNOR
      CUDD-ADD-XOR
      CUDD-AGGREGATION-TYPE
      CUDD-APA-ADD
      CUDD-APA-COMPARE
      CUDD-APA-COPY
      CUDD-APA-COUNT-MINTERM
      CUDD-APA-INT-DIVISION
      CUDD-APA-POWER-OF-TWO
      CUDD-APA-PRINT-DECIMAL
      CUDD-APA-PRINT-DENSITY
      CUDD-APA-PRINT-HEX
      CUDD-APA-PRINT-MINTERM
      CUDD-APA-SHIFT-RIGHT
      CUDD-APA-SUBTRACT
      CUDD-AUTODYN-DISABLE
      CUDD-AUTODYN-ENABLE
      CUDD-AVERAGE-DISTANCE
      CUDD-BDD-ADJ-PERMUTE-X
      CUDD-BDD-AND
      CUDD-BDD-AND-ABSTRACT
      CUDD-BDD-AND-LIMIT
      CUDD-BDD-BIND-VAR
      CUDD-BDD-BOOLEAN-DIFF
      CUDD-BDD-CHAR-TO-VECT
      CUDD-BDD-CLIPPING-AND
      CUDD-BDD-CLOSEST-CUBE
      CUDD-BDD-COMPOSE
      CUDD-BDD-COMPUTE-CUBE
      CUDD-BDD-CONSTRAIN
      CUDD-BDD-CORRELATION
      CUDD-BDD-CUBE
      CUDD-BDD-INTERSECT
      CUDD-BDD-IS-NS-VAR
      CUDD-BDD-IS-PI-VAR
      CUDD-BDD-IS-PS-VAR
      CUDD-BDD-ISOP
      CUDD-BDD-ITE
      CUDD-BDD-ITE-CONSTANT
      CUDD-BDD-ITH-VAR
      CUDD-BDD-LEQ
      CUDD-BDD-LEQ-UNLESS
      CUDD-BDD-LICOMPACTION
      CUDD-BDD-MAKE-PRIME
      CUDD-BDD-MINIMIZE
      CUDD-BDD-NAND
      CUDD-BDD-NEW-VAR
      CUDD-BDD-NOR
      CUDD-BDD-NOT
      CUDD-BDD-NPAND
      CUDD-BDD-OR
      CUDD-BDD-PERMUTE
      CUDD-BDD-PICK-ONE-CUBE
      CUDD-BDD-PRINT-COVER
      CUDD-BDD-READ
      CUDD-BDD-RESTRICT
      CUDD-BDD-SET-NS-VAR
      CUDD-BDD-SET-PI-VAR
      CUDD-BDD-SET-PS-VAR
      CUDD-BDD-SQUEEZE
      CUDD-BDD-TO-ADD
      CUDD-BDD-TRANSFER
      CUDD-BDD-UNBIND-VAR
      CUDD-BDD-VAR-IS-BOUND
      CUDD-BDD-VAR-MAP
      CUDD-BDD-XNOR
      CUDD-BDD-XOR
      CUDD-CACHE-SLOTS
      CUDD-CHECK-KEYS
      CUDD-CHECK-ZERO-REF
      CUDD-CLASSIFY-SUPPORT
      CUDD-CLEAR-ERROR-CODE
      CUDD-COF-MINTERM
      CUDD-COFACTOR
      CUDD-COUNT-LEAVES
      CUDD-COUNT-MINTERM
      CUDD-COUNT-PATH
      CUDD-CPROJECTION
      CUDD-DAG-SIZE
      CUDD-DEAD-ARE-COUNTED
      CUDD-DEBUG-CHECK
      CUDD-DECREASING
      CUDD-DENSITY
      CUDD-DEREF
      CUDD-DUMP-BLIF
      CUDD-DUMP-BLIF-BODY
      CUDD-DUMP-DA-VINCI
      CUDD-DUMP-DDCAL
      CUDD-DUMP-DOT
      CUDD-DXYGTDXZ
      CUDD-DXYGTDYZ
      CUDD-EQUAL-SUP-NORM
      CUDD-EQUIV-DC
      CUDD-ERROR-TYPE
      CUDD-EVAL
      CUDD-FIND-ESSENTIAL
      CUDD-FIRST-CUBE
      CUDD-FIRST-NODE
      CUDD-FIRST-PRIME
      CUDD-FREE-TREE
      CUDD-FREE-ZDD-TREE
      CUDD-GEN-FREE
      CUDD-HOOK-TYPE
      CUDD-INCREASING
      CUDD-INDICES-TO-CUBE
      CUDD-INIT
      CUDD-IS-GEN-EMPTY
      CUDD-IS-IN-HOOK
      CUDD-IS-NON-CONSTANT
      CUDD-ITER-DEREF-BDD
      CUDD-LARGEST-CUBE
      CUDD-LAZY-GROUP-TYPE
      CUDD-MAKE-TREE-NODE
      CUDD-MANAGER
      CUDD-MIN-HAMMING-DIST
      CUDD-NEW-APA-NUMBER
      CUDD-NEXT-CUBE
      CUDD-NEXT-NODE
      CUDD-NEXT-PRIME
      CUDD-NODE
      CUDD-NODE-GET-ELSE
      CUDD-NODE-GET-THEN
      CUDD-NODE-GET-VALUE
      CUDD-NODE-IS-CONSTANT
      CUDD-NODE-READ-INDEX
      CUDD-OUT-OF-MEM
      CUDD-OVER-APPROX
      CUDD-PRIME
      CUDD-PRINT-DEBUG
      CUDD-PRINT-INFO
      CUDD-PRINT-LINEAR
      CUDD-PRINT-MINTERM
      CUDD-PRINT-VERSION
      CUDD-PRIORITY-SELECT
      CUDD-QUIT
      CUDD-RANDOM
      CUDD-READ-BACKGROUND
      CUDD-READ-CACHE-HITS
      CUDD-READ-CACHE-SLOTS
      CUDD-READ-DEAD
      CUDD-READ-EPSILON
      CUDD-READ-ERROR-CODE
      CUDD-READ-GROUPCHECK
      CUDD-READ-INV-PERM
      CUDD-READ-ITH-CLAUSE
      CUDD-READ-KEYS
      CUDD-READ-LINEAR
      CUDD-READ-LOGIC-ZERO
      CUDD-READ-LOOSE-UP-TO
      CUDD-READ-MAX-CACHE
      CUDD-READ-MAX-GROWTH
      CUDD-READ-MAX-LIVE
      CUDD-READ-MAX-MEMORY
      CUDD-READ-MIN-DEAD
      CUDD-READ-MIN-HIT
      CUDD-READ-NODE-COUNT
      CUDD-READ-NODES-FREED
      CUDD-READ-ONE
      CUDD-READ-PERM
      CUDD-READ-PERM-ZDD
      CUDD-READ-RECOMB
      CUDD-READ-REORDERINGS
      CUDD-READ-SIZE
      CUDD-READ-SLOTS
      CUDD-READ-STDERR
      CUDD-READ-STDOUT
      CUDD-READ-SWAP-STEPS
      CUDD-READ-TREE
      CUDD-READ-USED-SLOTS
      CUDD-READ-VARS
      CUDD-READ-ZDD-ONE
      CUDD-READ-ZDD-SIZE
      CUDD-READ-ZDD-TREE
      CUDD-READ-ZERO
      CUDD-RECURSIVE-DEREF
      CUDD-REDUCE-HEAP
      CUDD-REF
      CUDD-REGULAR
      CUDD-REMOVE-HOOK
      CUDD-REORDERING-TYPE
      CUDD-RESIDUE-DEFAULT
      CUDD-RESIDUE-MSB
      CUDD-RESIDUE-TC
      CUDD-SET-ARCVIOLATION
      CUDD-SET-BACKGROUND
      CUDD-SET-EPSILON
      CUDD-SET-GROUPCHECK
      CUDD-SET-LOOSE-UP-TO
      CUDD-SET-MAX-GROWTH
      CUDD-SET-MAX-LIVE
      CUDD-SET-MAX-MEMORY
      CUDD-SET-MIN-HIT
      CUDD-SET-RECOMB
      CUDD-SET-SIFT-MAX-VAR
      CUDD-SET-STDERR
      CUDD-SET-STDOUT
      CUDD-SET-TREE
      CUDD-SET-VAR-MAP
      CUDD-SET-ZDD-TREE
      CUDD-SHARING-SIZE
      CUDD-SHORTEST-LENGTH
      CUDD-SHORTEST-PATH
      CUDD-SHUFFLE-HEAP
      CUDD-SOLVE-EQN
      CUDD-SPLIT-SET
      CUDD-SRANDOM
      CUDD-SUBSET-COMPRESS
      CUDD-SUPPORT
      CUDD-SUPPORT-INDEX
      CUDD-SUPPORT-SIZE
      CUDD-SYMM-PROFILE
      CUDD-TLC-INFO-FREE
      CUDD-UNDER-APPROX
      CUDD-UNIQUE-SLOTS
      CUDD-VARIABLE-TYPE
      CUDD-VECTOR-SUPPORT
      CUDD-VERIFY-SOL
      CUDD-VERSION
      CUDD-XEQY
      CUDD-XGTY
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


(mapcar
 (lambda (x) (princ x) (terpri))
 (sort '(cudd-ref cudd-eval cudd-init cudd-node cudd-quit cudd-xeqy
         cudd-xgty cudd-deref cudd-prime cudd-add-or cudd-bdd-or cudd-random
         cudd-add-ite cudd-add-leq cudd-add-log cudd-add-nor cudd-add-xor
         cudd-apa-add cudd-bdd-and cudd-bdd-ite cudd-bdd-leq cudd-bdd-nor
         cudd-bdd-not cudd-bdd-xor cudd-density cudd-manager cudd-regular
         cudd-srandom cudd-support cudd-version cudd-zdd-ite cudd-add-cmpl
         cudd-add-cube cudd-add-diff cudd-add-hook cudd-add-nand cudd-add-plus
         cudd-add-read cudd-add-xeqy cudd-add-xnor cudd-apa-copy cudd-bdd-cube
         cudd-bdd-isop cudd-bdd-nand cudd-bdd-read cudd-bdd-xnor cudd-cofactor
         cudd-dag-size cudd-dump-dot cudd-dxygtdxz cudd-dxygtdyz cudd-equiv-dc
         cudd-gen-free cudd-read-one cudd-set-tree cudd-zdd-diff cudd-zdd-isop
         cudd-add-apply cudd-add-const cudd-add-minus cudd-add-times cudd-add-walsh
         cudd-bdd-npand cudd-dump-blif cudd-free-tree cudd-hook-type cudd-next-cube
         cudd-next-node cudd-read-dead cudd-read-keys cudd-read-perm cudd-read-size
         cudd-read-tree cudd-read-vars cudd-read-zero cudd-solve-eqn cudd-split-set
         cudd-zdd-count cudd-zdd-union cudd-add-divide cudd-add-negate
         cudd-add-set-nz cudd-bdd-to-add cudd-check-keys cudd-count-path
         cudd-decreasing cudd-dump-ddcal cudd-error-type cudd-first-cube
         cudd-first-node cudd-increasing cudd-is-in-hook cudd-next-prime
         cudd-out-of-mem cudd-print-info cudd-read-slots cudd-residue-tc
         cudd-set-recomb cudd-set-stderr cudd-set-stdout cudd-verify-sol
         cudd-zdd-change cudd-zdd-divide +cudd-max-index+ cudd-add-compose
         cudd-add-hamming cudd-add-harwell cudd-add-ith-bit cudd-add-ith-var
         cudd-add-maximum cudd-add-minimum cudd-add-new-var cudd-add-permute
         cudd-add-residue cudd-apa-compare cudd-bdd-compose cudd-bdd-ith-var
         cudd-bdd-new-var cudd-bdd-permute cudd-bdd-squeeze cudd-bdd-var-map
         cudd-cache-slots cudd-cof-minterm cudd-cprojection cudd-debug-check
         cudd-first-prime cudd-over-approx cudd-print-debug cudd-read-linear
         cudd-read-recomb cudd-read-stderr cudd-read-stdout cudd-reduce-heap
         cudd-remove-hook cudd-residue-msb cudd-set-epsilon cudd-set-min-hit
         cudd-set-var-map cudd-zdd-ith-var cudd-zdd-product cudd-add-find-max
         cudd-add-find-min cudd-add-restrict cudd-add-triangle cudd-apa-subtract
         cudd-bdd-bind-var cudd-bdd-minimize cudd-bdd-restrict cudd-bdd-transfer
         cudd-count-leaves cudd-is-gen-empty cudd-largest-cube cudd-print-linear
         cudd-read-epsilon cudd-read-min-hit cudd-read-zdd-one cudd-set-max-live
         cudd-set-zdd-tree cudd-sharing-size cudd-shuffle-heap cudd-support-size
         cudd-symm-profile cudd-under-approx cudd-unique-slots cudd-zdd-dag-size
         cudd-zdd-divide-f cudd-zdd-dump-dot cudd-zdd-subset-0 cudd-zdd-subset-1
         cudd-zdd-weak-div cudd-add-agreement cudd-add-constrain cudd-add-outer-sum
         cudd-add-round-off cudd-add-threshold cudd-apa-print-hex
         cudd-bdd-and-limit cudd-bdd-constrain cudd-bdd-intersect
         cudd-bdd-is-ns-var cudd-bdd-is-pi-var cudd-bdd-is-ps-var
         cudd-count-minterm cudd-dump-da-vinci cudd-free-zdd-tree
         cudd-node-get-else cudd-node-get-then cudd-print-minterm
         cudd-print-version cudd-read-inv-perm cudd-read-max-live
         cudd-read-min-dead cudd-read-perm-zdd cudd-read-zdd-size
         cudd-read-zdd-tree cudd-shortest-path cudd-support-index
         cudd-tlc-info-free cudd-variable-type cudd-zdd-intersect
         cudd-zdd-next-path cudd-add-eval-const cudd-add-times-plus
         cudd-autodyn-enable cudd-bdd-leq-unless cudd-bdd-make-prime
         cudd-bdd-set-ns-var cudd-bdd-set-pi-var cudd-bdd-set-ps-var
         cudd-bdd-unbind-var cudd-check-zero-ref cudd-dump-blif-body
         cudd-equal-sup-norm cudd-find-essential cudd-iter-deref-bdd
         cudd-make-tree-node cudd-new-apa-number cudd-node-get-value
         cudd-read-max-cache cudd-set-background cudd-set-groupcheck
         cudd-set-max-growth cudd-set-max-memory cudd-vector-support
         cudd-zdd-complement cudd-zdd-diff-const cudd-zdd-first-path
         cudd-zdd-weak-div-f cudd-add-bdd-ith-bit cudd-add-bdd-pattern
         cudd-add-or-abstract cudd-apa-shift-right cudd-autodyn-disable
         cudd-bdd-correlation cudd-bdd-print-cover cudd-indices-to-cube
         cudd-is-non-constant cudd-lazy-group-type cudd-node-read-index
         cudd-priority-select cudd-read-background cudd-read-cache-hits
         cudd-read-error-code cudd-read-groupcheck cudd-read-ith-clause
         cudd-read-logic-zero cudd-read-max-growth cudd-read-max-memory
         cudd-read-node-count cudd-read-swap-steps cudd-read-used-slots
         cudd-recursive-deref cudd-reordering-type cudd-residue-default
         cudd-set-loose-up-to cudd-shortest-length cudd-subset-compress
         cudd-zdd-port-to-bdd cudd-zdd-print-cover cudd-zdd-print-debug
         cudd-zdd-reduce-heap cudd-add-bdd-interval cudd-add-compute-cube
         cudd-add-ite-constant cudd-aggregation-type cudd-apa-int-division
         cudd-apa-power-of-two cudd-average-distance cudd-bdd-and-abstract
         cudd-bdd-boolean-diff cudd-bdd-char-to-vect cudd-bdd-clipping-and
         cudd-bdd-closest-cube cudd-bdd-compute-cube cudd-bdd-ite-constant
         cudd-bdd-licompaction cudd-bdd-var-is-bound cudd-classify-support
         cudd-clear-error-code cudd-dead-are-counted cudd-min-hamming-dist
         cudd-node-is-constant cudd-read-cache-slots cudd-read-loose-up-to
         cudd-read-nodes-freed cudd-read-reorderings cudd-set-arcviolation
         cudd-set-sift-max-var cudd-zdd-count-double cudd-zdd-shuffle-heap
         cudd-zdd-symm-profile cudd-add-bdd-threshold cudd-add-monadic-apply
         cudd-add-univ-abstract cudd-apa-count-minterm cudd-apa-print-decimal
         cudd-apa-print-density cudd-apa-print-minterm cudd-bdd-adj-permute-x
         cudd-bdd-pick-one-cube)
       #'string<))

