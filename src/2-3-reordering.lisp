(in-package :cudd)

(deftype bdd-reordering-method ()
  `(member
    ,@(foreign-enum-keyword-list 'cudd-reordering-type)
    ;; :CUDD-REORDER-SAME
    ;; :CUDD-REORDER-NONE
    ;; :CUDD-REORDER-RANDOM
    ;; :CUDD-REORDER-RANDOM-PIVOT
    ;; :CUDD-REORDER-SIFT
    ;; :CUDD-REORDER-SIFT-CONVERGE
    ;; :CUDD-REORDER-SYMM-SIFT
    ;; :CUDD-REORDER-SYMM-SIFT-CONV
    ;; :CUDD-REORDER-WINDOW-2
    ;; :CUDD-REORDER-WINDOW-3
    ;; :CUDD-REORDER-WINDOW-4
    ;; :CUDD-REORDER-WINDOW-2-CONV
    ;; :CUDD-REORDER-WINDOW-3-CONV
    ;; :CUDD-REORDER-WINDOW-4-CONV
    ;; :CUDD-REORDER-GROUP-SIFT
    ;; :CUDD-REORDER-GROUP-SIFT-CONV
    ;; :CUDD-REORDER-ANNEALING
    ;; :CUDD-REORDER-GENETIC
    ;; :CUDD-REORDER-LINEAR          ; not documented
    ;; :CUDD-REORDER-LINEAR-CONVERGE ; not documented
    ;; :CUDD-REORDER-LAZY-SIFT       ; not documented
    ;; :CUDD-REORDER-EXACT
    ))

(defun enable-reordering (method)
  "Enables automatic dynamic reordering of BDDs and ADDs.

  Parameter method is used to determine the method used for
  reordering. If CUDD_REORDER_SAME is passed, the method is unchanged.

  @see Cudd_AutodynDisable Cudd_ReorderingStatus Cudd_AutodynEnableZdd "
  (declare (bdd-reordering-method method))
  (cudd-autodyn-enable %mp% method))

(define-simple-managed-function disable-reordering cudd-autodyn-disable
  "Disables automatic dynamic reordering of BDDs and ADDs.

  @see Cudd_AutodynEnable Cudd_ReorderingStatus Cudd_AutodynDisableZdd ")

(deftype zdd-reordering-method ()
  '(member :CUDD-REORDER-SAME
    :CUDD-REORDER-NONE
    :CUDD-REORDER-RANDOM
    :CUDD-REORDER-RANDOM-PIVOT
    :CUDD-REORDER-SIFT
    :CUDD-REORDER-SIFT-CONVERGE
    :CUDD-REORDER-SYMM-SIFT
    :CUDD-REORDER-SYMM-SIFT-CONV))

(defun zdd-enable-reordering (method)
  "Enables automatic dynamic reordering of ZDDs.

  Parameter method is used to determine the method used for
  reordering. If CUDD_REORDER_SAME is passed, the method is unchanged.

  @see Cudd_AutodynDisableZdd Cudd_ReorderingStatusZdd Cudd_AutodynEnable "
  (declare (zdd-reordering-method method))
  (cudd-autodyn-enable-zdd %mp% method))

(define-simple-managed-function zdd-disable-reordering cudd-autodyn-disable-zdd
  "Disables automatic dynamic reordering of ZDDs.

  @see Cudd_AutodynEnableZdd Cudd_ReorderingStatusZdd Cudd_AutodynDisable ")

(defun reordering-status ()
  "Reports the status of automatic dynamic reordering of BDDs and ADDs.
Return T if automatic reordering is enabled. NIL otherwise.
Secondary value returns the current reordering method.

  @see Cudd_AutodynDisableZdd Cudd_ReorderingStatusZdd Cudd_AutodynEnable"
  (with-foreign-object (method-ptr 'cudd-reordering-type)
    (values (= 1 (cudd-reordering-status %mp% method-ptr))
            (the bdd-reordering-method
                 (mem-ref method-ptr 'cudd-reordering-type)))))

(defun zdd-reordering-status ()
  "Reports the status of automatic dynamic reordering of ZDDs.
Return T if automatic reordering is enabled. NIL otherwise.
Secondary value returns the current reordering method.

  @see Cudd_AutodynEnableZdd Cudd_ReorderingStatusZdd Cudd_AutodynDisableZdd"
  (with-foreign-object (method-ptr 'cudd-reordering-type)
    (values (= 1 (cudd-reordering-status %mp% method-ptr))
            (the zdd-reordering-method
                 (mem-ref method-ptr 'cudd-reordering-type)))))
