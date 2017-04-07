(in-package :cudd)

(defun enable-reordering (method)
  "Enables automatic dynamic reordering of BDDs and ADDs.

  Parameter method is used to determine the method used for
  reordering. If CUDD_REORDER_SAME is passed, the method is unchanged.

  @see Cudd_AutodynDisable Cudd_ReorderingStatus Cudd_AutodynEnableZdd "
  (cudd-autodyn-enable %mp% method))

(define-simple-managed-function disable-reordering cudd-autodyn-disable
  "Disables automatic dynamic reordering of BDDs and ADDs.

  @see Cudd_AutodynEnable Cudd_ReorderingStatus Cudd_AutodynDisableZdd ")

(defun zdd-enable-reordering (method)
  "Enables automatic dynamic reordering of ZDDs.

  Parameter method is used to determine the method used for
  reordering. If CUDD_REORDER_SAME is passed, the method is unchanged.

  @see Cudd_AutodynDisableZdd Cudd_ReorderingStatusZdd Cudd_AutodynEnable "
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
            (mem-ref method-ptr 'cudd-reordering-type))))

(defun zdd-reordering-status ()
  "Reports the status of automatic dynamic reordering of ZDDs.
Return T if automatic reordering is enabled. NIL otherwise.
Secondary value returns the current reordering method.

  @see Cudd_AutodynEnableZdd Cudd_ReorderingStatusZdd Cudd_AutodynDisableZdd"
  (with-foreign-object (method-ptr 'cudd-reordering-type)
    (values (= 1 (cudd-reordering-status %mp% method-ptr))
            (mem-ref method-ptr 'cudd-reordering-type))))
