;;; functions on a raw cudd-node pointer
(in-package :cl-cudd.baseapi)

(deftype node-operation ()
  `(function (foreign-pointer) t))
(deftype managed-node-operation ()
  `(function (foreign-pointer foreign-pointer) t))

(declaim (ftype (function (foreign-pointer) foreign-pointer) cudd-regular))
(defun cudd-regular (node)
  (let ((addr (pointer-address node)))
    (setf (ldb (byte 1 0) addr) 0)
    (make-pointer addr)))

(declaim (ftype node-operation
                cudd-node-is-constant
                cudd-node-value
                cudd-node-then
                cudd-node-else
                cudd-node-ref-count))

(defun cudd-node-is-constant (node)
  (let ((regular (cudd-regular node)))
    (= (cudd-node-read-index regular) +cudd-maxindex+)))

(defun cudd-node-value (node)
  "Return the value of a leaf node.

Warning: Undefined behaviour if DD is not a leaf node"
  (foreign-slot-value
   (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
   '(:union dd-node/type) 'value))

(defun cudd-node-then (node)
  "Return the then-child of an inner node.

Warning: Undefined behaviour if DD is a leaf node"
  (foreign-slot-value
   (foreign-slot-value
    (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
    '(:union dd-node/type) 'kids)
   '(:struct dd-children) 'T))

(defun cudd-node-else (node)
  "Return the else-child of an inner node.

Warning: Undefined behaviour if DD is a leaf node"
  (foreign-slot-value
   (foreign-slot-value
    (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
    '(:union dd-node/type) 'kids)
   '(:struct dd-children) 'E))

(defun (setf cudd-node-value) (new-value node)
  "Return the value of a leaf node.

Warning: Undefined behaviour if DD is not a leaf node"
  (setf (foreign-slot-value
         (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
         '(:union dd-node/type) 'value)
        new-value))

(defun (setf cudd-node-then) (new-value node)
  "Return the then-child of an inner node.

Warning: Undefined behaviour if DD is a leaf node"
  (cudd-deref (foreign-slot-value
               (foreign-slot-value
                (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
                '(:union dd-node/type) 'kids)
               '(:struct dd-children) 'T))
  (setf (foreign-slot-value
         (foreign-slot-value
          (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
          '(:union dd-node/type) 'kids)
         '(:struct dd-children) 'T)
        new-value))

(defun (setf cudd-node-else) (new-value node)
  "Return the else-child of an inner node.

Warning: Undefined behaviour if DD is a leaf node"
  (cudd-deref (foreign-slot-value
               (foreign-slot-value
                (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
                '(:union dd-node/type) 'kids)
               '(:struct dd-children) 'E))
  (setf (foreign-slot-value
         (foreign-slot-value
          (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
          '(:union dd-node/type) 'kids)
         '(:struct dd-children) 'E)
        new-value))

(defun cudd-node-ref-count (node)
  "Return the reference count of the node."
  (foreign-slot-value (cudd-regular node) '(:struct dd-node) 'ref))

(defun cudd-bdd-variables (manager)
  "Return the number of BDD variables"
  (foreign-slot-value manager '(:struct dd-manager) 'size))
(defun cudd-zdd-variables (manager)
  "Return the number of ZDD variables"
  (foreign-slot-value manager '(:struct dd-manager) 'size-z))
(defun cudd-bdd-max-variables (manager)
  "Return the maximum number of BDD variables"
  (foreign-slot-value manager '(:struct dd-manager) 'max-size))
(defun cudd-zdd-max-variables (manager)
  "Return the maximum number of ZDD variables"
  (foreign-slot-value manager '(:struct dd-manager) 'max-size-z))
