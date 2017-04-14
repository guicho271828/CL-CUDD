;;; functions on a raw cudd-node pointer
(in-package :cl-cudd.baseapi)

(deftype managed-node-operation ()
  `(function (foreign-pointer foreign-pointer) t))

(declaim (ftype (function (foreign-pointer) foreign-pointer) cudd-regular))
(defun cudd-regular (node)
  (let ((addr (pointer-address node)))
    (setf (ldb (byte 1 0) addr) 0)
    (make-pointer addr)))

(declaim (ftype managed-node-operation
                cudd-node-is-constant
                cudd-node-value
                cudd-node-then
                cudd-node-else
                cudd-node-ref-count))

(defun cudd-node-is-constant (manager node)
  (declare (ignore manager))
  (let ((regular (cudd-regular node)))
    (= (cudd-node-read-index regular) +cudd-maxindex+)))

(defun cudd-node-value (manager node)
  "Return the value of a leaf node.

Warning: Undefined behaviour if DD is not a leaf node"
  (declare (ignore manager))
  (foreign-slot-value
   (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
   '(:union dd-node/type) 'value))

(defun cudd-node-then (manager node)
  "Return the then-child of an inner node.

Warning: Undefined behaviour if DD is a leaf node"
  (declare (ignore manager))
  (foreign-slot-value
   (foreign-slot-value
    (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
    '(:union dd-node/type) 'kids)
   '(:struct dd-children) 'T))

(defun cudd-node-else (manager node)
  "Return the else-child of an inner node.

Warning: Undefined behaviour if DD is a leaf node"
  (declare (ignore manager))
  (foreign-slot-value
   (foreign-slot-value
    (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
    '(:union dd-node/type) 'kids)
   '(:struct dd-children) 'E))

(defun (setf cudd-node-value) (new-value manager node)
  "Return the value of a leaf node.

Warning: Undefined behaviour if DD is not a leaf node"
  (declare (ignore manager))
  (setf (foreign-slot-value
         (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
         '(:union dd-node/type) 'value)
        new-value))

(defun (setf cudd-node-then) (new-value manager node)
  "Return the then-child of an inner node.

Warning: Undefined behaviour if DD is a leaf node"
  (declare (ignore manager))
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

(defun (setf cudd-node-else) (new-value manager node)
  "Return the else-child of an inner node.

Warning: Undefined behaviour if DD is a leaf node"
  (declare (ignore manager))
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

(defun cudd-node-ref-count (manager node)
  "Return the reference count of the node."
  (declare (ignore manager))
  (foreign-slot-value (cudd-regular node) '(:struct dd-node) 'ref))



