;;; base class definitions and macros for defining APIs
(in-package :cudd)

(defun required ()
  (error "Required slot"))
;;; Wrapped CUDD node
(defstruct node
  "A boxed CUDD node class. Top class of all CUDD nodes."
  (pointer (required) :type cffi:foreign-pointer))

(defmacro with-pointers (pointers &body body)
  "Create a binding to pointers using a let-like specification.
Takes care that the nodes stay alive during the body.

Example:
 (with-pointers ((f-ptr f)
                 (g-ptr g))
   (cudd-add-apply +or+ f-ptr g-ptr))

This is implemented by increasing the reference count of
every node in the body and decreasing it after the body is run"
  (let ((manager (gensym "MANAGER")))
    `(let* ((,manager %mp%))
       (declare (ignorable ,manager))
       (progn
         ;; Reference all pointers
         ;; Effectively, we call node-pointer twice on every wrapper.
         ;; The reason is that we have to keep holding on to
         ;; the wrapper until after all referencing is done, i.e.,
         ;; until after the following code block ran.
         ;; Therefore, we first call `(cudd-ref (node-pointer wrapper))`
         ;; for each wrapper. Then later we create the binding for
         ;; the pointers.
         ,@(loop
              :for binding :in pointers
              :unless (and (listp binding) (= (length binding) 2))
              :do (error "Binding ~A is mal-formed" binding)
              ;; increase refcount
              :collect `(cudd-ref (node-pointer ,(cadr binding))))
         (let
             ;; Create bindings
             ,(loop
                 :for binding :in pointers
                 :collect `(,(car binding) (node-pointer ,(cadr binding))))
           (unwind-protect
                (progn
                  ,@body)
             ,@(loop
                  :for binding :in pointers
                  :collect `(cudd-recursive-deref ,manager ,(car binding)))))))))

(declaim (inline wrap-and-finalize))
(defun wrap-and-finalize (pointer type &optional (finalize t))
  "Wrap the given pointer in a node-node of type TYPE.
Set the finalizer to call cudd-recursive-deref."
  (declare (foreign-pointer pointer)
           ((member bdd-node add-node zdd-node) type))
  (let ((node (ecase type
                (bdd-node (make-bdd-node :pointer pointer))
                (add-node (make-add-node :pointer pointer))
                (zdd-node (make-zdd-node :pointer pointer)))))
    (when finalize
      (trivial-garbage:finalize
       node
       (let ((manager *manager*))
         ;; NOTE: ^^^ This holds the reference from the __finalizer function__ to
         ;; the manager (along with avoiding problems related to dynamic binding).
         ;; Without it, the finalizer may be called after the manager is finalized
         ;; (i.e. cudd-quit is called), invalidating the pointer to the node.
         ;; It is insufficient to reference a manager from a node, since the order
         ;; to call finalizers is unspecified. If a manager and a node is freed in
         ;; the same gc, it could be possible that cudd-quit is called
         ;; first. Manager object should be referenced until the node finalizer
         ;; is called.
         (lambda ()
           (let ((mp (manager-pointer manager)))
             (when (zerop (cudd-node-ref-count mp pointer))
               (error "Tried to decrease reference count of node that already has refcount zero"))
             (ecase type
               (bdd-node (cudd-recursive-deref mp pointer))
               (add-node (cudd-recursive-deref mp pointer))
               (zdd-node (cudd-recursive-deref-zdd mp pointer))))))))
    node))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type (type-of object) :identity nil)
    (format stream "INDEX ~A " (cudd-node-read-index (node-pointer object)))
    (if (node-constant-p object)
        (format stream "LEAF (VALUE ~A)" (node-value object))
        (format stream "INNER 0x~x" (pointer-address (node-pointer object))))
    (format stream " REF ~d"
            (cudd-node-ref-count %mp% (node-pointer object)))))

(defun node-index (node)
  (cudd-node-read-index (node-pointer node)))

(defun node-equal (a b)
  "Return true iff A and B are the same graph.

Because CUDD caches all diagrams, this is true if and
only if their pointers are the same."
  (check-type a node)
  (check-type b node)
  (cffi:pointer-eq (node-pointer a) (node-pointer b)))

(defun node-constant-p (node)
  "return t if the node is constant, nil otherwise"
  (cudd-node-is-constant %mp%
                         (node-pointer node)))

(defun node-value (node)
  "Return the node value of a constant node"
  ;; Make sure that we only try to read the value of a constant node
  (assert (node-constant-p node))
  (cudd-node-value %mp% (node-pointer node)))

(defstruct (bdd-node (:include node))
  "Node of a binary decision diagram (BDD)")

(defstruct (add-node (:include node))
  "Node of an algebraic decision diagram (ADD)")

(defstruct (zdd-node (:include node))
  "Node of an zero-suppressed decision diagram (ZDD)")

(deftype node-type ()
  `(member bdd-node add-node zdd-node))
