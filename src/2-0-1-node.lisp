;;; base class definitions and macros for defining APIs
(in-package :cudd)

;;; Wrapped CUDD node
(defstruct node
  "A boxed CUDD node class. Top class of all CUDD nodes."
  (pointer (error "NODE needs to wrap a pointer")
           :type cffi:foreign-pointer))

(defmethod cffi:translate-to-foreign (pointer (node node))
  (node-pointer node))

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

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type (type-of object) :identity nil)
    (format stream "~A " (cudd-node-read-index (node-pointer object)))
    (if (node-constant-p object)
        (format stream "LEAF (VALUE ~A)" (node-value object))
        (format stream "INNER 0x~x INDEX ~d"
                (pointer-address (node-pointer object))
                (node-index object)))
    (format stream " REF ~d"
            (cudd-node-get-ref-count %mp% (node-pointer object)))))

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
  (cudd-node-get-value %mp%
                       (node-pointer node)))


(defstruct (bdd-node (:include node))
  "Node of a binary decision diagram (BDD)")

(defstruct (add-node (:include node))
  "Node of an algebraic decision diagram (ADD)")

(defstruct (zdd-node (:include node))
  "Node of an zero-suppressed decision diagram (ZDD)")

(deftype node-type ()
  `(member bdd-node add-node zdd-node))
