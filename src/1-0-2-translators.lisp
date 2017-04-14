
(in-package :cl-cudd.baseapi)

(defctype node :pointer "A DD-node returned by CUDD")
(defctype manager :pointer "A manager of CUDD")

(define-foreign-type node-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser node))

(define-foreign-type manager-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser manager))



;; Any functions in 1-1-2-fun.lisp should follow a certain arugment naming convention:
;; the arugument for a manager should be DD, otherwise DEFCFUN signals an error
;; because its expansion contains a reference to DD.

(defmethod expand-to-foreign (value (type node-type))
  value)
(defmethod expand-from-foreign (value (type node-type))
  (let ((gvalue (gensym "value")))
    `(let ((,gvalue ,value))
       (if (null-pointer-p ,gvalue)
           (error 'cudd-null-pointer-error
                  :code (foreign-slot-value DD '(:struct dd-manager) 'error-code))
           ,gvalue))))

(defmethod expand-to-foreign (value (type manager-type))
  value)
(defmethod expand-from-foreign (value (type manager-type))
  (let ((gvalue (gensym "value")))
    `(let ((,gvalue ,value))
       (if (null-pointer-p ,gvalue)
           (error 'cudd-null-manager-error)
           ,gvalue))))
