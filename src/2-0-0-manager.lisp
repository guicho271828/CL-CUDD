;;; base class definitions and macros for defining APIs
(in-package :cudd)

;;; Manager

(defvar *manager* nil "The current manager.

Every function in this package works with this manager.")

(defstruct manager
  "A boxed CUDD manager class"
  (pointer (error "MANAGER needs to wrap a pointer")
           :type cffi:foreign-pointer))

(define-symbol-macro %mp% (manager-pointer *manager*))

(defmethod cffi:translate-to-foreign (pointer (manager manager))
  (manager-pointer manager))

(defmacro with-manager ((&key 
                         (initial-num-vars 0)
                         (initial-num-vars-z 0)
                         (initial-num-slots 256)
                         (cache-size 262144)
                         (max-memory 0))
                        &body body)
  "Bind a freshly generated manager to *MANAGER*.
This macro is not so useful when multiple managers are in place.
Also, all data on the diagram are lost when it exits the scope of WITH-MANAGER.

* INITIAL-NUM-VARS and INITIAL-NUM-VARS-Z: are just initial values.
  The number of variables in CUDD manager is automatically increased when it exceeds this value.

* INITIAL-NUM-SLOTS : initial size of the unique tables
* CACHE-SIZE : initial size of the cache
* MAX-MEMORY : target maximum memory occupation. If zero, CUDD decides suitable
  values for the maximum size of the cache and for the limit for fast
  unique table growth based on the available memory.
"
  `(let ((*manager*
          (make-manager
           :pointer
           (cudd-init ,initial-num-vars
                      ,initial-num-vars-z
                      ,initial-num-slots
                      ,cache-size ,max-memory))))
     (cudd-add-hook %mp% (callback before-gc-hook) :cudd-pre-gc-hook)
     (cudd-add-hook %mp% (callback after-gc-hook) :cudd-post-gc-hook)
     (cudd-add-hook %mp% (callback before-gc-hook) :cudd-pre-reordering-hook)
     (cudd-add-hook %mp% (callback after-gc-hook) :cudd-post-reordering-hook)
     (unwind-protect
          (progn ,@body)
       (cudd-quit %mp%)
       (setf %mp% (cffi:null-pointer)))))

(defun info ()
  (uiop:with-temporary-file (:stream s :pathname path)
    (print-info %mp% path)
    (uiop:slurp-stream-string s)))

