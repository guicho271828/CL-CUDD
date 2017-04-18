
(in-package :cudd)

(defun plot (pathname node)
  (let ((dot (namestring (make-pathname :type "dot" :defaults pathname)))
        (pdf (namestring (make-pathname :type "pdf" :defaults pathname))))
    (etypecase node
      ((or add-node bdd-node)
       (cl-cudd.baseapi:dump-dot
        (manager-pointer *manager*)
        (cudd-regular (node-pointer node))
        dot))
      (zdd-node
       (cl-cudd.baseapi:zdd-dump-dot
        (manager-pointer *manager*)
        (cudd-regular (node-pointer node))
        dot)))
    (uiop:run-program `("dot" ,dot "-Tpdf" "-o" ,pdf))))
