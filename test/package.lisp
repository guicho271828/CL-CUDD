
(in-package :cl-user)

(defpackage cl-cudd.test
  (:use :cl :cl-cudd :cl-cudd.baseapi :fiveam :iterate :trivia :arrow-macros)
  (:shadow :next :<>))

(in-package cl-cudd.test)

(def-suite :cl-cudd)
(in-suite :cl-cudd)

(defun models (kind)
  (directory (merge-pathnames "*.tests" (asdf:system-relative-pathname :cl-cudd (format nil "test/~a/" kind)))))

;; we need a better api for creating a bdd

(defun dump (path name f)
  (cl-cudd.baseapi:dump-dot
   (manager-pointer *manager*)
   (cudd-regular (node-pointer f))
   (namestring (make-pathname :name name :type "dot" :defaults path))))


(defun dump-zdd (path name f)
  (cl-cudd.baseapi:zdd-dump-dot
   (manager-pointer *manager*)
   (cudd-regular (node-pointer f))
   (namestring (make-pathname :name name :type "dot" :defaults path))))


(defun parse-bdd (path)
  (fresh-line)
  (with-manager ()
    (let ((f
           (reduce #'node-or
                   (iter (for line in-file path using #'read-line)
                         (collect
                          (reduce #'node-and
                                  (iter (for c in-vector line)
                                        (for index from 0)
                                        (collect
                                         (ecase c
                                           (#\0 (node-complement
                                                 (make-var 'bdd-node :index index)))
                                           (#\1 (make-var 'bdd-node :index index)))))
                                  :initial-value (one-node 'bdd-node))))
                   :initial-value (zero-node 'bdd-node))))
      (pass "constructed DD")
      (finishes (print f))
      (finishes (print (dag-size f)))
      (match path
        ((pathname name)
         (finishes
           (dump path (format nil "~a-BDD" name) f))
         ;; since BDDs may contain complemented edges, it is slightly hard to understand.
         ;; Usually converting it into ADDs will improve the output
         (finishes
           (dump path (format nil "~a-BDD-as-ADD" name) (bdd->add f)))
         (finishes
           (dump-zdd path (format nil "~a-BDD-as-ZDD-simple" name) (bdd->zdd-simple f))))))))

(test bdd
  (dolist (m (models "gates"))
    (parse-bdd m))
  (uiop:run-program (format nil "make -C ~a" (asdf:system-relative-pathname :cl-cudd "test/gates/"))
                    :ignore-error-status t
                    :output t
                    :error-output t))

(defun parse-add (path)
  (fresh-line)
  (with-manager ()
    (let ((f
           (reduce #'node-or
                   (iter (for line in-file path using #'read-line)
                         (collect
                             (reduce #'node-and
                                     (iter (for c in-vector line)
                                           (for index from 0)
                                           (collect
                                               (ecase c
                                                 (#\0 (node-complement
                                                       (make-var 'add-node :index index)))
                                                 (#\1 (make-var 'add-node :index index)))))
                                     :initial-value (one-node 'add-node))))
                   :initial-value (zero-node 'add-node))))
      (pass "constructed DD")
      (finishes
        (print f))
      (finishes
        (print (dag-size f)))
      (match path
        ((pathname name)
         (finishes
           (dump path (format nil "~a-ADD" name) f)))))))

(test add
  (with-manager ()
    (finishes (print (zero-node 'add-node)))
    (finishes (print (add-constant 0.0d0)))
    (finishes (print (node-and (make-var 'add-node :index 1)
                               (one-node 'add-node))))
    (finishes (print (node-or (make-var 'add-node :index 1)
                              (one-node 'add-node))))
    (finishes (print (node-and (make-var 'add-node :index 1)
                               (zero-node 'add-node))))
    (finishes (print (node-or (make-var 'add-node :index 1)
                              (zero-node 'add-node))))
    (finishes (print (info))))
  (dolist (m (models "gates"))
    (format t "~%testing model ~a" m)
    (parse-add m))
  (uiop:run-program (format nil "make -C ~a" (asdf:system-relative-pathname :cl-cudd "test/gates/"))
                    :ignore-error-status t
                    :output t
                    :error-output t))


(defun parse-zdd-sets-of-subsets (path)
  (fresh-line)
  (let* ((all "abc"))
    (with-manager ()
      (let* ((f
              (reduce #'zdd-union
                      (iter (for line in-file path using #'read-line)
                            (collect
                                (iter (for c in-vector line)
                                      (with f = (zdd-set-of-emptyset)) ; {{}} --- does not contain anything
                                      ;; (break "~@{~a~}" c all (position c all))
                                      (setf f (zdd-change f (position c all))) ; add c to {{}} --> {{c}}
                                      (finally (return f)))))
                      :initial-value (zdd-emptyset))))
        (pass "constructed DD")
        (finishes
          (print f))
        (finishes
          (print (dag-size f)))
        (match path
          ((pathname name)
           (finishes
             (dump-zdd path (format nil "~a-ZDD" name) f))))))))

(test zdd
  (dolist (m (models "sets-of-subsets"))
    (format t "~%testing model ~a" m)
    (parse-zdd-sets-of-subsets m))
  (uiop:run-program (format nil "make -C ~a" (asdf:system-relative-pathname :cl-cudd "test/sets-of-subsets/"))
                    :ignore-error-status t
                    :output t
                    :error-output t))


(test reordering
  ;; swap

  ;; permutation

  ;; dynamic reordering
  )

