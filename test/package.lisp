
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

(defun basename (pathname)
  (make-pathname :type nil :defaults pathname))
(defun append-name (pathname suffix)
  (make-pathname :name (concatenate 'string (pathname-name pathname) suffix) :defaults pathname))

(defun parse-bdd (path &optional zdd-binate?)
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
      (finishes (print (multiple-value-list (reordering-status))))
      (finishes
       (plot (append-name path "-BDD") f))
      ;; since BDDs may contain complemented edges, it is slightly hard to understand.
      ;; Usually converting it into ADDs will improve the output
      (finishes
       (plot (append-name path "-BDD-as-ADD") (bdd->add f)))
      (finishes
       (if zdd-binate?
           (plot (append-name path "-BDD-as-ZDD-cover") (bdd->zdd-cover f))
           (plot (append-name path "-BDD-as-ZDD-simple") (bdd->zdd-simple f)))))))

(test bdd
  (dolist (m (append (models "gates") (models "modest")))
    (parse-bdd m)
    (parse-bdd m t)))

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
      (finishes (print (multiple-value-list (reordering-status))))
      (finishes
       (plot (append-name path "-ADD") f)))))

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
    (finishes (print (info)))
    (finishes (print (node-count)))
    (finishes (print (peak-node-count)))
    (finishes (print (peak-live-node-count)))
    ;; This number always includes the two constants 1 and 0.
    (is (= 2 (zdd-node-count))))
  (dolist (m (append (models "gates") (models "modest")))
    (format t "~%testing model ~a" m)
    (parse-add m)))


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
        (finishes (print (multiple-value-list (zdd-reordering-status))))
        (finishes
         (plot (append-name path "-ZDD") f))))))

(test zdd
  (dolist (m (models "sets-of-subsets"))
    (format t "~%testing model ~a" m)
    (parse-zdd-sets-of-subsets m)))


(test reordering
  ;; swap

  ;; permutation

  ;; dynamic reordering
  )

