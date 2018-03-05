#|
  This file is a part of simpath project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage simpath
  (:shadow :<>)
  (:use :cl :trivia :cl-cudd :alexandria :iterate :arrow-macros))
(in-package :simpath)

;; blah blah blah.


;; input: explicit graph representation

(deftype edge ()
  `(cons integer integer))

(defstruct input
  (s 0 :type non-negative-integer)
  (t 0 :type non-negative-integer)
  (edges #() :type (array edge)))

(defun grid-node (size x y)
  ;; breadth-first from top-left to bottom-right
  ;;   |  0  1  2  3  4
  ;;---+---------------
  ;; 0 |  0  1  3  6 10 ** **
  ;; 1 |  2  4  7 11 15 **
  ;; 2 |  5  8 12 16 19
  ;; 3 |  9 13 17 20 22
  ;; 4 | 14 18 21 23 24
  ;;     **
  (let ((n (+ x y)))
    (if (< n size)
        (+ (* 1/2 n (1+ n))
           y)
        (+ (* 1/2 n (1+ n))
           y
           ;; adjust the missing cells (**)
           (let ((m (- n size -1)))
             (* -1 m m))))))

(let ((size 2))
  (print
   (iter (with a = (make-array (list size size)))
         (for x below size)
         (iter (for y below size)
               (setf (aref a y x) (grid-node size x y)))
         (finally (return a)))))

(defun grid (size)
  (make-input
   :s 0
   :t (1- (* size size))
   :edges
   (coerce (iter outer
                 (for y below size)
                 (iter (for x below size)
                       (in outer
                           (unless (= x (1- size))
                             (collect (cons (grid-node size x y) (grid-node size (1+ x) y))
                               result-type vector))
                           (unless (= y (1- size))
                             (collect (cons (grid-node size x y) (grid-node size x (1+ y)))
                               result-type vector)))))
           '(array edge))))

(defun nodes (input)
  (match input
    ((input edges)
     (let ((h (make-hash-table)))
       (iter (for (from . to) in-vector edges)
             (setf (gethash from h) from
                   (gethash to h) to))
       (hash-table-keys h)))))

;; (defun simpath (input)
;;   (match input
;;     ((input s t edges)
;;      (with-manager (:initial-num-vars-z (length edges))
;;        (iter (for e in-vector edges with-index i)
;;              (for var = (make-var 'zdd-node :index i))
;;              ;; the details of branch pruning is not specified...

(defmacro with-renaming (bindings &body body)
  (let ((tmps (make-gensym-list (length bindings))))
    (iter (for (old new) in bindings)
          (for tmp in tmps)
          (setf body (subst tmp old body :test #'equalp)))
    (iter (for (old new) in bindings)
          (for tmp in tmps)
          (setf body (subst new tmp body :test #'equalp))))
  `(progn ,@body))

(defun test (a b)
  (* a b)
  #+nil
  (flet ((* (a b) (+ a b)))
    (* a b))
  #+nil
  (macrolet ((* (a b) `(+ ,a ,b)))
    (* a b))
  #+nil
  (symbol-macrolet ((* +))
    (* a b))
  (with-renaming ((* +)
                  (+ *))
    (* a b)
    (+ a b)))


(defun dump (path name f)
  (cl-cudd.baseapi:dump-dot
   (manager-pointer *manager*)
   (cl-cudd.baseapi:cudd-regular (node-pointer f))
   (namestring (make-pathname :name name :type "dot" :defaults path))))
(defun dump-zdd (path name f)
  (cl-cudd.baseapi:zdd-dump-dot
   (manager-pointer *manager*)
   (cl-cudd.baseapi:cudd-regular (node-pointer f))
   (namestring (make-pathname :name name :type "dot" :defaults path))))

(defun draw (f)
  (dump-zdd "." "dump" f)
  (uiop:run-program (format nil "dot dump.dot -Tpdf -o dump.pdf")))

(defmacro break+ (&rest args)
  (let* ((last-form (lastcar args))
         (last last-form)
         (butlast (butlast args)))
    (once-only (last)
      `(progn
         (break "~@{~a~%~<;;~@; -> ~4i~:@_~a~;~:>~2%~}"
                ,@(iter (for arg in butlast)
                        (collect `',arg)
                        (collect `(list ,arg)))
                ',last-form (list ,last))
         ,last))))

(defun progress (&optional (char #\.))
  (write-char char)
  ;; (finish-output)
  )

(defconstant +new+ 0)
(defconstant +frontier+ 1)
(defconstant +closed+ 2)
(defun mate-zdd (input)
  (ematch input
    ((input :s start :t terminal :edges edges)
     (let* ((%edges (length edges))
            (%nodes (max 1 (length (nodes input))))
            (%mates (/ (* %nodes (1+ %nodes)) 2)))
       (labels ((m1 (v1 v2)
                  ;; 0
                  ;; 1  2
                  ;; 3  4  5
                  ;; 6  7 [8] 9   (v1 = 2, v2 = 3)
                  ;;  ↖
                  ;;    3*4 /2
                  (let ((v1 (min v1 v2))
                        (v2 (max v1 v2)))
                    (+ (* v2 (1+ v2) 1/2) v1)))
                ;; 
                ;; several candidates for variable ordering
                ;; best/worst ratio is over x40
                ;; 
                ;; (m (v1 v2) (+ %edges (m1 v1 v2)))
                ;; (m (v1 v2) (+ %edges (- %mates 1 (m1 v1 v2)))) ;worst
                ;; (e (h)     h)
                ;; (e (h)     (- %edges 1 h)) ;worst
                ;; 
                ;; (m (v1 v2) (m1 v1 v2))
                (m (v1 v2) (- %mates 1 (m1 v1 v2))) ; best
                ;; (e (h)     (+ %mates h))
                (e (h)     (+ %mates (- %edges 1 h))) ; best
                (v (var)   (make-var 'zdd-node :index var)))
         (with-manager (:initial-num-vars-z (+ %mates %edges)
                                            :max-memory 1000000 ; 1Mbyte
                                            :stack-allocated t)
           (cl-cudd:info)
           ;; (set-zdd-variable-group :mtr-default :from 0 :size %mates)
           ;; (set-zdd-variable-group :mtr-default :from %mates :size %edges)
           ;; (print (dump-zdd-variable-group-hierarchy))
           (zdd-enable-reordering :cudd-reorder-sift)
           (push (lambda (mode) (format t "~&gc happening for ~a!" mode))
                 cl-cudd:*before-gc-hook*)
           (push (lambda (mode) (format t "~&reordering happening for ~a!" mode))
                 cl-cudd:*before-reordering-hook*)
           (let ((f (zdd-set-of-emptyset))
                 ;; frontier := visited nodes / closed nodes
                 (states (make-array %nodes
                                     :element-type 'fixnum
                                     :initial-element +new+)))
             (with-renaming ((+ zdd-union)
                             (* zdd-product-unate)
                             (/ zdd-divide-unate)
                             (! zdd-change)
                             (on  zdd-subset-1)
                             (off zdd-subset-0)
                             (% zdd-remainder-unate)
                             (_+ +)
                             (_* *)
                             (s zdd-singleton))
               ;; initially there is only one element: {{m00,m11,m22,m33}}
               (print :step0)
               (iter (for i below %nodes)
                     (setf f (* f (s (m i i)))))
               (print :step1)
               (flet ((prune (p)
                        (when (and (/= p start)
                                   (/= p terminal))
                          (setf (aref states p) +closed+)
                          (iter (for st in-vector states with-index k)
                                (when (and (= st +frontier+))
                                  (progress #\p)
                                  ;; (when (/= p k))
                                  ;; ^^^^ doesnt happen, already closed
                                  (setf f (off f (m p k)))))
                          (setf f (+ (on f (m p p))
                                     (off f (m p p)))))))
                 (iter (for (i . j) in-vector edges with-index h)
                       (for p previous i)
                       (unless (first-iteration-p)
                         (when (/= i p)  ; i.e., i is a new node
                           (prune p)))
                       (format t "~%~ath/~ath edge" h %edges)
                       (setf (aref states i) +frontier+)
                       (setf (aref states j) +frontier+)
                       (iter (for st in-vector states with-index k)
                             (unless (= st +frontier+) (next-iteration))
                             (iter (for st in-vector states with-index l)
                                   (unless (= st +frontier+) (next-iteration))
                                   (progress)
                                   (setf f (+ f
                                              (-> f
                                                (on (m i k))
                                                (on (m j l))
                                                (* (s (e h)))
                                                (* (s (m k l))))))))
                       (finally
                        (prune i))))
               (print :step2)
               (setf f (on f (m start terminal))))
             (print :step3)
             (cl-cudd:info)
             (zdd-count-minterm f))))))))

(assert (= (print (time (mate-zdd (grid 2)))) 2))
(assert (= (print (time (mate-zdd (grid 3)))) 12))
(assert (= (print (time (mate-zdd (grid 4)))) 184))
(assert (= (print (time (mate-zdd (grid 5)))) 8512))
