
# Table of contents:

+ Representing a binary function using BDD
+ Representing real functions using ADD and taking their symbolic sum
+ Representing a family of set using ZDD
+ The more complex example in simpath/ directory which implements Knuth's simpath algorithm in ZDD.
  See [README](simpath/README.md)

## Representing a binary function using BDD

Suppose you have a binary function denoted as `b = f(x0,x1,...xn)` which can be expressed in a DNF formula such as
`(or (and x0 (not x1) x2) ...)`.
To begin with, we construct the first disjunction, which is in this case `(and x0 (not x1) x2)`.
It can be implemented as follows:

```lisp
;; functional
(reduce #'node-and
        (list (make-var 'bdd-node :index 0)
              (node-complement (make-var 'bdd-node :index 1))
              (make-var 'bdd-node :index 2))
        :from-end t ;; more efficient when BDDs are build bottom-up
        :initial-value (one-node 'bdd-node))

;; imperative
(let ((f (one-node 'bdd-node)))
  (setf f (node-and f (make-var 'bdd-node :index 2)))
  (setf f (node-and f (node-complement (make-var 'bdd-node :index 1))))
  (setf f (node-and f (make-var 'bdd-node :index 0)))
  f)

;; with a threading macro

(ql:quickload :arrow-macros) (use-package :arrow-macros)

(-> (one-node 'bdd-node)
    (node-and (make-var 'bdd-node :index 2))
    (node-and (node-complement (make-var 'bdd-node :index 1)))
    (node-and (make-var 'bdd-node :index 0)))
```

To take the disjunction of conjunctions, there are similarly named `node-or` function and `zero-node` function.

```lisp
(-> (zero-node 'bdd-node)
    (node-or bdd1)
    (node-or bdd2)
    ...)
```

``(plot pathname dd)`` function (accepts bdd, add and zdd) generates a dot file
and a pdf, but requires graphviz.

## Representing real functions using ADD and taking their symbolic sum

In ADD, the terminal node can have a value instead of boolean (in BDD), and you
are able to perform various arithmetic operations in the closed form, i.e. for
two ADDs (functions) `f` and `g`, you can obtain `f+g` efficiently.

Imagine `f(x1,x2)` and `g(x1,x2)` takes the following values:

| x1 | x2 | f(x1,x2) | g(x1,x2) |
|:--:|:--:|:--------:|:--------:|
| 0  | 0  |  2       |  4       |
| 0  | 1  |  2       |  4       |
| 1  | 0  |  3       |  3       |
| 1  | 1  |  5       |  7       |

```lisp
;; f
(-> (-> (ADD-constant 2)
        (node-and (node-complement (make-var 'ADD-node :index 0)))
        (node-and (node-complement (make-var 'ADD-node :index 1))))
    (node-or
        (-> (ADD-constant 2)
            (node-and (node-complement (make-var 'ADD-node :index 0)))
            (node-and (make-var 'ADD-node :index 1))))
    (node-or
        (-> (ADD-constant 3)
            (node-and (make-var 'ADD-node :index 0))
            (node-and (node-complement (make-var 'ADD-node :index 1)))))
    (node-or
        (-> (ADD-constant 5)
            (node-and (make-var 'ADD-node :index 0))
            (node-and (make-var 'ADD-node :index 1)))))

;; g 
(-> ...) ;; omitted

(ADD-apply +plus+ f g) ;; -> an ADD representing (f+g)(x1,x2)
```

Other possible arguments to `ADD-apply` are

* `+XNOR+` (originally Cudd_addXnor) -- XNOR of two 0-1 ADDs.
* `+XOR+` (originally Cudd_addXor) -- XOR of two 0-1 ADDs.
* `+NOR+` (originally Cudd_addNor) -- NOR of two 0-1 ADDs.
* `+NAND+` (originally Cudd_addNand) -- NAND of two 0-1 ADDs.
* `+OR+` (originally Cudd_addOr) -- Disjunction of two 0-1 ADDs.
* `+AGREEMENT+` (originally Cudd_addAgreement) -- f op g,  where f op g is f if f==g; background if f!=g.
* `+DIFF+` (originally Cudd_addDiff) -- f op g , where f op g is plusinfinity if f=g; min(f,g) if f!=g.
* `+ONE-ZERO-MAXIMUM+` (originally Cudd_addOneZeroMaximum) -- 1 if f > g and 0 otherwise.
* `+MAXIMUM+` (originally Cudd_addMaximum) -- Integer and floating point maximum.
* `+MINIMUM+` (originally Cudd_addMinimum) -- Integer and floating point minimum.
* `+MINUS+` (originally Cudd_addMinus) -- Integer and floating point substraction.
* `+DIVIDE+` (originally Cudd_addDivide) -- Integer and floating point division.
* `+SET-NZ+` (originally Cudd_addSetNZ) -- This operator sets f to the value of g wherever g != 0.
* `+THRESHOLD+` (originally Cudd_addThreshold) -- Threshold operator for Apply (f if f >=g; 0 if f<g)
* `+TIMES+` (originally Cudd_addTimes) -- Integer and floating point multiplication.
* `+PLUS+` (originally Cudd_addPlus) -- Integer and floating point addition

Another  way  to  construct an  ADD  is  to  convert  the corresponding  BDD  by
`bdd->add` function. (This case applies only to the case where 0-1 ADDs suffice,
i.e. ADDs with only 0 or 1 terminal nodes).

## Representing a family of set using ZDD

Recently, ZDD is famous especially among Japanese CS researchers due to this
hyped youtube video https://www.youtube.com/watch?v=Q4gTV4r0zRs made by people
funded by
[JST ERATO (Exploratory Research for Advanced Technology)](http://www.jst.go.jp/erato/en/index.html). No,
it is actually quite powerful and is a better option than BDD when *most of the
paths leads to the zero-node*, and is particularly useful for representing a
family of sets.

One way to construct a ZDD is to call either of conversion functions
`bdd->zdd-simple` and `bdd->zdd-cover` to an existing BDD.

* `bdd->zdd-simple` directly maps one BDD variable to one ZDD variable.
  This is called a *unate* algebra representation, which is suitable for representing a binary function and a set of subsets.
* `bdd->zdd-cover` maps one BDD variable to two ZDD variables representing true and false.
  This is called *binate* algebra representation, which is suitable for representing cube covers because it can efficiently encode the missing variables. See section 3.3 of [Mishchenko 14] section 3.11 of CUDD manual.

Another option is to build a ZDD directly, starting from {{}} and "flipping" the
true/false by `(zdd-change zdd variable)` operator.  Consider we are encoding a
family of sets F={{1,2}, {1,3}, {3}}. First, A={{1,2}} is built as follows:

```lisp
(defvar *a* (zdd-set-of-emptyset))
(setf *a* (zdd-change *a* 1))
(setf *a* (zdd-change *a* 2))

;; alternatively, using threading macro:
(defvar *a*
   (-> (zdd-set-of-emptyset)
       (zdd-change 1)
       (zdd-change 2)))
```

Then take their union.

```lisp
(defvar *b* ..) ; construct *b* and *c* similarly.
(defvar *c* ..)

(defvar *f* (reduce #'zdd-union (list *a* *b* *c*) :initial-value (zdd-emptyset)))

;; alternatively, 
(defvar *f* 
   (-> (zdd-emptyset)
       (zdd-union *a*)
       (zdd-union *b*)
       (zdd-union *c*)))

;; or,
(defvar *f* (zdd-emptyset))
(setf *f* (zdd-union *f* *a*))
(setf *f* (zdd-union *f* *b*))
(setf *f* (zdd-union *f* *c*))

;; of course, this is equivalent and is more efficient:
(defvar *f* (zdd-union *a* *b*))
(setf *f* (zdd-union *f* *c*))
```
