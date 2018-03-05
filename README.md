Common Lisp binding to CUDD [![Build Status](https://travis-ci.org/guicho271828/CL-CUDD.svg?branch=master)](https://travis-ci.org/guicho271828/CL-CUDD)
===========================

This is a fork of original CL-CUDD using the modern common lisp convension.

* **Supported implementations**: SBCL, CCL and ECL.
* **Requirements**: make, curl
* **Developmental State**: After some refurbishment, now it loads reliably and all tests pass.
* **TODOs**:
    * ~~Automatic variable reordering~~
    * ~~Variable grouping API~~
    * ~~GC hook and control API~~
    * Higher-order layer for set manipulation
    * benchmarking
* **Related work**: 
  trivialib.bdd is another common lisp library for BDDs, which is entirely written in lisp. CUDD is more on the state-of-the-art side.

What is BDDs and CUDD?
-------------

BDDs (Binary Decision Diagrams) are awesome datastructures that can compactly represent exponentially large number of datasets, as well as allowing the direct computation over the compressed representation, i.e., you can take the sum/product/union/intersection of the datasets without decompressing the data!

[CUDD](http://vlsi.colorado.edu/~fabio/CUDD/)
is a famous C implementation of BDDs and its relatives: 
Multi-Terminal Binary Decision Diagrams (MTBDDs, also known as Algebraic DD / ADDs) and
Zero-suppressed Decision Diagrams.

References:

+ [Wikipedia](http://en.wikipedia.org/wiki/Binary_decision_diagram) contains lots of information.
+ [Binary Decision Diagrams](http://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=1675141) by Akers et al
+ [Symbolic Boolean manipulation with ordered binary-decision diagrams](http://www.cse.chalmers.se/edu/course/TDA956/Papers/acmcs92.pdf) by RE Bryant (this sorta made BDDs truely practical I guess)
+ ADDs : [Multi-Terminal Binary Decision Diagrams: An Efficient Data Structure for Matrix Representation](http://repository.cmu.edu/cgi/viewcontent.cgi?article=1456&context=compsci) by Clarke et al
+ ZDDs (survey) : [An Introduction to Zero-Suppressed Binary Decision Diagrams](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.500.6132&rep=rep1&type=pdf) by Alan Mishchenko

[trivialib.bdd](http://quickdocs.org/trivialib.bdd/) is another common lisp library for BDDs, which is entirely written in lisp. CUDD is more on the state-of-the-art side.

Building/Loading the system
---------------------------
The system is asdf-loadable.
This version of CL-CUDD automatically fetches CUDD v3.0.0 from http://vlsi.colorado.edu/~fabio/CUDD/ via curl.
The archive is expanded in the ASDF system directory and builds its dynamic library, which is then loaded by CL-CUDD.

To test the system, evaluate `(asdf:test-system :cl-cudd.test)`.
It also writes the visualizations of the decision diagrams to the system directory in DOT format.
If you have Graphviz installed, the test script also tries to convert the results into pdfs.

The binding(s)
--------------
The binding consists of two layers:
The lower layer has `cl-cudd.baseapi` package.
This layer is a very thin wrapper around the C library,
passes raw pointers around and requires that you take care of reference counting.

Above this layer there is a package named `cl-cudd` (with a nickname `cudd`).
It wraps the pointers from the lower layer, takes care of reference counting for you, and also
adds documentation from the CUDD manual.

DD Construction Examples
------------------------

See [EXAMPLE.md](EXAMPLE.md).

System structure
----------------

### Low-level

This is loosely based on the SWIG-extracted information and is using CFFI-Grovel
to actually map C symbols to lisp symbols.  If you want to use this layer, then
it would be best to have a look at the CUDD manual.

You can use the low-level system just as you would use the C API of
CUDD. This also means that you have to do all the reference counting
yourself, with one exception: The reference count of the return value
each CUDD function that returns a node is increased if it is not
`null`. If it is `null`, a signal of type `cudd-null-pointer-error` is
raised.

### High-level

The high level API automatically wraps the CUDD nodes in an instance
of class `node`. ADD nodes are wrapped in an instance of `add-node`
and BDD nodes are wrapped in an instance of type `bdd-node`.

This enables runtime type checking (so that you don't stick ADD nodes
into BDD functions or vice-versa) and also automatic reference counting.

Almost all CUDD functions need to refer to a CUDD manager. In the
high-level API this manager is contained in special variable
`*manager*`. You can bind a manager using the macro `with-manager`.
You can also create a manager by
`(make-instance 'manager :pointer (cudd-init 0 0 256 262144 0))`.

All functions of package `CL-CUDD` are documented using the original or
slightly modified documentation of CUDD.

History
-------

The initial version was automatically generated using [SWIG](http://www.swig.org) by Utz-Uwe Haus.
The second version was adapted to the needs by Christian von Essen <christian.vonEssen@imag.fr>.
Later, @Neronus made a git repository on Github and @rpgoldman made a few bugfixes.
Finally @guicho271828 (Masataro Asai) has modernized the repository according to the recent practice in common lisp:  unit tests, Travis-CI support, better documentation and additional support for ZDDs.


Known problems
--------------

Using the GC to do reference counting automatically has its own share of problems:

1. References may be freed very late.

   Nodes will be dereferenced only if your CL implementation thinks
   that it's time for it. This is usually when itself is running out
   of memory. Because you are usually only holding on to the top of
   a diagram, you are not using as much memory in CL as you are using
   in CUDD. Hence the GC might come pretty late while CUDD is happily
   accumulating memory.

   The solution to that is to try to call the garbage collector
   manually every so often using for example
   TRIVIAL-GARBAGE:GC

Solved problems
---------------

~~References may be freed too early~~

The old text below is wrong. CUDD's reference counting GC does not work this
way.  According to CUDD's manual, its GC happens when:

1. A call to cuddUniqueInter , to cuddUniqueInterZdd , to cuddUnique-
   Const, or to a function that may eventually cause a call to them.
2. A call to Cudd RecursiveDeref , to Cudd RecursiveDerefZdd , or to a
   function that may eventually cause a call to them.

Thus the GC does not occur at arbitrary code path, as assumed below.

     The following two examples demonstrate the problem.

        (defun foo (dd)
          (let ((ptr (node-pointer dd)))
            ;; please don't GC me here
            (cudd-do-something ptr)))

    In this example the GC might decide to run where there is the
    comment.
    In that case, provided that nothing outside of the function call
    holds on to `dd`, the reference count of `ptr` might be decreased,
    go down to zero and the node vanishes before `cudd-do-something` is
    called.
