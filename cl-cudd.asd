;;;; Autogenerated ASD file for system "CL-CUDD"
;;;; In order to regenerate it, run update-asdf
;;;; from shell (see https://github.com/phoe-krk/asd-generator)
;;;; For those who do not have update-asdf,
;;;; run `ros install asd-generator` (if you have roswell installed)
;;;; There are also an interface available from lisp:
;;;; (asd-generator:regen &key im-sure)
(defsystem cl-cudd
 :serial t
 :author "Christian von Essen <christian@mvonessen.de>"
 :license "BSD Style (see LICENSE)"
 :defsystem-depends-on (:cffi-grovel)
 :depends-on (:cffi
              :alexandria
              :trivial-garbage
              :cl-cudd.build)
 :serial t
 :components ((:file "src/0-package")
              (:file "src/1-0-0-library")
              (:file "src/1-0-1-conditions")
              (:file "src/1-0-2-translators")
              (:file "src/1-1-0-swig-macros")
              (:file "src/1-1-1-grovel")
              (:file "src/1-1-2-fun")
              (:file "src/1-2-0-base-common")
              (:file "src/1-2-1-base-add")
              (:file "src/1-2-1-base-bdd")
              (:file "src/1-3-dddmp")
              (:file "src/2-0-base")
              (:file "src/2-1-add-bdd-bridge")
              (:file "src/2-1-add")
              (:file "src/2-1-common")
              (:file "src/2-1-generic-complex")
              (:file "src/2-1-generic-simple")
              (:file "src/2-1-generic-swap")
              (:file "src/2-1-system"))
 :description "A two-layered binding to the CUDD binary decision diagram library.

See README.md for more details."
 :in-order-to ((test-op (test-op cl-cudd.test))))
