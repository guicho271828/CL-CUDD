(in-package :cudd)

;; add-hook
;; remove-hook
;; isinhook
;; stdprereordhook
;; stdpostreordhook
;;
;; in cl-cudd we completely ignore the provided hook facility.

(defvar *before-gc-hook* nil
  "A list of function designators that is called before GC.
The function takes one argument, a mode string (\"DD\", \"BDD\" or \"ZDD\").

This list is managed by a lisp process, independently from Cudd_AddHook function.
WITH-MANAGER macro registers 4 global hooks which in turn call the functions in this special variable.")
(defvar *after-gc-hook*  nil
  "A list of function designators that is called before GC.
The function takes one argument, a mode string (\"DD\", \"BDD\" or \"ZDD\").

This list is managed by a lisp process, independently from Cudd_AddHook function.
WITH-MANAGER macro registers 4 global hooks which in turn call the functions in this special variable.")
(defvar *before-reordering-hook* nil
  "A list of function designators that is called before GC.
The function takes one argument, a mode string (\"DD\", \"BDD\" or \"ZDD\").

This list is managed by a lisp process, independently from Cudd_AddHook function.
WITH-MANAGER macro registers 4 global hooks which in turn call the functions in this special variable.")
(defvar *after-reordering-hook*  nil
  "A list of function designators that is called before GC.
The function takes one argument, a mode string (\"DD\", \"BDD\" or \"ZDD\").

This list is managed by a lisp process, independently from Cudd_AddHook function.
WITH-MANAGER macro registers 4 global hooks which in turn call the functions in this special variable.")

(defcallback before-gc-hook :int ((dd :pointer) (mode :string) (data :pointer))
  (declare (ignore data))
  (let ((*manager* (make-manager :pointer dd)))
    (handler-case
        (dolist (fn *before-gc-hook* 1)
          (funcall fn mode))
      (error ()
        0))))

(defcallback after-gc-hook :int ((dd :pointer) (mode :string) (data :pointer))
  (declare (ignore data))
  (let ((*manager* (make-manager :pointer dd)))
    (handler-case
        (dolist (fn *after-gc-hook*)
          (funcall fn mode))
      (error ()
        0))))

(defcallback before-reordering-hook :int ((dd :pointer) (mode :string) (data :pointer))
  (declare (ignore data))
  (let ((*manager* (make-manager :pointer dd)))
    (handler-case
        (dolist (fn *before-reordering-hook*)
          (funcall fn mode))
      (error ()
        0))))

(defcallback after-reordering-hook :int ((dd :pointer) (mode :string) (data :pointer))
  (declare (ignore data))
  (let ((*manager* (make-manager :pointer dd)))
    (handler-case
        (dolist (fn *after-reordering-hook*)
          (funcall fn mode))
      (error ()
        0))))

(setf *manager* (manager-init))
