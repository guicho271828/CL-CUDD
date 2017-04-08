(in-package :cudd)

;; add-hook
;; remove-hook
;; isinhook
;; stdprereordhook
;; stdpostreordhook
;;
;; in cl-cudd we completely ignore the provided hook facility.

(defvar *before-gc-hook* nil)
(defvar *after-gc-hook*  nil)
(defvar *before-reordering-hook* nil)
(defvar *after-reordering-hook*  nil)

(defcallback before-gc-hook :int ((dd :pointer) (mode :string) (data :pointer))
  (let ((*manager* (make-manager :pointer dd)))
    (handler-case
        (dolist (fn *before-gc-hook* 1)
          (funcall fn mode data))
      (error ()
        0))))
(defcallback after-gc-hook :int ((dd :pointer) (mode :string) (data :pointer))
  (let ((*manager* (make-manager :pointer dd)))
    (handler-case
        (dolist (fn *after-gc-hook*)
          (funcall fn mode data))
      (error ()
        0))))
(defcallback before-reordering-hook :int ((dd :pointer) (mode :string) (data :pointer))
  (let ((*manager* (make-manager :pointer dd)))
    (handler-case
        (dolist (fn *before-reordering-hook*)
          (funcall fn mode data))
      (error ()
        0))))
(defcallback after-reordering-hook :int ((dd :pointer) (mode :string) (data :pointer))
  (let ((*manager* (make-manager :pointer dd)))
    (handler-case
        (dolist (fn *after-reordering-hook*)
          (funcall fn mode data))
      (error ()
        0))))


