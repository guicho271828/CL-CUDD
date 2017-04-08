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

(defcallback before-gc-hook :int ((dd (:pointer (:struct dd-manager))) (mode :string) (data :void))
  (let ((*manager* (make-manager :pointer dd)))
    (dolist (fn *before-gc-hook*)
      (funcall fn mode data))))
(defcallback after-gc-hook :int ((dd (:pointer (:struct dd-manager))) (mode :string) (data :void))
  (let ((*manager* (make-manager :pointer dd)))
    (dolist (fn *after-gc-hook*)
      (funcall fn mode data))))
(defcallback before-reordering-hook :int ((dd (:pointer (:struct dd-manager))) (mode :string) (data :void))
  (let ((*manager* (make-manager :pointer dd)))
    (dolist (fn *before-reordering-hook*)
      (funcall fn mode data))))
(defcallback after-reordering-hook :int ((dd (:pointer (:struct dd-manager))) (mode :string) (data :void))
  (let ((*manager* (make-manager :pointer dd)))
    (dolist (fn *after-reordering-hook*)
      (funcall fn mode data))))

