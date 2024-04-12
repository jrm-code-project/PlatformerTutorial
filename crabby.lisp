;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled crabby-width 26)
(defun-scaled crabby-height 20)

(defclass crabby (enemy)
  ()
  (:default-initargs
   :width (crabby-width)
   :height (crabby-height)))
