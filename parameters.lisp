;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun game-scale () 3.0)
(defun scalef (quantity) (* (game-scale) quantity))
(defun scale  (quantity) (floor (scalef quantity)))

(defun-scaled game-width  420)
(defun-scaled game-height 400)
