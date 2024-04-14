;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun game-scale () 2.0)
(defun scalef (quantity) (* (game-scale) quantity))
(defun scale  (quantity) (floor (scalef quantity)))

(defun game-width () 640)
(defun game-height () 480)
