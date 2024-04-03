;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun game-scale () 2.0)
(defun scalef (quantity) (* (game-scale) quantity))
(defun scale  (quantity) (floor (scalef quantity)))

(defun width-in-tiles () 26)
(defun height-in-tiles () 14)
(defun-scaled tile-size 32)
(defun game-width () (* (tile-size) (width-in-tiles)))
(defun game-height () (* (tile-size) (height-in-tiles)))
