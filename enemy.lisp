;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass enemy (hitbox entity)
  ())

(defmethod entity-step! (game level (entity enemy) (state (eql :idle)) dticks)
  nil)

(defmethod enemy? ((entity enemy)) t)
