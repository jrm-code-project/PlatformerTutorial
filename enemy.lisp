;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass enemy (hitbox health entity)
  ((x-velocity :initform 0 :accessor get-x-velocity)))

(defmethod (setf get-x-velocity) :after (new-x-velocity (entity entity))
  (cond ((minusp new-x-velocity) (setf (flip? entity) nil))
        ((plusp new-x-velocity) (setf (flip? entity) t))
        (t nil)))

(defmethod enemy? ((entity enemy)) t)
