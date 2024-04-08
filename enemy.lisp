;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass enemy (hitbox entity)
  ((x-velocity :initform 0 :accessor get-x-velocity)))

(defmethod (setf get-x-velocity) :after (new-x-velocity (entity entity))
  (cond ((minusp new-x-velocity) (setf (flip? entity) t))
        ((plusp new-x-velocity) (setf (flip? entity) nil))
        (t nil)))

(defmethod enemy? ((entity enemy)) t)
