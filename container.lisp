;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass container (hitbox entity)
  ((contents :initarg :contents :reader get-contents)))

(defmethod hit! ((container container))
  (setf (get-state container) :hit)
  (when (get-contents container)
    (setf (get-state (get-contents container)) :idle)))

(defmethod entity-step! (game level (container container) (state (eql :idle)) dticks)
  nil)

(defmethod entity-step! (game level (container container) (state (eql :hit)) dticks)
  (when (animation-finished? (get-animation container))
    (setf (get-state container) nil)))

(defclass barrel (container)
  ()
  (:default-initargs
   :height 49
   :width 45))

(defmethod (setf get-state) :after ((state (eql :hit)) (barrel barrel))
  (start-animation! barrel :destroy-barrel))

(defclass box (container)
  ()
  (:default-initargs
   :height 35
   :width 45))

(defmethod (setf get-state) :after ((state (eql :hit)) (box box))
  (start-animation! box :destroy-box))
