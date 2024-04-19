;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass cannonball (attackbox hitbox entity)
  ((speed :initarg :speed :reader get-speed)
   (start-tick :initform (sdl2:get-ticks) :accessor get-start-tick))
  (:default-initargs
   :width 26
   :height 26
   :attackbox-width 20
   :attackbox-height 20
   :attackbox-x-offset -10
   :attackbox-y-offset 0))

(defmethod get-x ((cannonball cannonball))
  (+ (call-next-method)
     (* (get-speed cannonball)
        (- (sdl2:get-ticks) (get-start-tick cannonball)))))

(defun start-cannonball! (cannonball)
  (setf (get-start-tick cannonball) (sdl2:get-ticks)
        (get-state cannonball) :idle))

(defmethod entity-step! (game level (cannonball cannonball) (state (eql :idle)) dticks)
  (cond ((let ((left (get-left cannonball))
               (right (get-right cannonball))
               (top (get-top cannonball))
               (bottom (get-bottom cannonball)))
           (or (< left 0)
               (>= right (level-width level))
               (< top 0)
               (>= bottom (game-height))
               (against-left-wall? level cannonball)
               (against-right-wall? level cannonball)
               ))
         (setf (get-state cannonball) nil))
        ((can-attack? cannonball (player level))
         (hit! (player level))
         (setf (get-state cannonball) nil))
        (t nil)))
