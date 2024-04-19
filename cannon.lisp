;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled cannon-attackbox-width (* 5 (base-tile-size)))
(defun-scaled cannon-attackbox-height 15)
(defun-scaled cannon-attackbox-x-offset (- (+ (base-cannon-attackbox-width) 15)))
(defun-scaled cannon-attackbox-y-offset 5)

(defclass cannon (attackbox entity)
  ((cannon-fired? :initform nil :accessor cannon-fired?) 
   (cannonball :initarg :cannonball :reader cannonball))
  (:default-initargs
   :attackbox-width (cannon-attackbox-width)
   :attackbox-height (cannon-attackbox-height)
   :attackbox-x-offset (cannon-attackbox-x-offset)
   :attackbox-y-offset (cannon-attackbox-y-offset)))

(defclass dormant ()
  ((until :initarg :until :reader get-until)))

(defclass hold ()
  ((until :initarg :until :reader get-until)))

(defmethod (setf get-state) :after ((state dormant) (cannon cannon))
  (start-animation! cannon :idle))

(defmethod (setf get-state) :after ((state (eql :fire)) (cannon cannon))
  (start-animation! cannon :fire))

(defmethod (setf get-state) :after ((state hold) (cannon cannon))
  (start-animation! cannon :idle))

(defmethod (setf get-state) :after ((state (eql :idle)) (cannon cannon))
  (setf (cannon-fired? cannon) nil)
  (start-animation! cannon :idle))

(defmethod entity-step! (game level (cannon cannon) (state dormant) dticks)
  (when (> (sdl2:get-ticks) (get-until state))
    (setf (get-state cannon) :idle)))

(defmethod entity-step! (game level (cannon cannon) (state (eql :fire)) dticks)
  (when (and (= (get-frame (get-animation cannon)) 4)
             (not (cannon-fired? cannon)))
    (start-cannonball! (cannonball cannon))
    (setf (cannon-fired? cannon) t))
  (when (animation-finished? (get-animation cannon))
    (setf (get-state cannon) (make-instance 'dormant :until (+ (sdl2:get-ticks) 2500)))))

(defmethod entity-step! (game level (cannon cannon) (state hold) dticks)
  (when (> (sdl2:get-ticks) (get-until state))
    (setf (get-state cannon) :fire)))

(defmethod entity-step! (game level (cannon cannon) (state (eql :idle)) dticks)
  (when (can-attack? cannon (player level))
    (setf (get-state cannon) (make-instance 'hold :until (+ (sdl2:get-ticks) 500)))))
