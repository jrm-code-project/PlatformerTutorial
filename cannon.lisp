;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass cannon (entity)
  ())

(defclass dormant ()
  ((until :initarg :until :reader get-until)))

(defmethod (setf get-state) :after ((state dormant) (cannon cannon))
  (start-animation! cannon :idle))

(defmethod (setf get-state) :after ((state (eql :fire)) (cannon cannon))
  (start-animation! cannon :fire))

(defmethod (setf get-state) :after ((state (eql :idle)) (cannon cannon))
  (start-animation! cannon :idle))

(defmethod entity-step! (game level (cannon cannon) (state dormant) dticks)
  (when (> (sdl2:get-ticks) (get-until state))
    (setf (get-state cannon) :idle)))

(defmethod entity-step! (game level (cannon cannon) (state (eql :fire)) dticks)
  (when (animation-finished? (get-animation cannon))
    (setf (get-state cannon) (make-instance 'dormant :until (+ (sdl2:get-ticks) 3000)))))

(defmethod entity-step! (game level (cannon cannon) (state (eql :idle)) dticks)
  (when (and (same-level? cannon (player level))
             (within-five-tiles? cannon (player level))
             (or (and (not (flip? cannon))
                      (left-to-right? (player level) cannon))
                 (and (flip? cannon)
                      (left-to-right? cannon (player level)))))
    (setf (get-state cannon) :fire)))
