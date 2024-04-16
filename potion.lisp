;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled blue-potion-attackbox-width 7)
(defun-scaled blue-potion-attackbox-height 14)
(defun-scaled blue-potion-attackbox-x-offset -3)
(defun-scaled blue-potion-attackbox-y-offset 0)
(defun-scaled red-potion-attackbox-width 9)
(defun-scaled red-potion-attackbox-height 14)
(defun-scaled red-potion-attackbox-x-offset -4)
(defun-scaled red-potion-attackbox-y-offset 0)

(defclass potion (attackbox entity)
  ())

;;; Make potions bob up and down.
(defmethod get-y ((object potion))
  (+ (call-next-method)
     (* (scale 5) (sin (* 2 (/ (sdl2:get-ticks) 1000) 3.14)))))

(defclass blue-potion (potion)
  ()
  (:default-initargs
   :attackbox-width (blue-potion-attackbox-width)
   :attackbox-height (blue-potion-attackbox-height)
   :attackbox-x-offset (blue-potion-attackbox-x-offset)
   :attackbox-y-offset (blue-potion-attackbox-y-offset)))

(defmethod entity-step! (game level (potion blue-potion) entity-state dticks)
  (when (can-attack? potion (player level))
    (format t "~&Take potion ~s.~%" potion)
    (setf (get-state potion) nil)))

(defclass red-potion (potion)
  ()
  (:default-initargs
   :attackbox-width (red-potion-attackbox-width)
   :attackbox-height (red-potion-attackbox-height)
   :attackbox-x-offset (red-potion-attackbox-x-offset)
   :attackbox-y-offset (red-potion-attackbox-y-offset)))

(defmethod entity-step! (game level (potion red-potion) entity-state dticks)
  (when (can-attack? potion (player level))
    (format t "~&Take potion ~s.~%" potion)
    (incf (get-health (player level)) 20)
    (setf (get-state potion) nil)))
