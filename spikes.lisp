;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled spikes-attackbox-width 32)
(defun-scaled spikes-attackbox-height 16)
(defun-scaled spikes-attackbox-x-offset -16)
(defun-scaled spikes-attackbox-y-offset 0)

(defclass spikes (attackbox entity)
  ()
  (:default-initargs
   :attackbox-width (spikes-attackbox-width)
   :attackbox-height (spikes-attackbox-height)
   :attackbox-x-offset (spikes-attackbox-x-offset)
   :attackbox-y-offset (spikes-attackbox-y-offset)))

(defmethod entity-step! (game level (spikes spikes) (state (eql :idle)) dticks)
  (when (can-attack? spikes (player level))
    (unless (member (get-state (player level)) '(:hit))
      (hit! (player level)))))
