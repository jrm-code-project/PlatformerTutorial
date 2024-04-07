;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled play-button-y    206)
(defun-scaled options-button-y 276)
(defun-scaled quit-button-y    346)

(defclass button (hitbox entity)
  ((action :initarg :action
           :initform (lambda () (format t "~&Unimplemented button.~%"))
           :reader action)))

(defmethod entity-step! (level (button button) (state (eql :idle)) dticks)
  (declare (ignore dticks))
  (multiple-value-bind (mouse-x mouse-y mouse-buttons) (sdl2:mouse-state)
    (when (entity-under-point? button mouse-x mouse-y)
      (unless (logbitp 0 mouse-buttons)
        (setf (get-state button) :hover
              (current-slide (get-animation button)) :hover)))))

(defmethod entity-step! (level (button button) (state (eql :hover)) dticks)
  (declare (ignore dticks))
  (multiple-value-bind (mouse-x mouse-y mouse-buttons) (sdl2:mouse-state)
    (cond ((not (entity-under-point? button mouse-x mouse-y))
           (setf (get-state button) :idle
                 (current-slide (get-animation button)) :idle))
          ((logbitp 0 mouse-buttons)
           (setf (get-state button) :pressed
                 (current-slide (get-animation button)) :pressed))
          (t nil))))

(defmethod entity-step! (level (button button) (state (eql :pressed)) dticks)
  (declare (ignore dticks))
  (multiple-value-bind (mouse-x mouse-y mouse-buttons) (sdl2:mouse-state)
    (cond ((not (entity-under-point? button mouse-x mouse-y))
           (setf (get-state button) :idle
                 (current-slide (get-animation button)) :idle))
          ((not (logbitp 0 mouse-buttons))
           (setf (get-state button) :idle
                 (current-slide (get-animation button)) :idle)
           (funcall (action button)))
          (t nil))))
