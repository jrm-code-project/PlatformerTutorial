;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled play-button-y    206)
(defun-scaled options-button-y 276)
(defun-scaled quit-button-y    346)

(defclass button (hitbox entity)
  ((action :initarg :action
           :initform (lambda (game button) (declare (ignore button game)) (format t "~&Unimplemented button.~%"))
           :reader action)))

(defmethod entity-step! (game level (button button) (state (eql :idle)) dticks)
  (declare (ignore dticks))
  (multiple-value-bind (mouse-x mouse-y mouse-buttons) (sdl2:mouse-state)
    (when (entity-under-point? button mouse-x mouse-y)
      (unless (logbitp 0 mouse-buttons)
        (setf (get-state button) :hover
              (current-slide (get-animation button)) :hover)))))

(defmethod entity-step! (game level (button button) (state (eql :hover)) dticks)
  (declare (ignore dticks))
  (multiple-value-bind (mouse-x mouse-y mouse-buttons) (sdl2:mouse-state)
    (cond ((not (entity-under-point? button mouse-x mouse-y))
           (setf (get-state button) :idle
                 (current-slide (get-animation button)) :idle))
          ((logbitp 0 mouse-buttons)
           (setf (get-state button) :pressed
                 (current-slide (get-animation button)) :pressed))
          (t nil))))

(defmethod entity-step! (game level (button button) (state (eql :pressed)) dticks)
  (declare (ignore dticks))
  (multiple-value-bind (mouse-x mouse-y mouse-buttons) (sdl2:mouse-state)
    (cond ((not (entity-under-point? button mouse-x mouse-y))
           (setf (get-state button) :idle
                 (current-slide (get-animation button)) :idle))
          ((not (logbitp 0 mouse-buttons))
           (setf (get-state button) :idle
                 (current-slide (get-animation button)) :idle)
           (funcall (action button) game button))
          (t nil))))

(defclass slider (button)
  ((left-limit  :initarg :left-limit  :reader left-limit)
   (right-limit :initarg :right-limit :reader right-limit)))

(defmethod entity-step! (game level (slider slider) (state (eql :pressed)) dticks)
  (declare (ignore dticks))
  (multiple-value-bind (mouse-x mouse-y mouse-buttons) (sdl2:mouse-state)
    (cond ((not (entity-under-point? slider mouse-x mouse-y))
           (setf (get-state slider) :idle
                 (current-slide (get-animation slider)) :idle)
           (funcall (action slider) game slider))
          ((not (logbitp 0 mouse-buttons))
           (setf (get-state slider) :idle
                 (current-slide (get-animation slider)) :idle)
           (funcall (action slider) game slider))
          ((and (>= mouse-x (left-limit slider))
                (<= mouse-x (right-limit slider)))
           (setf (get-x slider) mouse-x))
          (t nil))))
