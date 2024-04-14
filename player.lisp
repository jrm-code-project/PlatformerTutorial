;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled player-width 16)
(defun-scaled player-height 26)

(defclass player (hitbox entity)
  ((delta-y :initform 0.0 :accessor delta-y))
  (:default-initargs
   :width (player-width)
   :height (player-height)))

(defun l/r-input ()
  (- (if (sdl2:keyboard-state-p :scancode-right)
         1
         0)
     (if (sdl2:keyboard-state-p :scancode-left)
         1
         0)))

(defun u/d-input ()
  (- (if (sdl2:keyboard-state-p :scancode-down)
         1
         0)
     (if (sdl2:keyboard-state-p :scancode-up)
         1
         0)))

(defmethod (setf get-state) :after ((state (eql :falling)) (player player))
  (start-animation! player :falling))

(defmethod (setf get-state) :after ((state (eql :idle)) (player player))
  (start-animation! player :idle))

(defmethod (setf get-state) :after ((state (eql :jumping)) (player player))
  (start-animation! player :jumping))

(defmethod (setf get-state) :after ((state (eql :landing)) (player player))
  (start-animation! player :landing))

(defmethod (setf get-state) :after ((state (eql :running)) (player player))
  (start-animation! player :running))

(defun move-player-horizontally! (level player dticks)
  (let* ((l/r (l/r-input))
         (dx  (* (player-speed) l/r dticks)))
    (cond ((< dx 0)
           (move-entity-left! level player dx)
           (setf (flip? player) t))
          ((> dx 0)
           (move-entity-right! level player dx)
           (setf (flip? player) nil))
          (t nil))))

(defun move-player-vertically! (level player dticks)
  (let ((dy (* (delta-y player) dticks)))
    (unless (zerop dy)
      (move-entity-vertically! level player dy)))
  (incf (delta-y player) (* (gravity) dticks)))

(defmethod entity-step! (game level (player player) (state (eql :falling)) dticks)
  (if (entity-supported? level player)
      (setf (delta-y player) 0
            (get-state player) :landing)
      (progn
        (move-player-horizontally! level player dticks)
        (move-player-vertically! level player dticks))))

(defmethod entity-step! (game level (player player) (state (eql :idle)) dticks)
  (let ((l/r (l/r-input)))
    (cond ((not (entity-supported? level player))
           (setf (get-state player) :falling))
          ((sdl2:keyboard-state-p :scancode-up)
           (setf (get-state player) :jumping))
          ((not (zerop l/r))
           (cond ((< l/r 0) (setf (flip? player) t))
                 ((> l/r 0) (setf (flip? player) nil))
                 (t nil))
           (setf (get-state player) :running))
          (t nil))))

(defun jump-velocity () (scalef -.375))

(defmethod entity-step! (game level (player player) (state (eql :jumping)) dticks)
  (cond ((< (get-frame (get-animation player)) 1)
         (setf (delta-y player) (jump-velocity)))
        ((entity-covered? level player)
         (setf (delta-y player) (scalef .1)
               (get-state player) :falling))
        ((> (delta-y player) 0.0)
         (setf (get-state player) :falling))
        (t
         (move-player-horizontally! level player dticks)
         (move-player-vertically! level player dticks))))

(defmethod entity-step! (game level (player player) (state (eql :landing)) dticks)
  (when (animation-finished? (get-animation player))
    (setf (get-state player) :idle)))

(defun player-speed () (scalef .15))

(defmethod entity-step! (game level (player player) (state (eql :running)) dticks)
  (cond ((not (entity-supported? level player))
         (setf (get-state player) :falling))
        ((sdl2:keyboard-state-p :scancode-up)
         (setf (get-state player) :jumping))
        ((zerop l/r)
         (setf (get-state player) :idle))
        (t (move-player-horizontally! level player dticks))))
