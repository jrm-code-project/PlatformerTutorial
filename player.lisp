;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled player-width 16)
(defun-scaled player-height 26)

(defclass player (hitbox entity)
  ()
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

(defmethod (setf get-state) :after ((state (eql :idle)) (player player))
  (start-animation! player :idle))

(defmethod (setf get-state) :after ((state (eql :running)) (player player))
  (start-animation! player :running))

(defmethod entity-step! (game mode (player player) (state (eql :idle)) dticks)
  (let ((l/r (l/r-input))
        (u/d (u/d-input)))

    (cond ((or (not (zerop l/r))
               (not (zerop u/d)))
           (cond ((< l/r 0) (setf (flip? player) t))
                 ((> l/r 0) (setf (flip? player) nil))
                 (t nil))
           (setf (get-state player) :running))
          (t nil))))

(defun player-speed () (scalef .2))

(defmethod entity-step! (game mode (player player) (state (eql :running)) dticks)
  (let* ((l/r (l/r-input))
         (u/d (u/d-input))
         (dx (* (player-speed) l/r dticks))
         (dy (* (player-speed) u/d dticks))
         (x* (+ (get-x player) dx))
         (y* (+ (get-y player) dy)))
    (when (and (> x* (floor (get-width player) 2))
               (< (+ x* (floor (get-width player) 2)) (game-width)))
      (setf (get-x player) x*))
    (when (and (> y* (get-height player))
               (< y* (game-height)))
      (setf (get-y player) y*))
    (cond ((and (zerop (l/r-input))
                (zerop (u/d-input)))
           (setf (get-state player) :idle))
          ((< l/r 0) (setf (flip? player) t))
          ((> l/r 0) (setf (flip? player) nil))
          (t nil))))
