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

(defmethod entity-step! ((player player) (state (eql :idle)) dticks)
  (cond ((not (zerop (l/r-input)))
         (setf (flip? player) (< (l/r-input) 0)
               (get-state player) :running
               (get-animation player) (make-instance 'frame-loop :frame-set (getf (frame-sets player) :running))))
        (t nil)))

(defun player-speed () (scalef .1))

(defmethod entity-step! ((player player) (state (eql :running)) dticks)
  (let* ((dx (* (player-speed) (l/r-input) dticks))
         (x* (+ (get-x player) dx)))
    (when (and (> x* (floor (get-width player) 2))
               (< (+ x* (floor (get-width player) 2)) (game-width)))
      (setf (get-x player) x*))
    (cond ((zerop (l/r-input))
           (setf (get-state player) :idle
                 (get-animation player) (make-instance 'frame-loop :frame-set (getf (frame-sets player) :idle))))
          (t (setf (flip? player) (< (l/r-input) 0))))))
