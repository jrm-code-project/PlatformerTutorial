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

(defmethod entity-step! (level (player player) (state (eql :idle)) dticks)
  (let ((l/r (l/r-input))
        (u/d (u/d-input)))

    (cond ((or (not (zerop l/r))
               (not (zerop u/d)))
           (cond ((< l/r 0) (setf (flip? player) t))
                 ((> l/r 0) (setf (flip? player) nil))
                 (t nil))
           (setf (get-state player) :running
                 (get-animation player) (make-instance 'frame-loop :frame-set (getf (frame-sets player) :running))))
          (t nil))))

(defun player-speed () (scalef .2))

(defmethod entity-step! (level (player player) (state (eql :running)) dticks)
  (let* ((l/r (l/r-input))
         (u/d (u/d-input))
         (dx (* (player-speed) l/r dticks))
         (dy (* (player-speed) u/d dticks)))
    (cond ((< dx 0) (move-entity-left! level player dx))
          ((> dx 0) (move-entity-right! level player dx))
          (t nil))
    (cond ((< dy 0) (move-entity-up! level player dy))
          ((> dy 0) (move-entity-down! level player dy))
          (t nil))
    (cond ((and (zerop (l/r-input))
                (zerop (u/d-input)))
           (setf (get-state player) :idle
                 (get-animation player) (make-instance 'frame-loop :frame-set (getf (frame-sets player) :idle))))
          ((< l/r 0) (setf (flip? player) t))
          ((> l/r 0) (setf (flip? player) nil))
          (t nil))))
