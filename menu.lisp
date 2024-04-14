;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass menu (mode)
  ())

(defmethod render-mode! (renderer resources game (menu menu))
  (let ((texture (get-resource '(:textures :menu) resources)))
    (sdl2:with-rects ((src 0 0 (sdl2:texture-width texture) (sdl2:texture-height texture))
                      (dst (floor (- (game-width) (sdl2:texture-width texture)) 2)
                           (floor (- (game-height) (sdl2:texture-height texture)) 2)
                           (sdl2:texture-width texture)
                           (sdl2:texture-height texture)))
      (sdl2:render-copy renderer texture :source-rect src :dest-rect dst)))
  (call-next-method))

(defmethod mode-step! (game (menu menu) dticks)
  (cond ((sdl2:keyboard-state-p :scancode-return)
         (setf (mode game) (level game)))
        (t (call-next-method))))

(defun make-menu (resources)
  `(:menu
    ,(make-instance 'menu)
    ,@resources))
