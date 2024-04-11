;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass game-over (mode)
  ())

(defmethod render-mode! (renderer resources game (game-over game-over))
  (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #xff)
  (sdl2:render-clear renderer)
  (let ((texture (get-resource '(:textures :game-over) resources)))
    (sdl2:with-rects ((src 0 0 (sdl2:texture-width texture) (sdl2:texture-height texture))
                      (dst (floor (- (game-width) (sdl2:texture-width texture)) 2)
                           (floor (- (game-height) (sdl2:texture-height texture)) 2)
                           (sdl2:texture-width texture)
                           (sdl2:texture-height texture)))
      (sdl2:render-copy renderer texture :source-rect src :dest-rect dst)))
  (call-next-method))

(defun make-game-over (resources)
  `(:game-over
    ,(make-instance 'game-over)
    ,@resources))



