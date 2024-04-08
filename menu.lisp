;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled menu-background-y 45)

(defclass menu ()
  ((buttons :initarg :buttons :reader get-buttons)))

(defmethod level-step! (game (menu menu) dticks)
  (if (sdl2:keyboard-state-p :scancode-return)
      (setf (level game) (first-level game))
      (map nil (lambda (entity)
                 (unless (null (get-state entity))
                   (entity-step! menu entity (get-state entity) dticks)))
           (get-buttons menu))))

(defmethod render-level! (renderer resources game (menu menu))
  (let* ((texture (get-resource '(:textures :menu-background) resources))
         (height (sdl2:texture-height texture))
         (width (sdl2:texture-width texture)))
    (sdl2:with-rects ((src 0 0 width height)
                      (dst 0 0 (game-width) (game-height)))
      (sdl2:render-copy renderer texture :source-rect src :dest-rect dst)))


  (let* ((texture (get-resource '(:textures :menu) resources))
         (height (sdl2:texture-height texture))
         (width (sdl2:texture-width texture))
         (scaled-height (scale height))
         (scaled-width (scale width)))
    (sdl2:with-rects ((src 0 0 width height)
                      (dst (floor (- (game-width) scaled-width) 2)
                           (menu-background-y)
                           scaled-width
                           scaled-height))
      (sdl2:render-copy renderer texture :source-rect src :dest-rect dst)))
                            
  (let ((*world-x-offset* 0))
    (map nil (lambda (button)
               (render-entity! renderer resources button))
         (get-buttons menu))))
