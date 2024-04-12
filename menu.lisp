;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled menu-background-y 45)

(defclass menu (mode)
  ())

(defmethod mode-step! (game (menu menu) dticks)
  (if (sdl2:keyboard-state-p :scancode-return)
      (setf (level game) (first-level game))
      (call-next-method)))

(defmethod render-mode! (renderer resources game (menu menu))
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
                            
  (call-next-method))

(defun make-menu (resources)
  `(:menu
    ,(make-instance
      'menu
      :entities
      (let* ((texture (get-resource '(:textures :button-atlas) resources))
             (button-height (scale (floor (sdl2:texture-height texture) 3)))
             (button-width  (scale (floor (sdl2:texture-width texture) 3))))
        (list
         (make-instance 'button
                        :x (/ (game-width) 2)
                        :y (play-button-y)
                        :height button-height
                        :width button-width
                        :state :idle
                        :animation (funcall (get-resource '(:animations :button :play) resources))
                        :action (lambda (game button)
                                  (declare (ignore button))
                                  (setf (mode game) (first-level game))))
         (make-instance 'button
                        :x (/ (game-width) 2)
                        :y (options-button-y)
                        :height button-height
                        :width button-width
                        :state :idle
                        :animation (funcall (get-resource '(:animations :button :options) resources)))
         (make-instance 'button
                        :x (/ (game-width) 2)
                        :y (quit-button-y)
                        :height button-height
                        :width button-width
                        :state :idle
                        :animation (funcall (get-resource '(:animations :button :quit) resources))
                        :action (lambda (game button)
                                  (declare (ignore button game))
                                  (sdl2:push-quit-event))))))
    ,@resources))
