;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled level-complete-buttons-spacing 40)
(defun replay-button-position-x () (floor (- (/ (game-width) 2) (level-complete-buttons-spacing))))
(defun next-button-position-x () (floor (+ (/ (game-width) 2) (level-complete-buttons-spacing))))
(defun-scaled level-complete-buttons-position-y 300)

(defclass level-complete (mode)
  ())

(defmethod render-mode! (renderer resources game (level-complete level-complete))
  (render-mode! renderer resources game (level game))

  (sdl2:set-render-draw-blend-mode renderer :blend)
  (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #x3f)
  (sdl2:with-rects ((r 0 0 (game-width) (game-height)))
    (sdl2:render-fill-rect renderer r))
  (sdl2:set-render-draw-blend-mode renderer :none)

  (let ((background-texture (get-resource '(:textures :level-complete) resources)))
    (sdl2:with-rects ((src 0 0 (sdl2:texture-width background-texture) (sdl2:texture-height background-texture))
                      (dst (floor (- (game-width) (scale (sdl2:texture-width background-texture))) 2)
                           (floor (- (game-height) (scale (sdl2:texture-height background-texture))) 2)
                           (scale (sdl2:texture-width background-texture))
                           (scale (sdl2:texture-height background-texture))))
      (sdl2:render-copy renderer background-texture :source-rect src :dest-rect dst)))

  (call-next-method))

(defun make-level-complete (resources)
  `(:level-complete
    ,(let* ((urm-texture (get-resource '(:textures :urm-button-atlas) resources))
            (urm-button-width (scale (floor (sdl2:texture-width urm-texture) 3)))
            (urm-button-height (scale (floor (sdl2:texture-width urm-texture) 3)))
            (game-over (get-resource '(:game-over) resources)))
       (make-instance
        'level-complete
        :entities (list
                   (make-instance 'button
                                  :x (replay-button-position-x)
                                  :y (level-complete-buttons-position-y)
                                  :height urm-button-height
                                  :width urm-button-width
                                  :state :idle
                                  :animation (funcall (get-resource '(:animations :urm-buttons :restart) resources))
                                  :action (lambda (game button)
                                            (declare (ignore button))
                                            (let ((level (funcall (restart-level (level game)))))
                                              (setf (mode game) level
                                                    (level game) level))))
                   (make-instance 'button
                                  :x (next-button-position-x)
                                  :y (level-complete-buttons-position-y)
                                  :height urm-button-height
                                  :width urm-button-width
                                  :state :idle
                                  :animation (funcall (get-resource '(:animations :urm-buttons :resume) resources))
                                  :action (lambda (game button)
                                            (declare (ignore button))
                                            (let ((level (next-level (level game))))
                                              (setf (mode game) level
                                                    (level game) level)))))))
    ,@resources))
