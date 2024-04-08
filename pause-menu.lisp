;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled pause-menu-y 25)

(defun-scaled music-button-position-x 471)
(defun-scaled music-button-position-y 182)

(defun-scaled sfx-button-position-x 471)
(defun-scaled sfx-button-position-y 228)

(defun-scaled menu-button-position-x 341)
(defun-scaled menu-button-position-y 381)

(defun-scaled restart-button-position-x 387)
(defun-scaled restart-button-position-y 381)

(defun-scaled resume-button-position-x 462)
(defun-scaled resume-button-position-y 381)

(defun-scaled volume-slider-width 28)
(defun-scaled volume-slider-height 45)
(defun-scaled volume-slider-y 323)
(defun base-volume-slider-background-offset () (* 28 3))
(defun-scaled volume-slider-background-width 215)

(defclass pause-menu ()
  ((buttons :initarg :buttons :reader get-buttons)))

(defmethod level-step! (game (pause-menu pause-menu) dticks)
  (map nil (lambda (button)
             (entity-step! pause-menu button (get-state button) dticks))
       (get-buttons pause-menu)))

(defun render-pause-menu! (renderer resources pause-menu)
  (map nil (lambda (button)
             (render-entity! renderer resources button))
       (get-buttons pause-menu)))

(defmethod render-level! (renderer resources game (pause-menu pause-menu))
  (let ((background-texture (get-resource '(:textures :pause-menu) resources)))
    (sdl2:with-rects ((src 0
                           0
                           (sdl2:texture-width background-texture)
                           (sdl2:texture-height background-texture))
                      (dst (floor (- (game-width) (scale (sdl2:texture-width background-texture))) 2)
                           (pause-menu-y)
                           (scale (sdl2:texture-width background-texture))
                           (scale (sdl2:texture-height background-texture))))
      (sdl2:render-copy renderer background-texture :source-rect src :dest-rect dst))) 
                         
  (let ((slider-background (get-resource '(:textures :volume-button-atlas) resources)))
    (sdl2:with-rects ((src (base-volume-slider-background-offset)
                           0
                           (base-volume-slider-background-width)
                           (base-volume-slider-height))
                      (dst (floor (- (game-width) (volume-slider-background-width)) 2)
                           (- (volume-slider-y) (volume-slider-height))
                           (volume-slider-background-width)
                           (volume-slider-height)))
      (sdl2:render-copy renderer slider-background :source-rect src :dest-rect dst)))

  (render-pause-menu! renderer resources pause-menu))
