;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled pause-menu-y 25)

(defun-scaled music-button-position-x 471)
(defun-scaled music-button-position-y 182)

(defun-scaled sfx-button-position-x 471)
(defun-scaled sfx-button-position-y 228)

(defun-scaled menu-button-position-x 341)
(defun-scaled menu-button-position-y 381)

(defun-scaled restart-button-position-x 415)
(defun-scaled restart-button-position-y 381)

(defun-scaled resume-button-position-x 490)
(defun-scaled resume-button-position-y 381)

(defun-scaled volume-slider-width 28)
(defun-scaled volume-slider-height 45)
(defun-scaled volume-slider-y 323)
(defun base-volume-slider-background-offset () (* 28 3))
(defun-scaled volume-slider-background-width 215)

(defclass pause-menu (mode)
  ())

(defmethod render-mode! (renderer resources game (pause-menu pause-menu))
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

  (let ((*world-x-offset* 0))
    (call-next-method)))

(defun make-pause-menu (resources)
  `(:pause-menu
    ,(make-instance
      'pause-menu
      :entities
      (let* ((urm-texture (get-resource '(:textures :urm-button-atlas) resources))
             (urm-button-width (scale (floor (sdl2:texture-width urm-texture) 3)))
             (urm-button-height (scale (floor (sdl2:texture-width urm-texture) 3)))
             (sound-texture (get-resource '(:textures :sound-button-atlas) resources))
             (sound-button-height (scale (floor (sdl2:texture-height sound-texture) 2)))
             (sound-button-width  (scale (floor (sdl2:texture-width sound-texture) 3)))
             (volume-texture (get-resource '(:textures :volume-button-atlas) resources))
             (volume-slider-height (scale (sdl2:texture-height volume-texture)))
             )
        (list
         (make-instance 'button
                        :x (music-button-position-x)
                        :y (music-button-position-y)
                        :height sound-button-height
                        :width sound-button-width
                        :state :idle
                        :animation (funcall (get-resource '(:animations :sound-buttons :on) resources))
                        :animations (get-resource '(:animations :sound-buttons) resources)
                        :action (lambda (game button)
                                  (declare (ignore game))
                                  (cond ((eql (get-row (frame-set (get-animation button))) :on)
                                         (setf (get-animation button) (funcall (getf (animations button) :off))))
                                        ((eql (get-row (frame-set (get-animation button))) :off)
                                         (setf (get-animation button) (funcall (getf (animations button) :on))))
                                        (t nil))))
         (make-instance 'button
                        :x (sfx-button-position-x)
                        :y (sfx-button-position-y)
                        :height sound-button-height
                        :width sound-button-width
                        :state :idle
                        :animation (funcall (get-resource '(:animations :sound-buttons :on) resources))
                        :animations (get-resource '(:animations :sound-buttons) resources)
                        :action (lambda (game button)
                                  (declare (ignore game))
                                  (cond ((eql (get-row (frame-set (get-animation button))) :on)
                                         (setf (get-animation button) (funcall (getf (animations button) :off))))
                                        ((eql (get-row (frame-set (get-animation button))) :off)
                                         (setf (get-animation button) (funcall (getf (animations button) :on))))
                                        (t nil))))
         (make-instance 'slider
                        :x (/ (game-width) 2)
                        :y (volume-slider-y)
                        :height volume-slider-height
                        :width (volume-slider-width)
                        :left-limit (+ (floor (- (game-width) (volume-slider-background-width)) 2) (scale 22))
                        :right-limit (- (floor (+ (game-width) (volume-slider-background-width)) 2) (scale 22))
                        :state :idle
                        :animation (funcall (get-resource '(:animations :volume-button :slider) resources)))
         (make-instance 'button
                        :x (menu-button-position-x)
                        :y (menu-button-position-y)
                        :height urm-button-height
                        :width urm-button-width
                        :state :idle
                        :animation (funcall (get-resource '(:animations :urm-buttons :menu) resources))
                        :action (lambda (game button)
                                  (declare (ignore button))
                                  (setf (mode game) (menu game))))
         (make-instance 'button
                        :x (restart-button-position-x)
                        :y (restart-button-position-y)
                        :height urm-button-height
                        :width urm-button-width
                        :state :idle
                        :animation (funcall (get-resource '(:animations :urm-buttons :restart) resources))
                        :action (lambda (game button)
                                  (declare (ignore button))
                                  (setf (mode game) (first-level game))))
         (make-instance 'button
                        :x (resume-button-position-x)
                        :y (resume-button-position-y)
                        :height urm-button-height
                        :width urm-button-width
                        :state :idle
                        :animation (funcall (get-resource '(:animations :urm-buttons :resume) resources))
                        :action (lambda (game button)
                                  (declare (ignore button))
                                  (setf (mode game) (level game))))
         )))
    ,@resources))
