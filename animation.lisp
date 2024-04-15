;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defgeneric get-frame (animation))

(defclass frame-set ()
  ((atlas           :initarg :atlas            :reader atlas)
   (row             :initarg :row              :reader get-row)
   (ticks-per-frame :initarg :ticks-per-frame  :reader ticks-per-frame)))

(defun frame-set-length (frame-set)
  (row-limit (atlas frame-set) (get-row frame-set)))

(defclass animation ()
  ((frame-set  :initarg :frame-set        :reader frame-set)
   (start-tick :initform (sdl2:get-ticks) :accessor start-tick)))

(defun render-animation! (renderer textures animation x y &key flip?)
  (let ((frame (get-frame animation)))
    (render-sprite! renderer textures (atlas (frame-set animation))
                    (get-row (frame-set animation)) frame x y :flip? flip?)))

;;;;;;;;
;;; Frame Loop
;;;   Just runs on repeat.

(defclass frame-loop (animation)
  ())

(defmethod get-frame ((frame-loop frame-loop))
  (let* ((total-ticks (- (sdl2:get-ticks) (start-tick frame-loop)))
         (absolute-frame (floor total-ticks (ticks-per-frame (frame-set frame-loop)))))
    (mod absolute-frame (frame-set-length (frame-set frame-loop)))))

;;;;;;;;;;;
;;; One-shot
;;;   Runs once, then freezes

(defclass one-shot (animation)
  ())

(defmethod get-frame ((one-shot one-shot))
  (let* ((total-ticks (- (sdl2:get-ticks) (start-tick one-shot)))
         (absolute-frame  (floor total-ticks (ticks-per-frame (frame-set one-shot)))))
    (min absolute-frame (- (frame-set-length (frame-set one-shot)) 1))))

(defun animation-finished? (one-shot)
  (let* ((total-ticks (- (sdl2:get-ticks) (start-tick one-shot)))
         (absolute-frame  (floor total-ticks (ticks-per-frame (frame-set one-shot)))))
    (>= absolute-frame (frame-set-length (frame-set one-shot)))))

;;;;;;;;;;;;
;;; Slides
;;;   Select the current-slide to show.

(defclass slides (animation)
  ((current-slide :initarg :current-slide :accessor current-slide)
   (slides :initarg :slides :reader slides)))

(defmethod get-frame ((slides slides))
  (position (current-slide slides) (slides slides)))

;;;;;;;;;;;
;;; Load animations for the game

(defun button-animation (atlas row)
  (let ((frame-set (make-instance 'frame-set
                                  :atlas atlas
                                  :row row)))
    (lambda ()
      (make-instance 'slides
                      :frame-set frame-set
                      :current-slide :idle
                      :slides #(:idle :hover :pressed)))))

(defun frame-loop-animation (atlas row)
  (let ((frame-set (make-instance 'frame-set
                                  :atlas atlas
                                  :row row
                                  :ticks-per-frame 100)))
    (lambda ()
      (make-instance 'frame-loop :frame-set frame-set))))

(defun one-shot-animation (atlas row)
  (let ((frame-set (make-instance 'frame-set
                                  :atlas atlas
                                  :row row
                                  :ticks-per-frame 100)))
    (lambda ()
      (make-instance 'one-shot :frame-set frame-set))))

(defun make-animations (resources)
  `(:animations
    (:button
     ,(let ((button-atlas
              (make-atlas (get-resource '(:textures :button-atlas) resources)
                          (lambda (textures) (getf textures :button-atlas))
                          #(:play
                            :options
                            :quit)
                          #(3 3 3))))
        `(:play
          ,(button-animation button-atlas :play)
          :options
          ,(button-animation button-atlas :options)
          :quit
          ,(button-animation button-atlas :quit)))
     :crabby
     ,(let ((crabby-atlas
              (make-atlas (get-resource '(:textures :crabby) resources)
                          (lambda (textures) (getf textures :crabby))
                          #(:idle
                            :running
                            :attack
                            :hit
                            :dying)
                          #(9 6 7 4 5)
                          :baseline-offset 4)))
        `(:idle
          ,(frame-loop-animation crabby-atlas :idle)                                            
          :running
          ,(frame-loop-animation crabby-atlas :running)))
     :player
     ,(let ((player-atlas
              (make-atlas (get-resource '(:textures :player) resources)
                          (lambda (textures) (getf textures :player))
                          #(:idle
                            :running
                            :jumping
                            :falling
                            :landing
                            :hit
                            :attack1
                            :attack2
                            :attack3)
                          #(5 6 3 1 2 4 3 3 3)
                          :baseline-offset 9)))
        `(:falling
          ,(frame-loop-animation player-atlas :falling)
          :idle
          ,(frame-loop-animation player-atlas :idle)
          :jumping
          ,(one-shot-animation player-atlas :jumping)
          :landing
          ,(one-shot-animation player-atlas :landing)
          :running
          ,(frame-loop-animation player-atlas :running)))
     :sound-buttons
     ,(let ((button-atlas
              (make-atlas (get-resource '(:textures :sound-button-atlas) resources)
                          (lambda (textures) (getf textures :sound-button-atlas))
                          #(:on
                            :off)
                          #(3 3))))
        `(:on
          ,(button-animation button-atlas :on)
          :off
          ,(button-animation button-atlas :off)))

     :urm-buttons
     ,(let ((button-atlas
              (make-atlas (get-resource '(:textures :urm-button-atlas) resources)
                          (lambda (textures) (getf textures :urm-button-atlas))
                          #(:resume
                            :restart
                            :menu)
                          #(3 3 3))))
        `(:menu
          ,(button-animation button-atlas :menu)
          :restart
          ,(button-animation button-atlas :restart)
          :resume
          ,(button-animation button-atlas :resume)))
     :volume-button
     ,(let ((button-atlas
              (make-instance 'atlas
                             :selector (lambda (textures) (getf textures :volume-button-atlas))
                             :frame-width (base-volume-slider-width)
                             :frame-height (sdl2:texture-height
                                            (get-resource '(:textures :volume-button-atlas) resources))
                             :baseline-offset 0
                             :rows #(:volume)
                             :row-limits #(3))))
        `(:slider
          ,(button-animation button-atlas :volume))))
    ,@resources))
