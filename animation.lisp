;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defgeneric get-frame (animation))

(defclass frame-set ()
  ((sprite-sheet    :initarg :sprite-sheet     :reader sprite-sheet)
   (row             :initarg :row              :reader get-row)
   (ticks-per-frame :initarg :ticks-per-frame  :reader ticks-per-frame)))

(defun frame-set-length (frame-set)
  (row-limit (sprite-sheet frame-set) (get-row frame-set)))

(defclass animation ()
  ((frame-set  :initarg :frame-set        :reader frame-set)
   (start-tick :initform (sdl2:get-ticks) :accessor start-tick)))

(defun render-animation! (renderer textures animation x y &key flip?)
  (let ((frame (get-frame animation)))
    (render-sprite! renderer textures (sprite-sheet (frame-set animation))
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

(defun button-animation (sprite-sheet row)
  (let ((frame-set (make-instance 'frame-set
                                  :sprite-sheet sprite-sheet
                                  :row row
                                  :ticks-per-frame most-positive-fixnum)))
    (lambda ()
      (make-instance 'slides
                      :frame-set frame-set
                      :current-slide :idle
                      :slides #(:idle :hover :pressed)))))

(defun frame-loop-animation (sprite-sheet row)
  (let ((frame-set (make-instance 'frame-set
                                  :sprite-sheet sprite-sheet
                                  :row row
                                  :ticks-per-frame 100)))
    (lambda ()
      (make-instance 'frame-loop :frame-set frame-set))))

(defun one-shot-animation (sprite-sheet row)
  (let ((frame-set (make-instance 'frame-set
                                  :sprite-sheet sprite-sheet
                                  :row row
                                  :ticks-per-frame 100)))
    (lambda ()
      (make-instance 'one-shot :frame-set frame-set))))

(defun make-animations (resources)
  `(:animations
    (:button
     ,(let ((button-atlas
              (make-sprite-sheet (lambda (textures) (getf textures :button-atlas))
                                 (getf resources :textures)
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
     ,(let ((crabby-sprite-sheet
              (make-sprite-sheet (lambda (textures) (getf textures :crabby))
                                 (getf resources :textures)
                                 #(:idle
                                   :running
                                   :attack
                                   :hit
                                   :dying)
                                 #(9 6 7 4 5)
                                 :baseline-offset 4)))
        `(:attack
          ,(frame-loop-animation crabby-sprite-sheet :attack)
          :dying
          ,(frame-loop-animation crabby-sprite-sheet :dying)
          :hit
          ,(frame-loop-animation crabby-sprite-sheet :hit)                                            
          :idle
          ,(frame-loop-animation crabby-sprite-sheet :idle)                                            
          :running
          ,(frame-loop-animation crabby-sprite-sheet :running)))
     :player
     ,(let ((player-sprite-sheet
              (make-sprite-sheet (lambda (textures) (getf textures :player))
                                 (getf resources :textures)
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
        `(:attack1
          ,(frame-loop-animation player-sprite-sheet :attack1)
          :falling
          ,(frame-loop-animation player-sprite-sheet :falling)
          :hit
          ,(one-shot-animation player-sprite-sheet :hit)
          :idle
          ,(frame-loop-animation player-sprite-sheet :idle)
          :jumping
          ,(one-shot-animation player-sprite-sheet :jumping)
          :landing
          ,(one-shot-animation player-sprite-sheet :landing)
          :running
          ,(frame-loop-animation player-sprite-sheet :running)))
     :sound-buttons
     ,(let ((button-atlas
              (make-sprite-sheet (lambda (textures) (getf textures :sound-button-atlas))
                                 (getf resources :textures)
                                 #(:on
                                   :off)
                                 #(3 3))))
        `(:on
          ,(button-animation button-atlas :on)
          :off
          ,(button-animation button-atlas :off)))

     :urm-buttons
     ,(let ((button-atlas
              (make-sprite-sheet (lambda (textures) (getf textures :urm-button-atlas))
                                 (getf resources :textures)
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
              (make-instance 'sprite-sheet
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
