;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defgeneric get-frame (animation))

(defclass frame-set ()
  ((atlas    :initarg :atlas     :reader atlas)
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

;;;;;;;;;;;
;;; Load animations for the game

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
    (:player
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
        `(:attack1
          ,(frame-loop-animation player-atlas :attack1)
          :falling
          ,(frame-loop-animation player-atlas :falling)
          :hit
          ,(one-shot-animation player-atlas :hit)
          :idle
          ,(frame-loop-animation player-atlas :idle)
          :jumping
          ,(one-shot-animation player-atlas :jumping)
          :landing
          ,(one-shot-animation player-atlas :landing)
          :running
          ,(frame-loop-animation player-atlas :running))))
    ,@resources))
