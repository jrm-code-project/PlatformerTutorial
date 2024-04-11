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

(defclass frame-loop (animation)
  ())

(defmethod get-frame ((frame-loop frame-loop))
  (let* ((total-ticks (- (sdl2:get-ticks) (start-tick frame-loop)))
         (absolute-frame (floor total-ticks (ticks-per-frame (frame-set frame-loop)))))
    (mod absolute-frame (frame-set-length (frame-set frame-loop)))))

(defun render-animation! (renderer textures animation x y &key flip?)
  (let ((frame (get-frame animation)))
    (render-sprite! renderer textures (atlas (frame-set animation))
                    (get-row (frame-set animation)) frame x y :flip? flip?)))

(defun make-animations (resources)
  (let* ((player-atlas
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
 
                       :baseline-offset 8))
         (idle-frame-set
           (make-instance 'frame-set
                          :atlas player-atlas
                          :row :idle
                          :ticks-per-frame 100)))
    `(:animations
      (:player
       (:idle
        ,(make-instance 'frame-loop :frame-set idle-frame-set)))
      ,@resources)))
