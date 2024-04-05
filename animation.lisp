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

(defclass frame-loop (animation)
  ())

(defmethod get-frame ((frame-loop frame-loop))
  (let* ((total-ticks (- (sdl2:get-ticks) (start-tick frame-loop)))
         (absolute-frame (floor total-ticks (ticks-per-frame (frame-set frame-loop)))))
    (mod absolute-frame (frame-set-length (frame-set frame-loop)))))

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

(defun render-animation! (renderer textures animation x y &key flip?)
  (let ((frame (get-frame animation)))
    (render-sprite! renderer textures (sprite-sheet (frame-set animation))
                    (get-row (frame-set animation)) frame x y :flip? flip?)))
