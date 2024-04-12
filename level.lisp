;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass level (mode)
  ((player   :initarg :player   :accessor player)
   (tiles    :initarg :tiles    :accessor tiles)
   (x-offset :initform 0        :accessor x-offset)))

(defun level-tiles-width (level-tiles)
  (array-dimension level-tiles 0))

(defun level-tiles-height (level-tiles)
  (array-dimension level-tiles 1))

(defun level-width (level)
  (* (level-tiles-width (tiles level)) (tile-size)))

(defparameter *render-tile-outline* nil)

(defun render-tiles! (renderer outside-texture tiles)
  (multiple-value-bind (offset-tiles offset-within-tile) (floor *world-x-offset* (tile-size))
    (dotimes (row (height-in-tiles))
      (dotimes (column (if (zerop offset-within-tile)
                           (width-in-tiles)
                           (1+ (width-in-tiles))))
        (let ((tile (aref tiles (+ column offset-tiles) row 0)))
          (multiple-value-bind (tile-source-row tile-source-column) (floor tile 12)
            (sdl2:with-rects ((src (* tile-source-column (base-tile-size))
                                   (* tile-source-row (base-tile-size))
                                   (base-tile-size)
                                   (base-tile-size))
                              (dst (- (* column (tile-size)) offset-within-tile)
                                   (* row (tile-size))
                                   (tile-size)
                                   (tile-size)))
              (sdl2:render-copy renderer
                                outside-texture
                                :source-rect src
                                :dest-rect dst)
              (when *render-tile-outline*
                (sdl2:render-draw-rect renderer dst)))))))))

(defun adjust-x-offset! (level)
  (cond ((and (< (- (get-x (player level)) (x-offset level)) (/ (game-width) 5))
              (> (x-offset level) 0))
         (setf (x-offset level)
               (max 0 (floor (- (get-x (player level)) (/ (game-width) 5))))))
        ((and (> (- (get-x (player level)) (x-offset level)) (/ (* 4 (game-width)) 5))
              (< (x-offset level) (- (level-width level) (game-width))))
         (setf (x-offset level)
               (min (- (level-width level) (game-width))
                    (floor (- (get-x (player level)) (/ (* 4 (game-width)) 5))))))
        (t nil)))

(defmethod render-mode! (renderer resources game (level level))
  (adjust-x-offset! level)
  (let ((*world-x-offset* (x-offset level)))
    (render-tiles! renderer (get-resource '(:textures :outside) resources) (tiles level))
    (call-next-method)))

(defmethod mode-step! (game (level level) dticks)
  (cond ((sdl2:keyboard-state-p :scancode-backspace)
         (setf (mode game) (menu game)))
        ((sdl2:keyboard-state-p :scancode-escape)
         (setf (mode game) (paused-menu game)))
        (t (call-next-method))))

(defun blank-tile? (level tile-x tile-y)
  (and (>= tile-x 0)
       (< tile-x (level-tiles-width (tiles level)))
       (>= tile-y 0)
       (< tile-y (level-tiles-height (tiles level)))
       (= 11 (aref (tiles level) tile-x tile-y 0))))

(defun coord->tile (x y)
  (values (floor x (tile-size)) (floor y (tile-size))))

(defun point-supported? (level x y)
  (multiple-value-bind (tile-x tile-y) (coord->tile x (+ y 1))
    (not (blank-tile? level tile-x tile-y))))

(defun point-against-right-wall? (level x y)
  (multiple-value-bind (tile-x tile-y) (coord->tile (+ x 1) y)
    (not (blank-tile? level tile-x tile-y))))

(defun point-against-left-wall? (level x y)
  (multiple-value-bind (tile-x tile-y) (coord->tile (- x 1) y)
    (not (blank-tile? level tile-x tile-y))))

(defun point-covered? (level x y)
  (multiple-value-bind (tile-x tile-y) (coord->tile x (- y 1))
    (not (blank-tile? level tile-x tile-y))))

(defun tile-left (tile-x)
  (* tile-x (tile-size)))

(defun tile-right (tile-x)
  (- (* (+ tile-x 1) (tile-size)) 1))

(defun tile-top (tile-y)
  (* tile-y (tile-size)))

(defun tile-bottom (tile-y)
  (- (* (+ tile-y 1) (tile-size)) 1))

(defun move-point-left (level x y dx)
  (multiple-value-bind (tile-x tile-y) (coord->tile (+ x dx) y)
    (if (blank-tile? level tile-x tile-y)
        (+ x dx)
        (tile-left (+ tile-x 1)))))

(defun move-point-right (level x y dx)
  (multiple-value-bind (tile-x tile-y) (coord->tile (+ x dx) y)
    (if (blank-tile? level tile-x tile-y)
        (+ x dx)
        (tile-right (- tile-x 1)))))

(defun move-point-down (level x y dy)
  (multiple-value-bind (tile-x tile-y) (coord->tile x (+ y dy))
    (if (blank-tile? level tile-x tile-y)
        (+ y dy)
        (tile-bottom (- tile-y 1)))))

(defun move-point-up (level x y dy)
  (multiple-value-bind (tile-x tile-y) (coord->tile x (+ y dy))
    (if (blank-tile? level tile-x tile-y)
        (+ y dy)
        (tile-top (+ tile-y 1)))))

(defun make-level (resources)
  `(:level
    ,(let ((player (make-instance 'player
                                  :x (scale 100)
                                  :y (scale 150)
                                  :state :idle
                                  :animation (funcall (get-resource '(:animations :player :idle) resources))
                                  :animations (get-resource '(:animations :player) resources)))
           (entities '())
           (level-tiles (car (read-level-data))))
       (make-instance 'level
                      :tiles level-tiles
                      :player player
                      :entities (cons player entities)))
    ,@resources))
