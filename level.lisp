;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass level ()
  ((entities :initarg :entities :accessor entities)
   (player   :initarg :player   :accessor player)
   (tiles    :initarg :tiles    :accessor tiles)))

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
  (cond ((and (< (- (get-x (player level)) *world-x-offset*) (/ (game-width) 5))
              (> *world-x-offset* 0))
         (setf *world-x-offset*
               (max 0 (floor (- (get-x (player level)) (/ (game-width) 5))))))
        ((and (> (- (get-x (player level)) *world-x-offset*) (/ (* 4 (game-width)) 5))
              (< *world-x-offset* (- (level-width level) (game-width))))
         (setf *world-x-offset*
               (min (- (level-width level) (game-width))
                    (floor (- (get-x (player level)) (/ (* 4 (game-width)) 5))))))
        (t nil)))

(defmethod render-level! (renderer resources game (level level))
  (adjust-x-offset! level)
  (render-tiles! renderer (get-resource '(:textures :outside) resources) (tiles level))
  (render-entity! renderer resources (player level)))

(defmethod level-step! (game (level level) dticks)
  (cond ((sdl2:keyboard-state-p :scancode-backspace)
         (setf (level game) (menu game)))
        ((sdl2:keyboard-state-p :scancode-escape)
         (setf (level game) (paused-menu game)))
        (t
         (dolist (entity (cons (player level) (entities level)))
           (entity-step! level entity (get-state entity) dticks)))))

(defun blank-tile? (level tile-y tile-x)
  (and (>= tile-x 0)
       (< tile-x (array-dimension (tiles level) 1))
       (>= tile-y 0)
       (< tile-y (array-dimension (tiles level) 0))
       (= 11 (aref (tiles level) tile-y tile-x 0))))
