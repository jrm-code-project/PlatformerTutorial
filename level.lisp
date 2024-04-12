;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass level ()
  ((player   :initarg :player   :accessor player)
   (tiles    :initarg :tiles    :accessor tiles)))

(defun level-tiles-width (level-tiles)
  (array-dimension level-tiles 0))

(defun level-tiles-height (level-tiles)
  (array-dimension level-tiles 1))

(defun level-width (level)
  (* (level-tiles-width (tiles level)) (tile-size)))

(defparameter *render-tile-outline* nil)

(defun render-tiles! (renderer outside-texture tiles)
  (dotimes (row (height-in-tiles))
    (dotimes (column (width-in-tiles))
      (let ((tile (aref tiles column row 0)))
        (multiple-value-bind (tile-source-row tile-source-column) (floor tile 12)
          (sdl2:with-rects ((src (* tile-source-column (base-tile-size))
                                 (* tile-source-row (base-tile-size))
                                 (base-tile-size)
                                 (base-tile-size))
                            (dst (* column (tile-size))
                                 (* row (tile-size))
                                 (tile-size)
                                 (tile-size)))
            (sdl2:render-copy renderer
                              outside-texture
                              :source-rect src
                              :dest-rect dst)
            (when *render-tile-outline*
              (sdl2:render-draw-rect renderer dst))))))))

(defun render-level! (renderer resources level)
  (render-tiles! renderer (get-resource '(:textures :outside) resources) (tiles level))
  (render-entity! renderer resources (player level)))

(defun make-level (resources)
  `(:level
    ,(let ((player (make-instance 'player
                                  :x (scale 100)
                                  :y (scale 150)
                                  :state :idle
                                  :animation (funcall (get-resource '(:animations :player :idle) resources))
                                  :animations (get-resource '(:animations :player) resources)))
           (level-tiles (car (read-level-data))))
       (make-instance 'level
                      :tiles level-tiles
                      :player player))
    ,@resources))
