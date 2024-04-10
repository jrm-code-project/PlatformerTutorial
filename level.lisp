;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled big-cloud-y-offset 204)
(defun-scaled small-cloud-minimum-y 80)
(defun-scaled small-cloud-maximum-y 175)

(defun-scaled health-bar-x 10)
(defun-scaled health-bar-y 10)
(defun-scaled health-offset-x 34)
(defun-scaled health-offset-y 14)
(defun-scaled health-bar-width 150)
(defun-scaled health-bar-height 4)

(defclass level (mode)
  ((player   :initarg :player   :accessor player)
   (tiles    :initarg :tiles    :accessor tiles)
   (cloud-heights :initform
                  (do ((l '() (cons (+ (small-cloud-minimum-y)
                                       (random (- (small-cloud-maximum-y)
                                                  (small-cloud-minimum-y))))
                                    l))
                       (i 0 (1+ i)))
                      ((>= i 10) l))
                  :reader get-cloud-heights)))

(defun render-big-clouds! (renderer big-cloud-texture)
  (dotimes (i 3)
    (sdl2:with-rects ((src 0 0 (sdl2:texture-width big-cloud-texture) (sdl2:texture-height big-cloud-texture))
                      (dst (- (* i (scale (sdl2:texture-width big-cloud-texture)))
                              (floor (* .3 *world-x-offset*)))
                           (big-cloud-y-offset)
                           (scale (sdl2:texture-width big-cloud-texture))
                           (scale (sdl2:texture-height big-cloud-texture))))
      (sdl2:render-copy renderer big-cloud-texture :source-rect src :dest-rect dst))))

(defun render-small-clouds! (renderer small-cloud-texture small-cloud-heights)
  (do ((h small-cloud-heights (cdr h))
       (i 0 (+ i 1)))
      ((null h))
    (sdl2:with-rects ((src 0 0 (sdl2:texture-width small-cloud-texture) (sdl2:texture-height small-cloud-texture))
                      (dst (floor (- (* i 4 (scale (sdl2:texture-width small-cloud-texture))) (* *world-x-offset* .7)))
                           (car h)
                           (scale (sdl2:texture-width small-cloud-texture))
                           (scale (sdl2:texture-height small-cloud-texture))))
      (sdl2:render-copy renderer small-cloud-texture :source-rect src :dest-rect dst))))

(defun render-level-background! (renderer level-background-texture)
  (sdl2:with-rects ((src 0 0
                         (sdl2:texture-width level-background-texture)
                         (sdl2:texture-height level-background-texture))
                    (dst 0 0 (game-width) (game-height)))
    (sdl2:render-copy renderer level-background-texture :source-rect src :dest-rect dst)))

(defun render-health-bar! (renderer health-bar-texture player)
  (sdl2:with-rects ((src 0 0 (sdl2:texture-width health-bar-texture) (sdl2:texture-height health-bar-texture))
                    (dst (health-bar-x)
                         (health-bar-y)
                         (scale (sdl2:texture-width health-bar-texture))
                         (scale (sdl2:texture-height health-bar-texture))))
    (sdl2:render-copy renderer health-bar-texture :source-rect src :dest-rect dst))
  (sdl2:with-rects ((src (+ (health-offset-x) (health-bar-x))
                         (+ (health-offset-y) (health-bar-y))
                         (floor (* (health-bar-width) (/ (get-health player) 100)))
                         (health-bar-height)))
    (sdl2:set-render-draw-color renderer #xff #x00 #x00 #xff)
    (sdl2:render-fill-rect renderer src)))

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

(defmethod render-mode! (renderer resources game (level level))
  (adjust-x-offset! level)
  (render-level-background! renderer (get-resource '(:textures :playing-background) resources))
  (render-big-clouds! renderer (get-resource '(:textures :big-clouds) resources))
  (render-small-clouds! renderer (get-resource '(:textures :small-clouds) resources) (get-cloud-heights level))
  (render-tiles! renderer (get-resource '(:textures :outside) resources) (tiles level))
  (render-health-bar! renderer (get-resource '(:textures :health-bar) resources) (player level))
  (call-next-method))

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
    ,(let ((player nil)
           (entities nil)
           (level-tiles (car (read-level-data))))
       (dotimes (i (level-tiles-width level-tiles))
         (dotimes (j (level-tiles-height level-tiles))
           (cond ((= 100 (aref level-tiles i j 1))
                  (setq player
                        (make-instance 'player
                                       :x (+ (/ (tile-size) 2) (* i (tile-size)))
                                       :y (- (* (+ j 1) (tile-size)) 1)
                                       :state :idle
                                       :animation (funcall (get-resource '(:animations :player :idle) resources))
                                       :animations (get-resource '(:animations :player) resources))))
                 ((zerop (aref level-tiles i j 1))
                  (push (make-instance 'crabby
                                       :x (+ (/ (tile-size) 2) (* i (tile-size)))
                                       :y (- (* (+ j 1) (tile-size)) 1)
                                       :state :idle
                                       :animation (funcall (get-resource '(:animations :crabby :idle) resources))
                                       :animations (get-resource '(:animations :crabby) resources))
                        entities))
                 (t nil))))
       (make-instance 'level
                      :tiles level-tiles
                      :player player
                      :entities (cons player entities)))
    ,@resources))
