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
  ((player   :initarg :player   :reader player)
   (tiles    :initarg :tiles    :reader tiles)
   (x-offset :initform 0        :accessor x-offset)
   (cloud-heights :initform
                  (do ((l '() (cons (+ (small-cloud-minimum-y)
                                       (random (- (small-cloud-maximum-y)
                                                  (small-cloud-minimum-y))))
                                    l))
                       (i 0 (1+ i)))
                      ((>= i 10) l))
                  :reader get-cloud-heights)
   (restart-level :initarg :restart-level :reader restart-level)
   (next-level    :initarg :next-level    :reader next-level)))

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
    (render-level-background! renderer (get-resource '(:textures :playing-background) resources))
    (render-big-clouds! renderer (get-resource '(:textures :big-clouds) resources))
    (render-small-clouds! renderer (get-resource '(:textures :small-clouds) resources) (get-cloud-heights level))
    (render-tiles! renderer (get-resource '(:textures :outside) resources) (tiles level))
    (render-health-bar! renderer (get-resource '(:textures :health-bar) resources) (player level))
    (call-next-method)))

(defmethod mode-step! (game (level level) dticks)
  (cond ((sdl2:keyboard-state-p :scancode-backspace)
         (setf (mode game) (menu game)))
        ((sdl2:keyboard-state-p :scancode-escape)
         (setf (mode game) (paused-menu game)))
        ((find-if (lambda (entity)
                    (and (enemy? entity)
                         (get-state entity)))
                  (entities level))
         (call-next-method))
        ((next-level (level game))
         (setf (mode game) (level-complete game)))
        (t
         (setf (mode game) (game-over game)))))

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

(defun make-level (resources next-level tiles)
  (let ((player nil)
        (entities nil))
    (dotimes (i (level-tiles-width tiles))
      (dotimes (j (level-tiles-height tiles))
        (cond ((= 100 (aref tiles i j 1))
               (setq player
                     (make-instance 'player
                                    :x (+ (/ (tile-size) 2) (* i (tile-size)))
                                    :y (- (* (+ j 1) (tile-size)) 1)
                                    :state :idle
                                    :animation (funcall (get-resource '(:animations :player :idle) resources))
                                    :animations (get-resource '(:animations :player) resources))))
              ((zerop (aref tiles i j 1))
               (push (make-instance 'crabby
                                    :x (+ (/ (tile-size) 2) (* i (tile-size)))
                                    :y (- (* (+ j 1) (tile-size)) 1)
                                    :state :idle
                                    :animation (funcall (get-resource '(:animations :crabby :idle) resources))
                                    :animations (get-resource '(:animations :crabby) resources))
                     entities))
              ((= (aref tiles i j 2) 0)
               (let* ((potion (make-instance 'blue-potion
                                    :x (+ (/ (tile-size) 2) (* i (tile-size)))
                                    :y (- (* (+ j 1) (tile-size)) 1 (scale 5))
                                    :state nil
                                    :animation (funcall (get-resource '(:animations :potions :blue) resources))))
                      (box (make-instance 'box
                                    :x (+ (/ (tile-size) 2) (* i (tile-size)))
                                    :y (- (* (+ j 1) (tile-size)) 1)
                                    :state :idle
                                    :contents potion
                                    :animation (funcall (get-resource '(:animations :container :box) resources))
                                    :animations (get-resource '(:animations :container) resources))))
                 (push potion entities)
                 (push box entities)))
              ((= (aref tiles i j 2) 1)
               (let* ((potion (make-instance
                               'red-potion
                               :x (+ (/ (tile-size) 2) (* i (tile-size)))
                               :y (- (* (+ j 1) (tile-size)) 1 (scale 5))
                               :state nil
                               :animation (funcall (get-resource '(:animations :potions :red) resources))))
                     (barrel (make-instance 'barrel
                                    :x (+ (/ (tile-size) 2) (* i (tile-size)))
                                    :y (- (* (+ j 1) (tile-size)) 1)
                                    :contents potion
                                    :state :idle
                                    :animation (funcall (get-resource '(:animations :container :barrel) resources))
                                    :animations (get-resource '(:animations :container) resources))))
                 (push potion entities)
                 (push barrel entities)))
              ((= (aref tiles i j 2) 2)
               (push (make-instance 'blue-potion
                                    :x (+ (/ (tile-size) 2) (* i (tile-size)))
                                    :y (- (* (+ j 1) (tile-size)) 1 (scale 5))
                                    :state :idle
                                    :animation (funcall (get-resource '(:animations :potions :blue) resources)))
                     entities))
              ((= (aref tiles i j 2) 3)
               (push (make-instance 'red-potion
                                    :x (+ (/ (tile-size) 2) (* i (tile-size)))
                                    :y (- (* (+ j 1) (tile-size)) 1 (scale 5))
                                    :state :idle
                                    :animation (funcall (get-resource '(:animations :potions :red) resources)))
                     entities))
              (t nil))))
    (make-instance 'level
                   :tiles tiles
                   :player player
                   :entities (reverse (cons player entities))
                   :restart-level (lambda ()
                                    (make-level resources next-level tiles))
                   :next-level next-level)))

(defun make-levels (resources)
  `(:first-level
    ,(fold-left (lambda (next-level tile-map)
                  (make-level resources next-level tile-map))
                nil
                (reverse (read-level-data)))
    ,@resources))
