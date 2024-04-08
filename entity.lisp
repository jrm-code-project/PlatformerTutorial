;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass entity ()
  ((state :initarg :state :accessor get-state)
   (x :initarg :x :accessor get-x)
   (y :initarg :y :accessor get-y)

   (animation  :initarg :animation          :accessor get-animation)
   (animations :initarg :animations         :reader   animations)
   (flip       :initarg :flip :initform nil :accessor flip?)))

(defgeneric entity-step! (level entity state dticks)
  (:method (level (entity entity) state dticks)
    nil))

(defun start-animation! (entity animation)
  (setf (get-animation entity) (funcall (getf (animations entity) animation))))

(defgeneric render-entity! (renderer resources entity)
  (:method (renderer resources (entity entity))
    (render-animation! renderer (getf resources :textures) (get-animation entity)
                       (- (floor (get-x entity)) *world-x-offset*)
                       (floor (get-y entity)) :flip? (flip? entity))))

(defclass hitbox ()
  ((width  :initarg :width  :reader get-width)
   (height :initarg :height :reader get-height)))

(defun get-left (entity)
  (floor (- (get-x entity) (/ (get-width entity) 2))))

(defun get-right (entity)
  (floor (+ (get-x entity) (/ (get-width entity) 2))))

(defun get-top (entity)
  (floor (- (get-y entity) (get-height entity))))

(defun get-bottom (entity)
  (floor (get-y entity)))

(defun entity-under-point? (entity x y)
  (and (>= x (get-left entity))
       (<= x (get-right entity))
       (>= y (get-top entity))
       (<= y (get-bottom entity))))

(defparameter *render-hitbox* nil)

(defmethod render-entity! :after (renderer resources (entity hitbox))
  (when *render-hitbox*
    (sdl2:set-render-draw-color renderer #xff #x00 #x00 #xFF)
    (sdl2:with-rects ((r (- (get-left entity) *world-x-offset*)
                         (get-top entity)
                         (get-width entity)
                         (get-height entity)))
      (sdl2:render-draw-rect renderer r))))

(defun coord->tile (x y)
  (values (floor x (tile-size)) (floor y (tile-size))))

(defun point-supported? (level x y)
  (multiple-value-bind (tile-x tile-y) (coord->tile x (+ y 1))
    (not (blank-tile? level tile-x tile-y))))

(defun entity-supported? (level entity)
  (or (point-supported? level (get-left entity) (get-bottom entity))
      (point-supported? level (get-right entity) (get-bottom entity))))

(defun point-covered? (level x y)
  (multiple-value-bind (tile-x tile-y) (coord->tile x (- y 1))
    (not (blank-tile? level tile-x tile-y))))

(defun entity-covered? (level entity)
  (or (point-covered? level (get-left entity) (get-top entity))
      (point-covered? level (get-left entity) (get-top entity))))

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

(defun move-entity-left! (level entity dx)
  (let ((x1 (move-point-left level
                              (get-left entity)
                              (get-top entity)
                              dx))
        (x2 (move-point-left level
                              (get-left entity)
                              (get-bottom entity)
                              dx)))
    (setf (get-x entity) (+ (max x1 x2) (floor (get-width entity) 2)))))

(defun move-entity-right! (level entity dx)
  (let ((x1 (move-point-right level
                               (get-right entity)
                               (get-top entity)
                               dx))
        (x2 (move-point-right level
                               (get-right entity)
                               (get-bottom entity)
                               dx)))
    (setf (get-x entity) (- (min x1 x2) (floor (get-width entity) 2)))))

(defun move-entity-horizontally! (level entity dx)
  (cond ((< dx 0) (move-entity-left! level entity dx))
        ((> dx 0) (move-entity-right! level entity dx))
        (t nil)))

(defun move-entity-up! (level entity dy)
  (let ((y1 (move-point-up level
                           (get-left entity)
                           (get-top entity)
                           dy))
        (y2 (move-point-up level
                           (get-right entity)
                           (get-top entity)
                           dy)))
    (setf (get-y entity) (+ (max y1 y2) (get-height entity)))))

(defun move-entity-down! (level entity dy)
  (let ((y1 (move-point-down level
                             (get-left entity)
                             (get-bottom entity)
                             dy))
        (y2 (move-point-down level
                             (get-right entity)
                             (get-bottom entity)
                             dy)))
    (setf (get-y entity) (min y1 y2))))

(defun move-entity-vertically! (level entity dy)
  (cond ((< dy 0) (move-entity-up! level entity dy))
        ((> dy 0) (move-entity-down! level entity dy))
        (t nil)))
