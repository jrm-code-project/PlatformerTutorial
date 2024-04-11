;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun gravity () (scalef .001))

(defclass entity ()
  ((state :initarg :state :accessor get-state)
   (x :initarg :x :accessor get-x)
   (y :initarg :y :accessor get-y)

   (animation  :initarg :animation          :accessor get-animation)
   (animations :initarg :animations         :reader   animations)
   (flip       :initarg :flip :initform nil :accessor flip?)))

(defgeneric entity-step! (game level entity state dticks))

(defgeneric render-entity! (renderer resources entity)
  (:method (renderer resources (entity entity))
    (render-animation! renderer (getf resources :textures) (get-animation entity)
                       (- (floor (get-x entity)) *world-x-offset*)
                       (floor (get-y entity)) :flip? (flip? entity))))

(defun start-animation! (entity animation)
  (setf (get-animation entity) (funcall (getf (animations entity) animation))))

(defgeneric enemy? (entity)
  (:method ((entity entity)) nil))

;;;;;;;;;;;;;;;;;
;;; Health Mix-in
;;;   :initial-health :initarg
;;;   10 health lost when hit
;;;   :hit animation started when hit

(defclass health ()
  ((health :initarg :initial-health :accessor get-health)))

(defgeneric hit! (entity)
  (:method ((entity health))
    (decf (get-health entity) 10)
    (if (< (get-health entity) 0)
        (setf (get-health entity) 0))
    (setf (get-state entity) :hit)))

;;;;;;;;;;;;;;;;;
;;; Hitbox Mix-in
;;;   Gives a width and height to an entity
;;;   collision detection with level

(defclass hitbox ()
  ((width  :initarg :width  :reader get-width)
   (height :initarg :height :reader get-height)))

(defun get-left (entity)
  (- (get-x entity) (/ (get-width entity) 2)))

(defun get-right (entity)
  (+ (get-x entity) (/ (get-width entity) 2)))

(defun get-top (entity)
  (- (get-y entity) (get-height entity)))

(defun get-bottom (entity)
  (get-y entity))

(defparameter *render-hitbox* nil)

(defmethod render-entity! :after (renderer resources (entity hitbox))
  (when *render-hitbox*
    (sdl2:set-render-draw-color renderer #xff #x00 #x00 #xFF)
    (sdl2:with-rects ((r (floor (- (get-left entity) *world-x-offset*))
                         (floor (get-top entity))
                         (get-width entity)
                         (get-height entity)))
      (sdl2:render-draw-rect renderer r))))

(defun entity-under-point? (entity x y)
  (and (>= x (get-left entity))
       (<= x (get-right entity))
       (>= y (get-top entity))
       (<= y (get-bottom entity))))

(defun entity-supported? (level entity)
  (or (point-supported? level (get-left entity) (get-bottom entity))
      (point-supported? level (get-right entity) (get-bottom entity))))

(defun entity-covered? (level entity)
  (or (point-covered? level (get-left entity) (get-top entity))
      (point-covered? level (get-right entity) (get-top entity))))

(defun against-left-wall? (level entity)
  (or (point-against-left-wall? level (get-left entity) (get-top entity))
      (point-against-left-wall? level (get-left entity) (get-bottom entity))))

(defun against-right-wall? (level entity)
  (or (point-against-right-wall? level (get-right entity) (get-top entity))
      (point-against-right-wall? level (get-right entity) (get-bottom entity))))

(defun unsupported-on-left? (level entity)
  (not (point-supported? level (get-left entity) (get-bottom entity))))

(defun unsupported-on-right? (level entity)
  (not (point-supported? level (get-right entity) (get-bottom entity))))

(defun move-entity-left! (level entity dx)
  (let ((x1 (move-point-left level
                              (get-left entity)
                              (get-top entity)
                              dx))
        (x2 (move-point-left level
                              (get-left entity)
                              (get-bottom entity)
                              dx)))
    (setf (get-x entity) (+ (max x1 x2) (/ (get-width entity) 2)))))

(defun move-entity-right! (level entity dx)
  (let ((x1 (move-point-right level
                               (get-right entity)
                               (get-top entity)
                               dx))
        (x2 (move-point-right level
                               (get-right entity)
                               (get-bottom entity)
                               dx)))
    (setf (get-x entity) (- (min x1 x2) (/ (get-width entity) 2)))))

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

(defun same-level? (entity-x entity-y)
  (< (abs (- (get-y entity-x) (get-y entity-y))) 5))

(defun within-five-tiles? (entity-x entity-y)
  (< (abs (- (get-x entity-x) (get-x entity-y))) (* 5 (tile-size))))

;;;;;;;;;;;;;;;;;;;;
;;; Attackbox Mix-in
;;;   can-attack? returns true if attacker's attackbox overlaps defenders hitbox
(defclass attackbox ()
  ((attackbox-width    :initarg :attackbox-width    :reader get-attackbox-width)
   (attackbox-height   :initarg :attackbox-height   :reader get-attackbox-height)
   (attackbox-x-offset :initarg :attackbox-x-offset :reader get-attackbox-x-offset)
   (attackbox-y-offset :initarg :attackbox-y-offset :reader get-attackbox-y-offset)))

(defun attack-left (entity)
  (if (flip? entity)
      (- (get-x entity)
         (get-attackbox-x-offset entity)
         (get-attackbox-width entity))
      (+ (get-x entity)
         (get-attackbox-x-offset entity))))

(defun attack-right (entity)
  (if (flip? entity)
      (- (get-x entity)
         (get-attackbox-x-offset entity))
      (+ (get-x entity)
            (get-attackbox-x-offset entity)
            (get-attackbox-width entity))))

(defun attack-top (entity)
  (- (get-y entity)
     (get-attackbox-y-offset entity)
     (get-attackbox-height entity)))

(defun attack-bottom (entity)
  (- (get-y entity)
     (get-attackbox-y-offset entity)))

(defparameter *render-attackbox* nil)

(defmethod render-entity! :after (renderer resources (entity attackbox))
  (when *render-attackbox*
    (sdl2:set-render-draw-color renderer #x00 #x00 #xFF #xFF)
    (sdl2:with-rects ((r (floor (- (attack-left entity) *world-x-offset*))
                         (floor (attack-top entity))
                         (get-attackbox-width entity)
                         (get-attackbox-height entity)))
      (sdl2:render-draw-rect renderer r))))

(defun can-attack? (attacker defender)
  (not (or (< (attack-right attacker) (get-left defender))
           (> (attack-left attacker) (get-right defender))
           (< (attack-bottom attacker) (get-top defender))
           (> (attack-top attacker) (get-bottom defender)))))
