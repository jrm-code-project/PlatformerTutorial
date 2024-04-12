;;; -*- Lisp -*-

(in-package "TUTORIAL")

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
                       (floor (get-x entity))
                       (floor (get-y entity)) :flip? (flip? entity))))

(defun start-animation! (entity animation)
  (setf (get-animation entity) (funcall (getf (animations entity) animation))))

;;;;;;;;;;;;;;;;;
;;; Hitbox Mix-in
;;;   Gives a width and height to an entity

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
    (sdl2:with-rects ((r (floor (get-left entity))
                         (floor (get-top entity))
                         (get-width entity)
                         (get-height entity)))
      (sdl2:render-draw-rect renderer r))))

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

