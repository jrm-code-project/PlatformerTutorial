;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass entity ()
  ((state :initarg :state :accessor get-state)
   (x :initarg :x :accessor get-x)
   (y :initarg :y :accessor get-y)

   (animation  :initarg :animation          :accessor get-animation)
   (animations :initarg :animations         :reader   animations)
   (flip       :initarg :flip :initform nil :accessor flip?)))

(defgeneric entity-step! (game entity state dticks))

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

