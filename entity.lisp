;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass entity ()
  ((state :initarg :state :accessor get-state)
   (x :initarg :x :accessor get-x)
   (y :initarg :y :accessor get-y)

   (animation  :initarg :animation          :accessor get-animation)
   (frame-sets :initarg :frame-sets         :reader   frame-sets)
   (flip       :initarg :flip :initform nil :accessor flip?)))

(defgeneric entity-step! (game entity state dticks)
  (:method (game (entity entity) state dticks)
    nil))

(defgeneric render-entity! (renderer resources entity)
  (:method (renderer resources (entity entity))
    (render-animation! renderer (getf resources :textures) (get-animation entity)
                       (floor (get-x entity))
                       (floor (get-y entity)) :flip? (flip? entity))))

(defclass hitbox ()
  ((width  :initarg :width  :reader get-width)
   (height :initarg :height :reader get-height)))

(defparameter *render-hitbox* nil)

(defmethod render-entity! :after (renderer resources (entity hitbox))
  (when *render-hitbox*
    (sdl2:set-render-draw-color renderer #xff #x00 #x00 #xFF)
    (sdl2:with-rects ((r (floor (- (get-x entity) (/ (get-width entity) 2)))
                         (floor (- (get-y entity) (get-height entity)))
                         (get-width entity)
                         (get-height entity)))
      (sdl2:render-draw-rect renderer r))))
