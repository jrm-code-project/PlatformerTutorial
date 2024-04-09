;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass mode ()
  ((entities :initarg :entities :reader entities)))

(defgeneric render-mode! (renderer resources game mode)
  (:method (renderer resources game (mode mode))
    (dolist (entity (entities mode))
      (when (get-state entity)
        (render-entity! renderer resources entity)))))

(defgeneric mode-step! (game mode dticks)
  (:method (game (mode mode) dticks)
    (dolist (entity (entities mode))
      (when (get-state entity)
        (entity-step! mode entity (get-state entity) dticks)))))
