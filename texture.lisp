;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun call-with-surface-texture (renderer func surface)
  (let ((texture nil))
    (unwind-protect
         (progn
           (setq texture (sdl2:create-texture-from-surface renderer surface))
           (funcall func texture))
      (when texture (sdl2:destroy-texture texture)))))

(defun call-with-surface-textures (renderer func &rest surfaces)
  (if (null surfaces)
      (funcall func)
      (call-with-surface-texture
       renderer
       (lambda (texture)
         (apply
          #'call-with-surface-textures
          renderer
          (lambda (&rest textures)
            (apply func (cons texture textures)))
          (cdr surfaces)))
       (car surfaces))))

(defmacro let-texture ((renderer &rest bindings) &body body)
  `(CALL-WITH-SURFACE-TEXTURES
    ,renderer
    (LAMBDA ,(map 'list #'car bindings)
      ,@body)
    ,@(map 'list (lambda (binding)
                   `(PROGN ,@(cdr binding)))
           bindings)))

(defun call-with-textures (surfaces renderer receiver)
  (let-texture (renderer
                  (player-sprites-texture (getf surfaces :player))
                  (outside-sprites-texture (getf surfaces :outside)))
    (funcall receiver
             `(:player ,player-sprites-texture
               :outside ,outside-sprites-texture))))

(defmacro with-textures ((textures surfaces renderer) &body body)
  `(CALL-WITH-TEXTURES
    ,surfaces ,renderer
    (LAMBDA (,textures)
      ,@body)))
