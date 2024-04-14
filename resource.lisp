;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun resource-directory ()
  (asdf/system:system-relative-pathname "tutorial" "resources/"))

(defun resource-pathname (namestring)
  (merge-pathnames (parse-namestring namestring) (resource-directory)))

(defun text-font () (resource-pathname "Inconsolata-Regular.ttf"))

(defun level-files ()
  (sort
   (directory (merge-pathnames (make-pathname :directory '(:relative "levels")
                                              :name :wild
                                              :type "png")
                               (resource-directory)))
   #'string-lessp
   :key #'pathname-name))

(defun read-level-data ()
  (map 'list (lambda (level-file)
               (png-read:image-data
                (png-read:read-png-file level-file)))
       (level-files)))

(defun call-with-image-surface (func pathname)
  (let ((surface nil))
    (unwind-protect
         (progn
           (setq surface (sdl2-image:load-image pathname))
           (funcall func surface))
      (when surface (sdl2:free-surface surface)))))

(defun call-with-image-surfaces (func &rest pathnames)
  (if (null pathnames)
      (funcall func)
      (call-with-image-surface
       (lambda (surface)
         (apply #'call-with-image-surfaces
          (lambda (&rest surfaces)
            (apply func (cons surface surfaces)))
          (cdr pathnames)))
       (car pathnames))))

(defmacro let-surfaces (bindings &body body)
  `(CALL-WITH-IMAGE-SURFACES
    (LAMBDA ,(map 'list #'car bindings)
      ,@body)
    ,@(map 'list (lambda (binding)
                   `(PROGN ,@(cdr binding)))
           bindings)))

(defun call-with-surfaces (receiver)
  (let-surfaces ((button-atlas-surface        (resource-pathname "button_atlas.png"))
                 (menu-surface                (resource-pathname "menu_background.png"))
                 (menu-background-surface     (resource-pathname "background_menu.png"))
                 (outside-sprites-surface     (resource-pathname "outside_sprites.png"))
                 (player-sprites-surface      (resource-pathname "player_sprites.png")))
    (funcall receiver
             `(:button-atlas        ,button-atlas-surface
               :menu                ,menu-surface
               :menu-background     ,menu-background-surface
               :outside             ,outside-sprites-surface
               :player              ,player-sprites-surface))))

(defmacro with-surfaces ((surfaces) &body body)
  `(CALL-WITH-SURFACES
    (LAMBDA (,surfaces)
      ,@body)))
