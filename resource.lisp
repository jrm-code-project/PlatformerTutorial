;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun resource-directory ()
  (asdf/system:system-relative-pathname "tutorial" "resources/"))

(defun resource-pathname (namestring)
  (merge-pathnames (parse-namestring namestring) (resource-directory)))

(defun text-font () (resource-pathname "Inconsolata-Regular.ttf"))

(defun call-with-open-font (font-path size receiver)
  (let ((font nil))
    (unwind-protect
         (progn (setq font (sdl2-ttf:open-font font-path size))
                (funcall receiver font))
      (when font
        (sdl2-ttf:close-font font)))))

(defmacro with-open-font ((font font-path size) &body body)
  `(CALL-WITH-OPEN-FONT ,font-path ,size
     (LAMBDA (,font)
       ,@body)))

(defun call-with-rendered-text (font text r g b a receiver)
  (let ((surface nil))
    (unwind-protect
         (progn (setq surface (sdl2-ttf:render-text-solid font text r g b a))
                (funcall receiver surface))
      (when surface
        ;(sdl2:free-surface surface)
        ))))

(defmacro with-rendered-text ((surface font text r g b a) &body body)
  `(CALL-WITH-RENDERED-TEXT ,font ,text ,r ,g ,b ,a
     (LAMBDA (,surface)
       ,@body)))

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
  (let-surfaces ((big-clouds-surface          (resource-pathname "big_clouds.png"))
                 (button-atlas-surface        (resource-pathname "button_atlas.png"))
                 (menu-surface                (resource-pathname "menu_background.png"))
                 (menu-background-surface     (resource-pathname "background_menu.png"))
                 (outside-sprites-surface     (resource-pathname "outside_sprites.png"))
                 (pause-menu-surface          (resource-pathname "pause_menu.png"))
                 (player-sprites-surface      (resource-pathname "player_sprites.png"))
                 (playing-background-surface  (resource-pathname "playing_bg_img.png"))
                 (small-clouds-surface        (resource-pathname "small_clouds.png"))
                 (sound-button-atlas-surface  (resource-pathname "sound_button.png"))
                 (urm-button-atlas-surface    (resource-pathname "urm_buttons.png"))
                 (volume-button-atlas-surface (resource-pathname "volume_buttons.png")))
    (funcall receiver
             `(:big-clouds          ,big-clouds-surface
               :button-atlas        ,button-atlas-surface
               :menu                ,menu-surface
               :menu-background     ,menu-background-surface
               :outside             ,outside-sprites-surface
               :pause-menu          ,pause-menu-surface
               :player              ,player-sprites-surface
               :playing-background  ,playing-background-surface
               :small-clouds        ,small-clouds-surface
               :sound-button-atlas  ,sound-button-atlas-surface
               :urm-button-atlas    ,urm-button-atlas-surface
               :volume-button-atlas ,volume-button-atlas-surface))))

(defmacro with-surfaces ((surfaces) &body body)
  `(CALL-WITH-SURFACES
    (LAMBDA (,surfaces)
      ,@body)))

