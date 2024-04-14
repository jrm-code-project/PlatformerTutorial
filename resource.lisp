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
  (let-surfaces ((outside-sprites-surface     (resource-pathname "outside_sprites.png"))
                 (player-sprites-surface      (resource-pathname "player_sprites.png")))
    (with-open-font (font (text-font) 80)
      (with-rendered-text (menu-surface font "Menu" #x00 #x00 #x00 #xff)
        (funcall receiver
                 `(:menu                ,menu-surface
                   :outside             ,outside-sprites-surface
                   :player              ,player-sprites-surface))))))

(defmacro with-surfaces ((surfaces) &body body)
  `(CALL-WITH-SURFACES
    (LAMBDA (,surfaces)
      ,@body)))


