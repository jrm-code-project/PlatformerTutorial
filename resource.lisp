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

