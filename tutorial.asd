;;; -*- Lisp -*-

(defsystem "tutorial"
  :depends-on ("sdl2")
  :components ((:file "package")
               (:file "tutorial" :depends-on ("package"))))

