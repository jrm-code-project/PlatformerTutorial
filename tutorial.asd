;;; -*- Lisp -*-

(defsystem "tutorial"
  :depends-on ("sdl2")
  :components ((:file "game" :depends-on ("package"))
               (:file "package")
               (:file "tutorial" :depends-on ("game"
                                              "package"))))

