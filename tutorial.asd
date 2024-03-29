;;; -*- Lisp -*-

(defsystem "tutorial"
  :depends-on ("sdl2" "sdl2-image")
  :components ((:file "game" :depends-on ("package"
                                          "resource"
                                          "texture"))
               (:file "package")
               (:file "resource" :depends-on ("package"))
               (:file "texture"  :depends-on ("package"
                                              "resource"))
               (:file "tutorial" :depends-on ("game"
                                              "package"
                                              "resource"
                                              "texture"))))
