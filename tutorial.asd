;;; -*- Lisp -*-

(defsystem "tutorial"
  :depends-on ("sdl2" "sdl2-image")
  :components ((:file "animation" :depends-on ("package"
                                               "parameters"
                                               "macros"
                                               "resource"
                                               "texture"
                                               "sprites"))
               (:file "game" :depends-on ("animation"
                                          "package"
                                          "parameters"
                                          "macros"
                                          "resource"
                                          "texture"))
               (:file "macros"   :depends-on ("package"))
               (:file "package")
               (:file "parameters" :depends-on ("package"
                                                "macros"))
               (:file "resource" :depends-on ("package"))
               (:file "sprites"  :depends-on ("package"
                                              "parameters"
                                              "macros"
                                              "texture"))
               (:file "texture"  :depends-on ("package"
                                              "resource"))
               (:file "tutorial" :depends-on ("animation"
                                              "game"
                                              "package"
                                              "parameters"
                                              "macros"
                                              "resource"
                                              "texture"
                                              "utilities"))
               (:file "utilities" :depends-on ("package"))))
