;;; -*- Lisp -*-

(defsystem "tutorial"
  :depends-on ("png-read" "sdl2" "sdl2-image" "sdl2-ttf")
  :components ((:file "animation" :depends-on ("atlas"
                                               "macros"
                                               "package"
                                               "parameters"
                                               "resource"
                                               "texture"))
               (:file "atlas"    :depends-on ("macros"
                                              "package"
                                              "parameters"
                                              "texture"))
               (:file "entity"   :depends-on ("macros"
                                              "package"
                                              "parameters"))
               (:file "game" :depends-on ("animation"
                                          "macros"
                                          "package"
                                          "parameters"
                                          "resource"
                                          "texture"))
               (:file "level"    :depends-on ("game"
                                              "macros"
                                              "mode"
                                              "package"
                                              "parameters"
                                              "resource"
                                              "texture"
                                              "utilities"))
               (:file "macros"   :depends-on ("package"))
               (:file "mode"      :depends-on ("entity"
                                               "macros"
                                               "package"))
               (:file "package")
               (:file "parameters" :depends-on ("macros"
                                                "package"))
               (:file "player"   :depends-on ("entity"
                                              "macros"
                                              "package"
                                              "parameters"))
               (:file "resource" :depends-on ("package"))
               (:file "texture"  :depends-on ("package"
                                              "resource"))
               (:file "tutorial" :depends-on ("animation"
                                              "game"
                                              "level"
                                              "macros"
                                              "package"
                                              "parameters"
                                              "resource"
                                              "texture"
                                              "utilities"))
               (:file "utilities" :depends-on ("package"))))
