;;; -*- Lisp -*-

(defsystem "tutorial"
  :depends-on ("png-read" "sdl2" "sdl2-image" "sdl2-ttf")
  :components ((:file "animation" :depends-on ("macros"
                                               "package"
                                               "parameters"
                                               "resource"
                                               "texture"
                                               "sprites"))
               (:file "button"   :depends-on ("animation"
                                              "entity"
                                              "macros"
                                              "package"
                                              "parameters"))
               (:file "crabby"   :depends-on ("enemy"
                                              "entity"
                                              "macros"
                                              "package"
                                              "parameters"))
               (:file "enemy"    :depends-on ("entity"
                                              "macros"
                                              "package"
                                              "parameters"))
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
                                              "package"
                                              "parameters"
                                              "resource"
                                              "texture"
                                              "utilities"))
               (:file "macros"   :depends-on ("package"))
               (:file "menu"     :depends-on ("game"
                                              "macros"
                                              "package"
                                              "parameters"
                                              "resource"
                                              "texture"
                                              "utilities"))
               (:file "package")
               (:file "parameters" :depends-on ("macros"
                                                "package"))
               (:file "pause-menu" :depends-on ("animation"
                                                "entity"
                                                "level"
                                                "macros"
                                                "package"))
               (:file "player"   :depends-on ("entity"
                                              "macros"
                                              "package"
                                              "parameters"))
               (:file "resource" :depends-on ("package"))
               (:file "sprites"  :depends-on ("macros"
                                              "package"
                                              "parameters"
                                              "texture"))
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
