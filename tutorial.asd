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
               (:file "button"   :depends-on ("animation"
                                              "entity"
                                              "macros"
                                              "package"
                                              "parameters"))
               (:file "cannon"   :depends-on ("entity"
                                              "macros"
                                              "package"
                                              "parameters"))
               (:file "container" :depends-on ("entity"
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
               (:file "game-over" :depends-on ("level"
                                               "macros"
                                               "mode"
                                               "package"))
               (:file "level"    :depends-on ("game"
                                              "macros"
                                              "mode"
                                              "package"
                                              "parameters"
                                              "resource"
                                              "texture"
                                              "utilities"))
               (:file "level-complete" :depends-on ("game"
                                                    "macros"
                                                    "mode"
                                                    "package"
                                                    "parameters"
                                                    "utilities"))
               (:file "macros"   :depends-on ("package"))
               (:file "menu"     :depends-on ("game"
                                              "macros"
                                              "mode"
                                              "package"
                                              "parameters"
                                              "resource"
                                              "texture"
                                              "utilities"))
               (:file "mode"      :depends-on ("entity"
                                               "macros"
                                               "package"))
               (:file "package")
               (:file "parameters" :depends-on ("macros"
                                                "package"))
               (:file "pause-menu" :depends-on ("animation"
                                                "entity"
                                                "level"
                                                "macros"
                                                "mode"
                                                "package"))
               (:file "player"   :depends-on ("entity"
                                              "macros"
                                              "package"
                                              "parameters"))
               (:file "potion"   :depends-on ("entity"
                                              "macros"
                                              "package"
                                              "parameters"))
               (:file "resource" :depends-on ("package"))
               (:file "spikes"   :depends-on ("entity"
                                              "macros"
                                              "package"
                                              "parameters"))
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
