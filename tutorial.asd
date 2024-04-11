;;; -*- Lisp -*-

(defsystem "tutorial"
  :depends-on ("sdl2" "sdl2-image")
  :components ((:file "animation" :depends-on ("atlas"
                                               "macros"
                                               "package"
                                               "parameters"
                                               "resource"
                                               "texture"
                                               ))
               (:file "atlas"  :depends-on ("macros"
                                            "package"
                                            "parameters"
                                            "texture"))
               (:file "game" :depends-on ("animation"
                                          "macros"
                                          "package"
                                          "parameters"
                                          "resource"
                                          "texture"))
               (:file "macros"   :depends-on ("package"))
               (:file "package")
               (:file "parameters" :depends-on ("macros"
                                                "package"))
               (:file "resource" :depends-on ("package"))
               
               (:file "texture"  :depends-on ("package"
                                              "resource"))
               (:file "tutorial" :depends-on ("animation"
                                              "game"
                                              "macros"
                                              "package"
                                              "parameters"
                                              "resource"
                                              "texture"
                                              "utilities"))
               (:file "utilities" :depends-on ("package"))))
