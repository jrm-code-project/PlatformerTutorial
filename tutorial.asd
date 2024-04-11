;;; -*- Lisp -*-

(defsystem "tutorial"
  :depends-on ("sdl2" "sdl2-image")
  :components ((:file "atlas"    :depends-on ("package"
                                              "parameters"
                                              "macros"
                                              "texture"))
               (:file "game"     :depends-on ("package"
                                              "parameters"
                                              "macros"
                                              "resource"
                                              "texture"))
               (:file "macros"   :depends-on ("package"))
               (:file "package")
               (:file "parameters" :depends-on ("macros"
                                                "package"))
               (:file "resource" :depends-on ("package"))
               (:file "texture"  :depends-on ("package"
                                              "resource"))
               (:file "tutorial" :depends-on ("game"
                                              "macros"
                                              "package"
                                              "parameters"
                                              "resource"
                                              "texture"))))
