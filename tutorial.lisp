;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun call-with-open-font (font-path size receiver)
  (let ((font nil))
    (unwind-protect
         (progn (setq font (sdl2-ttf:open-font font-path size))
                (funcall receiver font))
      (when font
        (sdl2-ttf:close-font font)))))

(defmacro with-open-font ((font font-path size) &body body)
  `(CALL-WITH-OPEN-FONT ,font-path ,size
     (LAMBDA (,font)
       ,@body)))

(defun call-with-rendered-text (font text r g b a receiver)
  (let ((surface nil))
    (unwind-protect
         (progn (setq surface (sdl2-ttf:render-text-solid font text r g b a))
                (funcall receiver surface))
      (when surface
        ;(sdl2:free-surface surface)
        ))))

(defmacro with-rendered-text ((surface font text r g b a) &body body)
  `(CALL-WITH-RENDERED-TEXT ,font ,text ,r ,g ,b ,a
     (LAMBDA (,surface)
       ,@body)))

(defun call-with-sdl2-images (formats thunk)
  "Initialize the SDL2 image library with the given FORMATS and call THUNK."
  (unwind-protect
       (progn (sdl2-image:init formats)
              (funcall thunk))
    (sdl2-image:quit)))

(defmacro with-sdl2-images (image-formats &body body)
  `(CALL-WITH-SDL2-IMAGES ',image-formats
     (LAMBDA ()
       ,@body)))

(defconstant +frames-per-second+ 60)
(defconstant +ticks-per-frame+ (/ +ticks-per-second+ +frames-per-second+))
(defconstant +titles-per-second+ 1)
(defconstant +ticks-per-title+ (/ +ticks-per-second+ +titles-per-second+))

(defun main-event-loop (game window renderer resources)
  (let ((start-tick (sdl2:get-ticks))
        (last-frame 0)
        (last-title 0)
        (frame-count 0))

    (sdl2:with-event-loop (:method :poll)

      (:idle ()
             (let* ((tick (- (sdl2:get-ticks) start-tick))
                    (this-frame (floor tick +ticks-per-frame+))
                    (this-title (floor tick +ticks-per-title+)))
               (cond ((> this-frame last-frame)
                      (render-game! renderer game resources)
                      (setq last-frame this-frame)
                      (incf frame-count))
                     ((> this-title last-title)
                      (sdl2:set-window-title window
                                             (format nil "Platformer, frame rate: ~d, game rate: ~d"
                                                     frame-count
                                                     (car (steps game))))
                      (setq last-title this-title)
                      (setq frame-count 0)
                      (sb-ext:atomic-decf (car (steps game)) (car (steps game))))
                     (t (sleep 0.002)))))

      (:keydown (:keysym keysym)
                (cond ((or (eql (sdl2:scancode keysym) :scancode-escape)
                           (eql (sdl2:scancode keysym) :scancode-x))
                       (sdl2:push-quit-event))
                      ;; suppress output for these known keys
                      ((member (sdl2:scancode keysym) '(:scancode-backspace
                                                        :scancode-left
                                                        :scancode-return
                                                        :scancode-right
                                                        :scancode-up))
                       nil)
                      (t
                       (format t "~&Keydown: ~s~%" (sdl2:scancode keysym))
                       (force-output))))

      (:quit () t)
      )))

(defun main-window (game surfaces)
  (sdl2:with-window (window
                     :w (game-width)
                     :h (game-height)
                     :flags '(:shown))
    (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
      (with-resources ((resources) game surfaces renderer)
        (initialize-game! game resources)
        (with-game-loop (game)
          (main-event-loop game window renderer resources))))))

(defun run (game)
  (sdl2-ttf:init)
  (with-sdl2-images (:png)
    (with-surfaces (surfaces game)
      (sdl2:with-init (:video)
        (main-window game surfaces)))))

(defclass platformer (game)
  ())

(defmethod call-with-surfaces ((game platformer) receiver)
  (let-surfaces ((button-atlas-surface    (resource-pathname "button_atlas.png"))
                 (menu-surface            (resource-pathname "menu_background.png"))
                 (outside-sprites-surface (resource-pathname "outside_sprites.png"))
                 (player-sprites-surface  (resource-pathname "player_sprites.png")))
    (funcall receiver
             `(:button-atlas ,button-atlas-surface
               :menu         ,menu-surface
               :outside      ,outside-sprites-surface
               :player       ,player-sprites-surface))))

(defmethod call-with-resources ((game platformer) surfaces renderer receiver)
  (flet ((make-animations (resources)
           `(:animations
             (:button
              ,(let ((button-atlas
                       (make-sprite-sheet (lambda (textures) (getf textures :button-atlas))
                                         (getf resources :textures)
                                         #(:play
                                           :options
                                           :quit)
                                         #(3 3 3))))
                 `(:play
                   ,(let ((frame-set (make-instance 'frame-set
                                                    :sprite-sheet button-atlas
                                                    :row :play
                                                    :ticks-per-frame most-positive-fixnum)))
                      (lambda ()
                        (make-instance 'slides
                                       :frame-set frame-set
                                       :current-slide :idle
                                       :slides #(:idle :hover :pressed))))
                   :options
                   ,(let ((frame-set (make-instance 'frame-set
                                                    :sprite-sheet button-atlas
                                                    :row :options
                                                    :ticks-per-frame most-positive-fixnum)))
                      (lambda ()
                        (make-instance 'slides
                                       :frame-set frame-set
                                       :current-slide :idle
                                       :slides #(:idle :hover :pressed))))
                   :quit
                   ,(let ((frame-set (make-instance 'frame-set
                                                    :sprite-sheet button-atlas
                                                    :row :quit
                                                    :ticks-per-frame most-positive-fixnum)))
                      (lambda ()
                        (make-instance 'slides
                                       :frame-set frame-set
                                       :current-slide :idle
                                       :slides #(:idle :hover :pressed))))))
              :player
              ,(let ((player-sprite-sheet
                      (make-sprite-sheet (lambda (textures) (getf textures :player))
                                         (getf resources :textures)
                                         #(:idle
                                           :running
                                           :jumping
                                           :falling
                                           :landing
                                           :hit
                                           :attack1
                                           :attack2
                                           :attack3)
                                         #(5 6 3 1 2 4 3 3 3)
                                         :baseline-offset 9)))
                `(:falling
                  ,(let ((frame-set (make-instance 'frame-set
                                                   :sprite-sheet player-sprite-sheet
                                                   :row :falling
                                                   :ticks-per-frame 1000)))
                     (lambda ()
                       (make-instance 'frame-loop :frame-set frame-set)))
                  :idle
                  ,(let ((frame-set (make-instance 'frame-set
                                                   :sprite-sheet player-sprite-sheet
                                                   :row :idle
                                                   :ticks-per-frame 100)))
                     (lambda ()
                       (make-instance 'frame-loop :frame-set frame-set)))
                  :jumping
                  ,(let ((frame-set (make-instance 'frame-set
                                                   :sprite-sheet player-sprite-sheet
                                                   :row :jumping
                                                   :ticks-per-frame 100)))
                     (lambda ()
                       (make-instance 'one-shot :frame-set frame-set)))
                  :landing
                  ,(let ((frame-set (make-instance 'frame-set
                                                   :sprite-sheet player-sprite-sheet
                                                   :row :landing
                                                   :ticks-per-frame 100)))
                     (lambda ()
                       (make-instance 'one-shot :frame-set frame-set)))
                  :running
                  ,(let ((frame-set (make-instance 'frame-set
                                                   :sprite-sheet player-sprite-sheet
                                                   :row :idle
                                                   :ticks-per-frame 100)))
                     (lambda ()
                       (make-instance 'frame-loop :frame-set frame-set))))))
             ,@resources))

         (make-level (resources)
           `(:level
             ,(let ((player (make-instance 'player
                                           :x (scale 100)
                                           :y (scale 200)
                                           :state :idle
                                           :animation (funcall (get-resource '(:animations :player :idle) resources))
                                           :animations (get-resource '(:animations :player) resources))))
                (make-instance 'level
                               :tiles (car (read-level-data))
                               :player player
                               :entities '()))
             ,@resources))

         (make-menu (resources)
           `(:menu
             ,(make-instance
               'menu
               :buttons
               (let* ((texture (get-resource '(:textures :button-atlas) resources))
                      (button-height (scale (floor (sdl2:texture-height texture) 3)))
                      (button-width  (scale (floor (sdl2:texture-width texture) 3))))
                 (list
                  (make-instance 'button
                                 :x (/ (game-width) 2)
                                 :y (play-button-y)
                                 :height button-height
                                 :width button-width
                                 :state :idle
                                 :animation (funcall (get-resource '(:animations :button :play) resources))
                                 :action (lambda ()
                                           (setf (level game) (first-level game))))
                  (make-instance 'button
                                 :x (/ (game-width) 2)
                                 :y (options-button-y)
                                 :height button-height
                                 :width button-width
                                 :state :idle
                                 :animation (funcall (get-resource '(:animations :button :options) resources)))
                  (make-instance 'button
                                 :x (/ (game-width) 2)
                                 :y (quit-button-y)
                                 :height button-height
                                 :width button-width
                                 :state :idle
                                 :animation (funcall (get-resource '(:animations :button :quit) resources))
                                 :action #'sdl2:push-quit-event))))
            ,@resources)))

    (let-texture (renderer
                  (button-atlas-texture    (getf surfaces :button-atlas))
                  (menu-texture            (getf surfaces :menu))
                  (outside-sprites-texture (getf surfaces :outside))
                  (player-sprites-texture  (getf surfaces :player)))
      (fold-left (lambda (resources constructor)
                   (funcall constructor resources))
                 `(:textures
                   (:button-atlas   ,button-atlas-texture
                    :menu           ,menu-texture
                    :outside        ,outside-sprites-texture
                    :player         ,player-sprites-texture))
                 (list #'make-animations
                       #'make-level
                       #'make-menu
                       receiver)))))

(defun main ()
  (let ((game (make-instance 'platformer)))
    (run game)))
