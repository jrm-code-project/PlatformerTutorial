;;; -*- Lisp -*-

(in-package "TUTORIAL")

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
                (cond ((eql (sdl2:scancode keysym) :scancode-x)
                       (sdl2:push-quit-event))
                      ;; suppress output for these known keys
                      ((member (sdl2:scancode keysym) '(:scancode-backspace
                                                        :scancode-escape
                                                        :scancode-left
                                                        :scancode-return
                                                        :scancode-right
                                                        :scancode-space
                                                        :scancode-up))
                       nil)
                      (t
                       (format t "~&Keydown: ~s~%" (sdl2:scancode keysym))
                       (force-output))))

      (:quit () t)
      )))

(defun call-with-resources (surfaces renderer receiver)
  (let-texture (renderer
                (big-clouds-texture      (getf surfaces :big-clouds))
                (button-atlas-texture    (getf surfaces :button-atlas))
                (cannon-texture          (getf surfaces :cannon))
                (cannonball-texture      (getf surfaces :cannonball))
                (containers-texture      (getf surfaces :containers))
                (crabby-atlas-texture    (getf surfaces :crabby-atlas))
                (game-over-texture       (getf surfaces :game-over))
                (health-bar-texture      (getf surfaces :health-bar))
                (level-complete-texture  (getf surfaces :level-complete))
                (menu-texture            (getf surfaces :menu))
                (menu-background-texture (getf surfaces :menu-background))
                (outside-sprites-texture (getf surfaces :outside))
                (pause-menu-texture      (getf surfaces :pause-menu))
                (player-sprites-texture  (getf surfaces :player))
                (playing-background-texture (getf surfaces :playing-background))
                (potions-texture         (getf surfaces :potions))
                (small-clouds-texture    (getf surfaces :small-clouds))
                (sound-atlas-texture     (getf surfaces :sound-button-atlas))
                (spikes-texture          (getf surfaces :spikes))
                (urm-atlas-texture       (getf surfaces :urm-button-atlas))
                (volume-atlas-texture    (getf surfaces :volume-button-atlas)))
    (fold-left (lambda (resources constructor)
                 (funcall constructor resources))
               `(:textures
                 (:big-clouds          ,big-clouds-texture
                  :button-atlas        ,button-atlas-texture
                  :cannon              ,cannon-texture
                  :cannonball          ,cannonball-texture
                  :containers          ,containers-texture
                  :crabby              ,crabby-atlas-texture
                  :game-over           ,game-over-texture
                  :health-bar          ,health-bar-texture
                  :level-complete      ,level-complete-texture
                  :menu                ,menu-texture
                  :menu-background     ,menu-background-texture
                  :outside             ,outside-sprites-texture
                  :pause-menu          ,pause-menu-texture
                  :playing-background  ,playing-background-texture
                  :player              ,player-sprites-texture
                  :potions             ,potions-texture
                  :small-clouds        ,small-clouds-texture
                  :sound-button-atlas  ,sound-atlas-texture
                  :spikes              ,spikes-texture
                  :urm-button-atlas    ,urm-atlas-texture
                  :volume-button-atlas ,volume-atlas-texture))
               (list #'make-animations
                     #'make-game-over
                     #'make-level-complete
                     #'make-levels
                     #'make-menu
                     #'make-pause-menu
                     receiver))))

(defmacro with-resources ((resources surfaces renderer) &body body)
  `(CALL-WITH-RESOURCES
    ,surfaces ,renderer
    (LAMBDA (,resources)
      ,@body)))

(defun main-window (game surfaces)
  (sdl2:with-window (window
                     :w (game-width)
                     :h (game-height)
                     :flags '(:shown))
    (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
      (with-resources (resources surfaces renderer)
        (initialize-game! game resources)
        (with-game-loop (game)
          (main-event-loop game window renderer resources))))))

(defun run (game)
  (sdl2-ttf:init)
  (with-sdl2-images (:png)
    (with-surfaces (surfaces)
      (sdl2:with-init (:video)
        (main-window game surfaces)))))

(defun main ()
  (let ((game (make-instance 'game)))
    (run game)))
