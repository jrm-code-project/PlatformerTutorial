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
                (cond ((eql (sdl2:scancode keysym) :scancode-x)
                       (sdl2:push-quit-event))
                      ;; suppress output for these known keys
                      ((member (sdl2:scancode keysym) '(:scancode-backspace
                                                        :scancode-down
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


(defun call-with-surfaces (receiver)
  (let-surfaces ((outside-sprites-surface     (resource-pathname "outside_sprites.png"))
                 (player-sprites-surface      (resource-pathname "player_sprites.png")))
    (funcall receiver
             `(:outside             ,outside-sprites-surface
               :player              ,player-sprites-surface))))

(defun call-with-resources (surfaces renderer receiver)
  (let-texture (renderer
                (outside-sprites-texture (getf surfaces :outside))
                (player-sprites-texture  (getf surfaces :player)))
    (fold-left (lambda (resources constructor)
                 (funcall constructor resources))
               `(:textures
                 (:outside             ,outside-sprites-texture
                  :player              ,player-sprites-texture))
               (list #'make-animations
                     #'make-level
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

(defclass platformer (game)
  ())

(defun main ()
  (let ((game (make-instance 'platformer)))
    (run game)))
