;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defconstant +frames-per-second+ 60)
(defconstant +ticks-per-frame+ (/ +ticks-per-second+ +frames-per-second+))
(defconstant +titles-per-second+ 1)
(defconstant +ticks-per-title+ (/ +ticks-per-second+ +titles-per-second+))

(defun main-event-loop (game window renderer textures)
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
                      (render-game! renderer game textures)
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
                (format t "~&Keydown: ~s~%" (sdl2:scancode keysym))
                (force-output)
                ;; Quit on escape.
                (when (eql (sdl2:scancode keysym) :scancode-escape)
                  (sdl2:push-quit-event)))

      (:quit () t)
      )))

(defun main-window (game surfaces)
  (sdl2:with-window (window
                     :w 800
                     :h 600
                     :flags '(:shown))
    (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
      (with-textures (textures game surfaces renderer)
        (main-event-loop game window renderer textures)))))

(defun run (game)
  (with-surfaces (surfaces game)
    (with-game-loop (game)
      (sdl2:with-init (:video)
        (main-window game surfaces)))))

#||
(defclass bouncing-rectangle (game)
  ((x-pos :initform 100 :accessor get-x)
   (y-pos :initform 100 :accessor get-y)
   (x-vel :initform .25 :accessor get-x-vel)
   (y-vel :initform .25 :accessor get-y-vel)
   (width :initform 200 :reader get-rect-width)
   (height :initform 50 :reader get-rect-height)))

(defmethod render-game! (renderer (game bouncing-rectangle) textures)
  (sdl2:set-render-draw-color renderer #xff #x00 #x00 #xff)
  (sdl2:with-rects ((rect (floor (get-x game))
                          (floor (get-y game))
                          (get-rect-width game)
                          (get-rect-height game)))
    (sdl2:render-fill-rect renderer rect)))

(defmethod game-step! ((game bouncing-rectangle) dticks)
  (let* ((x-pos* (+ (get-x game) (* dticks (get-x-vel game))))
         (y-pos* (+ (get-y game) (* dticks (get-y-vel game)))))
    (cond ((< x-pos* 0) (setf (get-x-vel game) .25))
          ((> (+ x-pos* (get-rect-width game)) 800) (setf (get-x-vel game) -.25))
          (t (setf (get-x game) x-pos*)))
    (cond ((< y-pos* 0) (setf (get-y-vel game) .25))
          ((> (+ y-pos* (get-rect-height game)) 600) (setf (get-y-vel game) -.25))
          (t (setf (get-y game) y-pos*)))
    (call-next-method)))
||#

(defclass player-sprite (game)
  ())

(defmethod call-with-surfaces ((game player-sprite) receiver)
  (let-surfaces ((player-sprites-surface (resource-pathname "player_sprites.png")))
    (funcall receiver
             `(:player ,player-sprites-surface))))

(defmethod call-with-textures ((game player-sprite) surfaces renderer receiver)
  (let-texture (renderer
                (player-sprites-texture (getf surfaces :player)))
    (funcall receiver
             `(:player ,player-sprites-texture))))

(defmethod render-game! (renderer (game player-sprite) textures)
  (let ((sprites (getf textures :player)))
    (sdl2:with-rects ((src 0
                           0
                           (sdl2:texture-width sprites)
                           (sdl2:texture-height sprites))
                      (dst 100
                           100
                           (sdl2:texture-width sprites)
                           (sdl2:texture-height sprites)))
      (sdl2:render-copy renderer sprites :source-rect src :dest-rect dst))))

(defun main ()
  (let ((game (make-instance 'player-sprite)))
    (run game)))
