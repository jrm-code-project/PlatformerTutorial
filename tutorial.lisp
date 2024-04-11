;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defconstant +frames-per-second+ 60)
(defconstant +ticks-per-frame+ (/ +ticks-per-second+ +frames-per-second+))
(defconstant +titles-per-second+ 1)
(defconstant +ticks-per-title+ (/ +ticks-per-second+ +titles-per-second+))

(defun main-event-loop (game window renderer)
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
                      (render-game! renderer game)
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
               (t
                (format t "~&Keydown: ~s~%" (sdl2:scancode keysym))
                (force-output))))

      (:quit () t)
      )))

(defun main-window (game)
  (sdl2:with-window (window
                     :w 800
                     :h 600
                     :flags '(:shown))
    (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
      (main-event-loop game window renderer))))

(defun run (game)
  (with-game-loop (game)
    (sdl2:with-init (:video)
      (main-window game))))

(defun main ()
  (let ((game (make-instance 'game)))
    (run game)))
