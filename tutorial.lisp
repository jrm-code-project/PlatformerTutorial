;;; -*- Lisp -*-
    
(in-package "TUTORIAL")

(defun render! (renderer)
  ;; Clear any old image
  (sdl2:set-render-draw-color renderer #xff #xff #xff #xff)
  (sdl2:render-clear renderer)

  ;; Draw the new image
  (sdl2:set-render-draw-color renderer #xff #x00 #x00 #xff)
  (sdl2:with-rects ((rect 100 200 200 50))
    (sdl2:render-fill-rect renderer rect))

  ;; Send it to the screen
  (sdl2:render-present renderer))

(defconstant +ticks-per-second+ 1000)
(defconstant +frames-per-second+ 60)
(defconstant +ticks-per-frame+ (/ +ticks-per-second+ +frames-per-second+))
(defconstant +titles-per-second+ 1)
(defconstant +ticks-per-title+ (/ +ticks-per-second+ +titles-per-second+))

(defun main-event-loop (window renderer)
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
                      (render! renderer)
                      (setq last-frame this-frame)
                      (incf frame-count))
                     ((> this-title last-title)
                      (sdl2:set-window-title window
                                             (format nil "Platformer, frame rate: ~d" frame-count))
                      (setq last-title this-title)
                      (setq frame-count 0))
                     (t (sleep 0.002)))))

      (:keydown (:keysym keysym)
                (format t "~&Keydown: ~s~%" (sdl2:scancode keysym))
                (force-output)
                ;; Quit on escape.
                (when (eql (sdl2:scancode keysym) :scancode-escape)
                  (sdl2:push-quit-event)))

      (:quit () t)
      )))

(defun main-window ()
  (sdl2:with-window (window
                     :w 800
                     :h 600
                     :flags '(:shown))
    (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
      (main-event-loop window renderer))))

(defun main ()
  (sdl2:with-init (:video)
    (main-window)))
