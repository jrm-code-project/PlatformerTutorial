;;; -*- Lisp -*-
    
(in-package "TUTORIAL")

(defun main-event-loop ()
  (sdl2:with-event-loop (:method :poll)

    (:idle () (sleep 0.02))

    (:keydown (:keysym keysym)
       (cond ((eql (sdl2:scancode keysym) :scancode-x)
              (sdl2:push-quit-event))
             (t
              (format t "~&Keydown: ~s~%" (sdl2:scancode keysym))
              (force-output))))

    (:quit () t)
    ))

(defun main-window ()
  (sdl2:with-window (window
                     :w 800
                     :h 600
                     :flags '(:shown))
    (main-event-loop)))

(defun main ()
  (sdl2:with-init (:video)
    (main-window)))
