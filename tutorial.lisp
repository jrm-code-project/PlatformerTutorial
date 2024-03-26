;;; -*- Lisp -*-
    
(in-package "TUTORIAL")

(defun main-event-loop ()
  (sdl2:with-event-loop (:method :poll)

    (:idle () (sleep 0.02))

    (:keydown (:keysym keysym)
       (format t "~&Keydown: ~s~%" (sdl2:scancode keysym))
       (force-output)
       ;; Quit on escape.
       (when (eql (sdl2:scancode keysym) :scancode-escape)
         (sdl2:push-quit-event)))

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
