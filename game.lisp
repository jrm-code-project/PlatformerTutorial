;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass game ()
  ((steps :initform (cons 0 nil) :accessor steps)))

(defgeneric render-game! (renderer game)
  (:method :before (renderer game)
    ;; Clear any old image
    (sdl2:set-render-draw-color renderer #xff #xff #xff #xff)
    (sdl2:render-clear renderer))

  (:method (renderer game)
    nil)

  (:method :after (renderer game)
    ;; display image
    (sdl2:render-present renderer)))

(defgeneric game-step! (game dticks)
  (:method ((game game) dticks)
    (sb-ext:atomic-incf (car (steps game)))))

(defconstant +steps-per-second+ 200)
(defconstant +ticks-per-step+ (/ +ticks-per-second+ +steps-per-second+))

(defun game-loop (game thread-control-cell)
  (let ((start-tick (sdl2:get-ticks))
        (last-tick  (sdl2:get-ticks))
        (last-step 0))
    (do ()
        ((not (zerop (car thread-control-cell))))
      (let* ((tick (- (sdl2:get-ticks) start-tick))
             (dticks (- tick last-tick))
             (this-step (floor tick +ticks-per-step+)))
        (cond ((> this-step last-step)
               (game-step! game dticks)
               (setq last-step this-step)
               (setq last-tick tick))
              (t (sleep 0.001)))))))

(defun call-with-game-loop (game thunk)
  (let ((thread-control-cell (cons 0 nil))
        (thread nil))
    (unwind-protect
         (progn (setq thread
                      (bordeaux-threads:make-thread
                       (lambda () (game-loop game thread-control-cell))
                       :name "Game Loop"))
                (funcall thunk))
      (when thread
        (sb-ext:atomic-incf (car thread-control-cell))
        (bordeaux-threads:join-thread thread)))))
    
