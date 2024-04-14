;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass game ()
  ((level       :accessor level)
   (first-level :accessor first-level)
   (steps   :initform (cons 0 nil) :accessor steps)))

(defun initialize-game! (game resources)
  (setf (first-level game) (getf resources :level)
        (level game)  (getf resources :level)))

(defun render-game! (renderer game resources)
  ;; Clear any old image
  (sdl2:set-render-draw-color renderer #xff #xff #xff #xff)
  (sdl2:render-clear renderer)

  (render-level! renderer resources (level game))

  ;; display image
  (sdl2:render-present renderer))

(defun game-step! (game dticks)
  (level-step! game (level game) dticks)
  (sb-ext:atomic-incf (car (steps game))))

(defconstant +ticks-per-second+ 1000)
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

(defmacro with-game-loop ((game) &body body)
  `(CALL-WITH-GAME-LOOP
    ,game
    (LAMBDA () ,@body)))
