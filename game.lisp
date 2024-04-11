;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass game ()
  ((steps :initform (cons 0 nil) :accessor steps)

   (x-pos :initform 100 :accessor get-x)
   (y-pos :initform 100 :accessor get-y)
   (x-vel :initform .25 :accessor get-x-vel)
   (y-vel :initform .25 :accessor get-y-vel)
   (width  :initform 200 :reader get-rect-width)
   (height :initform 50  :reader get-rect-height)))

(defun render-game! (renderer game)
  ;; Clear any old image
  (sdl2:set-render-draw-color renderer #xff #xff #xff #xff)
  (sdl2:render-clear renderer)

  ;; Draw the red rectangle at its location.
  (sdl2:set-render-draw-color renderer #xff #x00 #x00 #xff)
  (sdl2:with-rects ((rect (floor (get-x game))
                          (floor (get-y game))
                          (get-rect-width game)
                          (get-rect-height game)))
    (sdl2:render-fill-rect renderer rect))
  
  ;; display image
  (sdl2:render-present renderer))

(defun game-step! (game dticks)
  (let* ((x-pos* (+ (get-x game) (* dticks (get-x-vel game))))
         (y-pos* (+ (get-y game) (* dticks (get-y-vel game)))))
    ;; Update x position and velocity
    (cond ((< x-pos* 0) (setf (get-x-vel game) .25))
          ((> (+ x-pos* (get-rect-width game)) 800) (setf (get-x-vel game) -.25))
          (t (setf (get-x game) x-pos*)))
    ;; Update y position and velocity.
    (cond ((< y-pos* 0) (setf (get-y-vel game) .25))
          ((> (+ y-pos* (get-rect-height game)) 600) (setf (get-y-vel game) -.25))
          (t (setf (get-y game) y-pos*)))

    (sb-ext:atomic-incf (car (steps game)))))

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
  `(CALL-WITH-GAME-LOOP ,game
     (LAMBDA () ,@body)))
