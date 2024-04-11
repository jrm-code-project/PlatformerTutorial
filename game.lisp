;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass game ()
  ((player  :accessor player)
   (level   :accessor level)

   (steps   :initform (cons 0 nil) :accessor steps)))

(defun initialize-game! (game resources)
  (setf (level game)  (getf resources :level)
        (player game) (getf resources :player)))

(defun render-game! (renderer game resources)
  ;; Clear any old image
  (sdl2:set-render-draw-color renderer #xff #xff #xff #xff)
  (sdl2:render-clear renderer)

  (render-level! renderer resources (level game))
  (render-entity! renderer resources (player game))

  ;; display image
  (sdl2:render-present renderer))

(defun game-step! (game dticks)
  (when (slot-boundp game 'player)
    (entity-step! game (player game) (get-state (player game)) dticks))

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

(defun call-with-resources (surfaces renderer receiver)
  (flet ((make-sprite-sheets (resources)
           `(:sprite-sheets
             (:player
              ,(make-sprite-sheet (lambda (textures) (getf textures :player))
                                  (getf resources :textures)
                                  #(:idle
                                    :running
                                    :jumping
                                    :falling
                                    :landing
                                    :hit
                                    :attack1
                                    :attack2
                                    :attack3)
                                  #(5 6 3 1 2 4 3 3 3)
                                  :baseline-offset 8))
             ,@resources))

         (make-frame-sets (resources)
           `(:frame-sets
             (:player
              (:idle
               ,(make-instance 'frame-set
                               :sprite-sheet (get-resource '(:sprite-sheets :player) resources)
                               :row :idle
                               :ticks-per-frame 100)
               :running
               ,(make-instance 'frame-set
                               :sprite-sheet (get-resource '(:sprite-sheets :player) resources)
                               :row :running
                               :ticks-per-frame 100)))
             ,@resources))

         (make-animations (resources)
           `(:animations
             (:player
              (:idle
               ,(make-instance 'frame-loop
                               :frame-set (get-resource '(:frame-sets :player :idle) resources))
               :running
               ,(make-instance 'frame-loop
                               :frame-set (get-resource '(:frame-sets :player :running) resources))))
             ,@resources))

         (make-player (resources)
           `(:player
             ,(make-instance 'player
                             :x (scale 50)
                             :y (scale 50)
                             :state :idle
                             :animation (get-resource '(:animations :player :idle) resources)
                             :frame-sets (get-resource '(:frame-sets :player) resources))
             ,@resources))

         (make-level (resources)
           `(:level
             ,(make-instance 'level
                             :tiles (car (read-level-data))
                             :entities '())
             ,@resources)))

    (let-texture (renderer
                  (player-sprites-texture (getf surfaces :player))
                  (outside-sprites-texture (getf surfaces :outside)))
      (fold-left (lambda (resources constructor)
                   (funcall constructor resources))
                 `(:textures
                   (:outside ,outside-sprites-texture
                    :player ,player-sprites-texture))
                 (list #'make-sprite-sheets
                       #'make-frame-sets
                       #'make-animations
                       #'make-player
                       #'make-level
                       receiver)))))

(defmacro with-resources (((resources) game surfaces renderer) &body body)
  `(CALL-WITH-RESOURCES
    ,game ,surfaces ,renderer
    (LAMBDA (,resources)
      ,@body)))
