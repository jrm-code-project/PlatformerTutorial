;;; -*- Lisp -*-

(in-package "TUTORIAL")

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
                (cond ((or (eql (sdl2:scancode keysym) :scancode-escape)
                           (eql (sdl2:scancode keysym) :scancode-x))
                       (sdl2:push-quit-event))
                      ((member (sdl2:scancode keysym) '(:scancode-down
                                                        :scancode-left
                                                        :scancode-right
                                                        :scancode-up))
                       nil)
                      (t
                       (format t "~&Keydown: ~s~%" (sdl2:scancode keysym))
                       (force-output))))

      (:quit () t)
      )))

(defun main-window (game surfaces)
  (sdl2:with-window (window
                     :w (game-width)
                     :h (game-height)
                     :flags '(:shown))
    (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
      (with-resources ((resources) game surfaces renderer)
        (initialize-game! game resources)
        (with-game-loop (game)
          (main-event-loop game window renderer resources))))))

(defun run (game)
  (with-surfaces (surfaces game)
    (sdl2:with-init (:video)
      (main-window game surfaces))))

(defclass platformer (game)
  ())

(defmethod call-with-surfaces ((game platformer) receiver)
  (let-surfaces ((outside-sprites-surface (resource-pathname "outside_sprites.png"))
                 (player-sprites-surface  (resource-pathname "player_sprites.png")))
    (funcall receiver
             `(:outside ,outside-sprites-surface
               :player  ,player-sprites-surface))))

(defmethod call-with-resources ((game platformer) surfaces renderer receiver)
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

(defun main ()
  (let ((game (make-instance 'platformer)))
    (run game)))
