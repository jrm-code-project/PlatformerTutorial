# PlatformerTutorial — Chapter 22

## Cannonballs

A cannonball is essentially an enemy with very predictable movement.
Unlike the crabby enemy, which keeps track of its x-position and
increments or decrements it according to its speed, a cannonball is
more like a bobbing potion, but in this case it is the x-position that
varies linearly over time.

```
;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass cannonball (attackbox hitbox entity)
  ((speed :initarg :speed :reader get-speed)
   (start-tick :initform (sdl2:get-ticks) :accessor get-start-tick))
  (:default-initargs
   :width 26
   :height 26
   :attackbox-width 20
   :attackbox-height 20
   :attackbox-x-offset -10
   :attackbox-y-offset 0))

(defmethod get-x ((cannonball cannonball))
  (+ (call-next-method)
     (* (get-speed cannonball)
        (- (sdl2:get-ticks) (get-start-tick cannonball)))))
```

The underlying x-position of the cannonball is at the mouth of the
cannon, so it will travel linearly away from the cannon as time
increases.

Each cannon will have one cannonball.  When the cannon is fired, the
cannonball will enter the `:idle` state and the `start-tick` will be
set to the `(sdl2:get-ticks)`.  The cannonball's x-position is at the
mouth of the cannon, so as time increases its x-position will move
away from the cannon.

```
(defun start-cannonball! (cannonball)
  (setf (get-start-tick cannonball) (sdl2:get-ticks)
        (get-state cannonball) :idle))
```

At each `entity-step!`, we check to see if we have run into anything.
First we check against the edges of the game and against a left or
right wall.  If so, we just set the state to `nil` to disable the
cannonball.  The we check if we can attack the player (if we hit him).
If so, we `hit!` the player and set the state to `nil`.  If we don't
hit the game edges, the walls, or the player, we stay `:idle`.

```
(defmethod entity-step! (game level (cannonball cannonball) (state (eql :idle)) dticks)
  (cond ((let ((left (get-left cannonball))
               (right (get-right cannonball))
               (top (get-top cannonball))
               (bottom (get-bottom cannonball)))
           (or (< left 0)
               (>= right (level-width level))
               (< top 0)
               (>= bottom (game-height))
               (against-left-wall? level cannonball)
               (against-right-wall? level cannonball)
               ))
         (setf (get-state cannonball) nil))
        ((can-attack? cannonball (player level))
         (hit! (player level))
         (setf (get-state cannonball) nil))
        (t nil)))
```

In `cannon.lisp`, we add two slots:  a boolean to say whether we have
fired the cannon and a reference to the cannonball. 
```
(defclass cannon (attackbox entity)
  ((cannon-fired? :initform nil :accessor cannon-fired?) 
   (cannonball :initarg :cannonball :reader cannonball))
  (:default-initargs
   :attackbox-width (cannon-attackbox-width)
   :attackbox-height (cannon-attackbox-height)
   :attackbox-x-offset (cannon-attackbox-x-offset)
   :attackbox-y-offset (cannon-attackbox-y-offset)))
```

When we transition to the `:idle` state, we set the cannon as
not-fired.

```
(defmethod (setf get-state) :after ((state (eql :idle)) (cannon cannon))
  (setf (cannon-fired? cannon) nil)
  (start-animation! cannon :idle))
```
When we fire the cannon, the start the cannonball flying at frame 4 of
the animation.
```
(defmethod entity-step! (game level (cannon cannon) (state (eql :fire)) dticks)
  (when (and (= (get-frame (get-animation cannon)) 4)
             (not (cannon-fired? cannon)))
    (start-cannonball! (cannonball cannon))
    (setf (cannon-fired? cannon) t))
  (when (animation-finished? (get-animation cannon))
    (setf (get-state cannon) (make-instance 'dormant :until (+ (sdl2:get-ticks) 2500)))))
```
The `cannon-fired?` is an interlock so that we call
`start-cannonball!` exactly once when reach frame 4.

### `level.lisp`

When we construct a level, when we place a cannon, we also create a
cannonball at its mouth in the `nil` state.

```
              ...
              ((= (aref tiles i j 2) 5)
               (let* ((cannonball
                        (make-instance
                         'cannonball
                         :x (- (+ (* i (tile-size)) (/ (tile-size) 2)) (scale 24))
                         :y (- (* (+ j 1) (tile-size)) (scale 6))
                         :state nil
                         :speed (scalef -0.2)
                         :animation (funcall (get-resource '(:animations :cannonball :idle) resources))))
                      (cannon
                        (make-instance 'cannon
                                       :x (+ (/ (tile-size) 2) (* i (tile-size)))
                                       :y (- (* (+ j 1) (tile-size)) 1)
                                       :cannonball cannonball
                                       :state :idle
                                       :animation (funcall (get-resource '(:animations :cannon :idle) resources))
                                       :animations (get-resource '(:animations :cannon) resources))))
                 (push cannon entities)
                 (push cannonball entities)))
              ((= (aref tiles i j 2) 6)
               (let* ((cannonball
                        (make-instance
                         'cannonball
                         :x (+ (+ (* i (tile-size)) (/ (tile-size) 2)) (scale 24))
                         :y (- (* (+ j 1) (tile-size)) (scale 6))
                         :state nil
                         :speed (scalef 0.2)
                         :animation (funcall (get-resource '(:animations :cannonball :idle) resources))))
                      (cannon
                        (make-instance 'cannon
                                             :x (+ (/ (tile-size) 2) (* i (tile-size)))
                                             :y (- (* (+ j 1) (tile-size)) 1)
                                             :state :idle
                                             :flip t
                                             :cannonball cannonball
                                             :animation (funcall (get-resource '(:animations :cannon :idle) resources))
                                             :animations (get-resource '(:animations :cannon) resources))))
                 (push cannon entities)
                 (push cannonball entities)))
               ...
```
