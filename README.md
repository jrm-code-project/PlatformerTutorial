# PlatformerTutorial — Chapter 21

## Cannon

A cannon is entity with an attackbox, but instead of attacking the
player directly, it triggers a cannonball.  The attackbox determines
how far the cannon "sees".  If the player enters the attackbox, the
cannon fires after a brief delay.  Cannon are not machine guns, so we
add some cool down time after firing before the cannon can fire again.
```
;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled cannon-attackbox-width (* 5 (base-tile-size)))
(defun-scaled cannon-attackbox-height 15)
(defun-scaled cannon-attackbox-x-offset (- (+ (base-cannon-attackbox-width) 15)))
(defun-scaled cannon-attackbox-y-offset 5)

(defclass cannon (attackbox entity)
  ()
  (:default-initargs
   :attackbox-width (cannon-attackbox-width)
   :attackbox-height (cannon-attackbox-height)
   :attackbox-x-offset (cannon-attackbox-x-offset)
   :attackbox-y-offset (cannon-attackbox-y-offset)))
```

## Parameterized State

Until now, the state of an entity has always just been a keyword.
We've used `eql` specializers to dispatch on the state.  But we can
make the state be an instance of a class and dispatch on it with a
normal class specifier.  So the state of an entity can be
parameterized with some state of its own.

In this case, we introduce two new states, `dormant`, the state of
rest after a cannon is fired and before it becomes idle again, and
`hold`, the time between when the cannon detects the player and it
fires the cannonball.  Both of these states have a timestamp called
`until` that is the time in the future when the state expires.

```
(defclass dormant ()
  ((until :initarg :until :reader get-until)))

(defclass hold ()
  ((until :initarg :until :reader get-until)))
```

As usual, we set the `(setf get-state)` `:after` methods to start the
approprite animations:
```
(defmethod (setf get-state) :after ((state dormant) (cannon cannon))
  (start-animation! cannon :idle))

(defmethod (setf get-state) :after ((state (eql :fire)) (cannon cannon))
  (start-animation! cannon :fire))

(defmethod (setf get-state) :after ((state hold) (cannon cannon))
  (start-animation! cannon :idle))

(defmethod (setf get-state) :after ((state (eql :idle)) (cannon cannon))
  (start-animation! cannon :idle))
```

Finally, the entity state machine:
```
(defmethod entity-step! (game level (cannon cannon) (state dormant) dticks)
  (when (> (sdl2:get-ticks) (get-until state))
    (setf (get-state cannon) :idle)))

(defmethod entity-step! (game level (cannon cannon) (state (eql :fire)) dticks)
  (when (animation-finished? (get-animation cannon))
    (setf (get-state cannon) (make-instance 'dormant :until (+ (sdl2:get-ticks) 2500)))))

(defmethod entity-step! (game level (cannon cannon) (state hold) dticks)
  (when (> (sdl2:get-ticks) (get-until state))
     (setf (get-state cannon) :fire)))

(defmethod entity-step! (game level (cannon cannon) (state (eql :idle)) dticks)
  (when (can-attack? cannon (player level))
    (setf (get-state cannon) (make-instance 'hold :until (+ (sdl2:get-ticks) 500)))))
```
