;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled crabby-width 26)
(defun-scaled crabby-height 20)
(defun crabby-velocity () (scalef 0.05))

(defclass crabby (enemy)
  ((delta-y :initform 0 :accessor delta-y)
   (x-velocity :initform 0 :accessor get-x-velocity))
  (:default-initargs
   :width (crabby-width)
   :height (crabby-height)
   :initial-health 20))

(defmethod (setf get-state) :after ((state (eql :dying)) (crabby crabby))
  (start-animation! crabby :dying))

(defmethod (setf get-state) :after ((state (eql :hit)) (crabby crabby))
  (start-animation! crabby :hit))

(defmethod (setf get-state) :after ((state (eql :idle)) (crabby crabby))
  (start-animation! crabby :idle))

(defmethod (setf get-state) :after ((state (eql :running)) (crabby crabby))
  (start-animation! crabby :running))

(defmethod entity-step! (game level (crabby crabby) (state (eql :dying)) dticks)
  (when (animation-finished? (get-animation crabby))
    (setf (get-state crabby) nil)))

(defmethod entity-step! (game level (crabby crabby) (state (eql :falling)) dticks)
  (if (entity-supported? level crabby)
      (setf (delta-y crabby) 0
            (get-state crabby) :idle)
      (let ((dy (* (delta-y crabby) dticks)))
        (incf (delta-y crabby) (* (gravity) dticks))
        (move-entity-vertically! level crabby dy))))

(defmethod entity-step! (game level (crabby crabby) (state (eql :hit)) dticks)
  (cond ((not (plusp (get-health crabby)))
         (setf (get-state crabby) :dying
               (get-x-velocity crabby) 0))
        ((animation-finished? (get-animation crabby))
         (setf (get-state crabby) :idle))
        (t nil)))

(defmethod entity-step! (game level (crabby crabby) (state (eql :idle)) dticks)
  (let ((player (player level)))
    (cond ((not (entity-supported? level crabby))
           (setf (delta-y crabby) 0
                 (get-state crabby) :falling))
          ((not (plusp (get-health crabby)))
           (setf (get-state crabby) :dying))
          ((and (same-level? crabby player)
                (within-five-tiles? crabby player))
           (setf (get-state crabby) :running
                 (get-x-velocity crabby) (if (> (get-x crabby) (get-x player))
                                             (- (crabby-velocity))
                                             (crabby-velocity))))
          (t nil))))

(defmethod entity-step! (game level (crabby crabby) (state (eql :running)) dticks)
  (move-entity-horizontally! level crabby (* (get-x-velocity crabby) dticks))
  (cond ((not (plusp (get-health crabby)))
         (setf (get-state crabby) :dying
               (get-x-velocity crabby) 0))
        ((or (unsupported-on-left? level crabby)
             (against-left-wall? level crabby))
         (setf (get-x-velocity crabby) (crabby-velocity)))
        ((or (unsupported-on-right? level crabby)
             (against-right-wall? level crabby))
         (setf (get-x-velocity crabby) (- (crabby-velocity))))
        ((and (same-level? crabby (player level))
              (within-five-tiles? crabby (player level)))
         (setf (get-x-velocity crabby) (if (< (get-x crabby) (get-x (player level)))
                                           (crabby-velocity)
                                           (- (crabby-velocity)))))
        (t nil)))
