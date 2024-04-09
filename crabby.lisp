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
   :height (crabby-height)))

(defmethod entity-step! (level (crabby crabby) (state (eql :falling)) dticks)
  (if (entity-supported? level crabby)
      (setf (delta-y crabby) 0
            (get-state crabby) :idle)
      (let ((dy (* (delta-y crabby) dticks)))
        (incf (delta-y crabby) (* (gravity) dticks))
        (move-entity-vertically! level crabby dy))))

(defmethod entity-step! (level (crabby crabby) (state (eql :idle)) dticks)
  (let ((player (player level)))
    (cond ((not (entity-supported? level crabby))
           (setf (delta-y crabby) 0
                 (get-state crabby) :falling))
          ((and (< (abs (- (get-y crabby) (get-y player))) (tile-size))
                (< (abs (- (get-x crabby) (get-x player))) (* 5 (tile-size))))
           (setf (get-state crabby) :running
                 (get-animation crabby) (funcall (getf (animations crabby) :running))
                 (get-x-velocity crabby) (if (> (get-x crabby) (get-x player))
                                             (- (crabby-velocity))
                                             (crabby-velocity))))
          (t nil))))

(defmethod entity-step! (level (crabby crabby) (state (eql :running)) dticks)
  (move-entity-horizontally! level crabby (* (get-x-velocity crabby) dticks))
  (cond ((or (unsupported-on-left? level crabby)
            (against-left-wall? level crabby))
         (setf (get-x-velocity crabby) (crabby-velocity)))
        ((or (unsupported-on-right? level crabby)
             (against-right-wall? level crabby))
         (setf (get-x-velocity crabby) (- (crabby-velocity))))
        ((and (< (abs (- (get-y crabby) (get-y (player level)))) (tile-size))
              (> (get-x (player level)) (get-x crabby))
              (< (- (get-x (player level)) (get-x crabby)) (* 5 (tile-size))))
         (setf (get-x-velocity crabby) (crabby-velocity)))
        ((and (< (abs (- (get-y crabby) (get-y (player level)))) (tile-size))
              (< (get-x (player level)) (get-x crabby))
              (< (- (get-x crabby) (get-x (player level))) (* 5 (tile-size))))
         (setf (get-x-velocity crabby) (- (crabby-velocity))))
        (t nil)))
