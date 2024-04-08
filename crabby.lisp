;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defun-scaled crabby-width 26)
(defun-scaled crabby-height 20)

(defclass crabby (enemy)
  ((delta-y :initform 0 :accessor delta-y))
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
  (cond ((not (entity-supported? level crabby))
         (setf (delta-y crabby) 0
               (get-state crabby) :falling))
        (t nil)))
  
