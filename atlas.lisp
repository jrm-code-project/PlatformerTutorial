;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass atlas ()
  ((selector        :initarg :selector        :reader selector)
   (frame-width     :initarg :frame-width     :reader frame-width)
   (frame-height    :initarg :frame-height    :reader frame-height)
   (baseline-offset :initarg :baseline-offset :reader baseline-offset)
   (rows            :initarg :rows            :reader get-rows)
   (row-limits      :initarg :row-limits      :reader row-limits)))

(defun row-limit (atlas row)
  (svref (row-limits atlas) (position row (get-rows atlas))))

(defun make-atlas (texture selector rows row-limits &key (baseline-offset 0))
  (let* ((texture-width  (sdl2:texture-width texture))
         (texture-height (sdl2:texture-height texture)))
    (make-instance 'atlas
                   :selector selector
                   :frame-width  (/ texture-width (reduce #'max row-limits :initial-value most-negative-fixnum))
                   :frame-height (/ texture-height (length rows))
                   :baseline-offset baseline-offset
                   :rows rows
                   :row-limits row-limits)))

(defun render-sprite! (renderer textures atlas row column x y &key flip?)
  (sdl2:with-rects ((src (* column (frame-width atlas))
                         (* (position row (get-rows atlas)) (frame-height atlas))
                         (frame-width atlas)
                         (frame-height atlas))
                    (dest (- x (floor (scalef (frame-width atlas)) 2))
                          (- y (scale (- (frame-height atlas)
                                         (baseline-offset atlas))))
                          (scale (frame-width atlas))
                          (scale (frame-height atlas))))
    (sdl2:render-copy-ex renderer (funcall (selector atlas) textures)
                         :source-rect src
                         :dest-rect dest
                         :flip (if flip? '(:horizontal) '(:none)))))
