;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defclass sprite-sheet ()
  ((selector        :initarg :selector        :reader selector)
   (frame-width     :initarg :frame-width     :reader frame-width)
   (frame-height    :initarg :frame-height    :reader frame-height)
   (baseline-offset :initarg :baseline-offset :reader baseline-offset)
   (rows            :initarg :rows            :reader get-rows)
   (row-limits      :initarg :row-limits      :reader row-limits)))

(defun make-sprite-sheet (selector textures rows row-limits &key (baseline-offset 0))
  (let* ((texture (funcall selector textures))
         (texture-width  (sdl2:texture-width texture))
         (texture-height (sdl2:texture-height texture)))
    (make-instance 'sprite-sheet
                   :selector selector
                   :frame-width  (/ texture-width (reduce #'max row-limits :initial-value most-negative-fixnum))
                   :frame-height (/ texture-height (length rows))
                   :baseline-offset baseline-offset
                   :rows rows
                   :row-limits row-limits)))

(defun row->index (sprite-sheet row)
  (position row (get-rows sprite-sheet)))

(defun row-limit (sprite-sheet row)
  (svref (row-limits sprite-sheet) (row->index sprite-sheet row)))

(defun render-sprite! (renderer textures sprite-sheet row column x y &key flip?)
  (sdl2:with-rects ((src (* column (frame-width sprite-sheet))
                         (* (row->index sprite-sheet row) (frame-height sprite-sheet))
                         (frame-width sprite-sheet)
                         (frame-height sprite-sheet))
                    (dest (- x (floor (scalef (frame-width sprite-sheet)) 2))
                          (- y (scale (- (frame-height sprite-sheet)
                                         (baseline-offset sprite-sheet))))
                          (scale (frame-width sprite-sheet))
                          (scale (frame-height sprite-sheet))))
    (sdl2:render-copy-ex renderer (funcall (selector sprite-sheet) textures)
                         :source-rect src
                         :dest-rect dest
                         :flip (if flip? '(:horizontal) '(:none)))))
