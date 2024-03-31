;;; -*- Lisp -*-

(in-package "TUTORIAL")

(defmacro defun-scaled (parameter value)
  (let ((base-name (intern (concatenate 'string "BASE-" (symbol-name parameter)))))
    `(PROGN
       (DEFUN ,base-name () ,value)
       (DEFUN ,parameter () (SCALE (,base-name))))))
