;;; -*- Lisp -*-

(in-package "TUTORIAL")

(declaim (ftype (function ((function (t t &rest t) t) t t &rest t) t) fold-left))
(defun fold-left (function initial list &rest lists)
  (labels ((fold-left-1 (state item tail)
             (declare (optimize (debug 0) (safety 0) (speed 3)))
             (cond ((consp tail)
                    (fold-left-1 (funcall function state item) (car tail) (cdr tail)))
                   ((null tail) (funcall function state item))
                   (t (error "Dotted list encountered by fold-left."))))

           (fold-left-n (state items tails)
             (cond ((every #'consp tails)
                    (fold-left-n (apply function state items) (map 'list #'car tails) (map 'list #'cdr tails)))
                   ((every #'null tails) (apply function state items))
                   (t (error "Lists of different lengths or dotted list in fold-left.")))))

    (if (null lists)
        (cond ((consp list) (fold-left-1 initial (car list) (cdr list)))
              ((null list) initial)
              (t (error "Non list in fold-left.")))
        (let ((tails (cons list lists)))
          (cond ((every #'consp tails)
                 (fold-left-n initial (map 'list #'car tails) (map 'list #'cdr tails)))
                ((every #'null tails) initial)
                (t (error "Non list in fold-left.")))))))

(defun get-resource (key resources)
  (fold-left #'getf resources key))
