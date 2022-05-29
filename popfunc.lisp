;; popfunc - Sketch iterates of the population function

(ql:quickload "sketch")

(defpackage :popfunc
  (:use :cl)
  (:nicknames :pf)
  (:export :popfunc-sketch :main)
  (:local-nicknames (:sk :sketch)))
(in-package :popfunc)

(defun rescale (x old-scale new-scale)
  (/ (* x new-scale) old-scale))

(defun rescale-range (x old-min old-max new-min new-max)
  (+ new-min (rescale (- x old-min)
                      (- old-max old-min)
                      (- new-max new-min))))

(defun iterate (f x times)
  (do ((y x (funcall f x))
       (n times (1- n)))
      ((zerop n) y)))

;; Construct the function f(x) = r * x * (1 - x)
(defun population-function (r)
  (lambda (x) (* r x (- 1 x))))

(defun draw-iterates (r x pens)
  (let ((f (population-function r))
        (y 0.5))
    (dotimes (step (length pens))
      (setf y (funcall f y))
      (sk:with-pen (aref pens step)
        (sk:point x (rescale-range y 0.0 1.0 0 480))))))

(defun make-grayscale-pens (steps)
  (let ((grayscale-pens (make-array steps)))
    (dotimes (i steps)
      (setf (aref grayscale-pens i)
            (sk:make-pen :stroke (sk:gray (/ (- steps i) steps)))))
    grayscale-pens))

(sk:defsketch popfunc-sketch ((sk:y-axis :up))
  (let ((pens (make-grayscale-pens 500)))
    (sk:background sk:+white+)
    (dotimes (x 640)
      (let ((r (rescale-range x 0 640 2.0 4.0)))
        (draw-iterates r x pens)))))

(defun main ()
  (make-instance 'popfunc-sketch :height 480 :width 640))
