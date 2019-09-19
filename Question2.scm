#lang racket

(define TOL 1e-6)

(define newtonRhap
  (lambda (x f fx)
    (let* ((change (/ (f x) (fx x))) (newX (- x change)))
      (if (< (abs change) TOL) newX
          (newtonRhap newX f fx)))))

(newtonRhap 0.1 sin cos)
(newtonRhap 2.0 (lambda (x) (- (* x x) x 6)) (lambda (x) (- (* 2 x) 1)))
(newtonRhap -20.0 (lambda (x) (- (* x x) x 6)) (lambda (x) (- (* 2 x) 1)))
