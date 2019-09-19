#lang racket

(define TOL 1e-6)

(define p_cos
  (lambda (x)
    (calculateProduct x 1)
    )
  )

; Gets product for a specific n.
(define calculateProduct
  (lambda (x n)
    (let ((change (/ (* 4 (expt x 2)) (* (expt pi 2) (expt (- (* 2 n) 1) 2)))))
      (if (<= (abs change) TOL) 1
          (* (- 1 change) (calculateProduct x (+ n 1)))
          )
      )
    )
  )
