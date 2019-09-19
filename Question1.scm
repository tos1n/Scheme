#lang racket

(define 1over
  (lambda (list)
    (if (null? list) '()
    (let ((current (car list)))
      (if (or (= current 0) (= current 1)) (cons current (1over (cdr list)))
          (cons (/ 1 current) (1over (cdr list))))
      ))))
