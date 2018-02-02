#!/usr/bin/guile
!#

; 习题1.41

(define (double f)
  (lambda (x) (f (f x)))
  )

; 21
(display (((double (double double)) (lambda (x) (+ x 1))) 5))
(newline)
