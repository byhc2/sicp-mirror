#!/usr/bin/guile
!#

; 习题1.42

(define (compose f g)
  (lambda (x) (f (g x)))
  )

(display ((compose (lambda (x) (* x x)) (lambda (x) (+ x 1))) 6))
(newline)
