#!/usr/bin/guile
!#

; 习题1.43

(define (smooth f)
  (define dx 0.0001)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))
  )

(define (repeated f n)
  (define (repeated-iter g i)
    (if (> i n)
      g
      (repeated-iter (lambda (x) (f (g x))) (+ i 1))
      )
    )
  (repeated-iter (lambda (x) x) 1)
  )

(display ((repeated (smooth (lambda (x) x)) 10) 1.0))
(newline)
