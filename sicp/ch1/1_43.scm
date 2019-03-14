#!/usr/bin/guile
!#

; 习题1.43

(define (repeated f n)
  (define (repeated-iter g i)
    (if (> i n)
      g
      (repeated-iter (lambda (x) (f (g x))) (+ i 1))
      )
    )
  (repeated-iter (lambda (x) x) 1)
  )

(define (inc x) (+ x 1))

(display ((repeated inc 2) 1))
(newline)
(display ((repeated (lambda (x) (* x x)) 2) 5))
(newline)
