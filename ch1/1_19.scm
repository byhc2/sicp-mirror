#!/usr/bin/guile
!#

; 习题1.19

(define (even x)
  (= (remainder x 2) 0)
  )

(define (fib n)
  (define (fib-iter a b p q r)
    (if (= r 0)
      b
      (if (even r)
        (fib-iter
          a
          b
          (+ (* p p) (* q q))
          (+ (* 2 p q) (* q q))
          (/ r 2)
          )
        (fib-iter
          (+ (* b q) (* a q) (* a p))
          (+ (* b p) (* a q))
          p
          q
          (- r 1)
          )
        )
      )
    )
  (fib-iter 1 0 0 1 n)
  )

(display (fib 8))
(newline)
