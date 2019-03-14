#!/usr/bin/guile
!#

; 习题1.17 递归过程略
; 习题1.18

(define (double x)
  (+ x x)
  )

(define (halve x)
  (/ x 2)
  )

(define (fast-mult a b)
  (define (even x)
    (= (remainder x 2) 0)
    )
  (define (fast-mult-iter s m r)
    (if (= r 0)
      s
      (if (even r)
        (fast-mult-iter s (double m) (halve r))
        (fast-mult-iter (+ s m) m (+ r -1))
        )
      )
    )
  (fast-mult-iter 0 a b)
  )

(display (fast-mult 3 6))
(newline)
