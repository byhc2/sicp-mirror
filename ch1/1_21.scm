#!/usr/bin/guile
!#

; 习题1.21

(define (smallest-divisor n)
  (define (square x)
    (* x x)
    )
  (define (find-divisor x)
    (if (> (square x) n)
      n
      (if (= (remainder n x) 0)
	x
	(find-divisor (+ x 1))
	)
      )
    )
  (find-divisor 2)
  )

(display (smallest-divisor 199))
(newline)
(display (smallest-divisor 1999))
(newline)
(display (smallest-divisor 19999))
(newline)
