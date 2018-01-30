#!/usr/bin/guile
!#

; 习题1.10
; (A 1 10) 2^10=1024
; (A 2 4) 2^16=65536
; (A 3 3) 2^16=65536

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))
	)
  )

(display (A 1 10))
(newline)
(display (A 3 3))
(newline)
(display (A 2 1))
(newline)
(display (A 2 2))
(newline)
(display (A 2 3))
(newline)
(display (A 2 4))
(newline)

; (define (f n) (A 0 n))  2n
; (define (g n) (A 1 n))  2^n
; (define (h n) (A 2 n))  2^2^n
