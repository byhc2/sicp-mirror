#!/usr/bin/guile
!#

(define (square x) (* x x))

(define (good-enough x y)
	(< (abs (- (square y) x))
		0.000000000000001
	)
)

(define (update x y)
	(/ (+ y (/ x y)) 2.0)
)

(define (sqrt-iter x y)
	(if (good-enough x y)
		y
		(sqrt-iter x (update x y))
	)
)

(define (sqrt2 x)
	(sqrt-iter x 1.0)
)

(display (sqrt2 900000000000000000000000000000000000000000000000000000000000000000000000000000000000))
(newline)
