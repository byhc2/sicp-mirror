#!/usr/bin/guile
!#

; 习题1.46

(define (iterative-improve g u)
  (lambda (x)
    (let ((x_ (u x)))
      (if (g x_)
	x_
	((iterative-improve g u) x_)
	)
      )
    )
  )

(define (sqrt2 x)
  ((iterative-improve
    (lambda (r) (< (abs (- (* r r) x)) 0.0001))
    (lambda (y) (/ (+ y (/ x y)) 2))
    ) 1.0)
  )

(display (sqrt2 3))
(newline)
