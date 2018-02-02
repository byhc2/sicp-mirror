#!/usr/bin/guile
!#

; 习题1.40

(define (derive g)
  (define dx 0.000000001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))
  )

(display ((derive (lambda (x) (* x x x))) 5))
(newline)

(define (fixed-point f)
  (define (fixed-point-iter guess)
    (
     let (
	  (new-guess (f guess))
	  (close-enough? (lambda (x y) (< (abs (- x y)) 0.00001)))
	  )
     (if (close-enough? new-guess guess)
       new-guess
       (fixed-point-iter new-guess)
       )
     )
    )
  (fixed-point-iter 1.0)
  )

(display "=========================")
(newline)
(display (fixed-point cos))
(newline)
(display (fixed-point sin))
(newline)
