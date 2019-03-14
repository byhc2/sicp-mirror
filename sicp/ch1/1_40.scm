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
	  (close-enough? (lambda (x y) (< (abs (- x y)) 0.000001)))
	  )
     (if (close-enough? new-guess guess)
       new-guess
       (fixed-point-iter new-guess)
       )
     )
    )
  (fixed-point-iter 1.5)
  )

(display "=========================")
(newline)
(display (fixed-point cos))
(newline)
(display (fixed-point sin))
(newline)

(define (newton-method f)
  (fixed-point (lambda (x) (- x (/ (f x) ((derive f) x)))))
  )

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c))
  )

(display "=========================")
(newline)
(display (newton-method (cubic 0 -1 0)))
(newline)
