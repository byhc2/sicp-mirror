#!/usr/bin/guile
!#

; 习题1.11

(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))
    )
  )


(define (f2 n)
  (define (f2-iter i x y z)
    (if (>= i n)
      x
      (f2-iter (+ 1 i) (+ x (* 2 y) (* 3 z)) x y)
      )
    )
  (f2-iter 2 2 1 0)
  )

(define (test n)
  (define (iter i)
    (if (> i n)
      (newline)
      (begin
	(display (f i))
	(newline)
	(iter (+ i 1))
	)
      )
    )
  (iter 3)
  )

(display (f 4))
(newline)
(display (f2 4))
(newline)
(display "=========================")
(newline)
(display (test 10))
