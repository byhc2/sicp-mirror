#!/usr/bin/guile
!#

; 习题1.29

(define (general-sum term a b next)
  (define (general-sum-iter sum cur)
    (if (<= cur b)
      (general-sum-iter (+ sum (term cur)) (next cur))
      sum
      )
    )
  (general-sum-iter 0 a)
  )

(define (identity x) x)
(define (inc x) (+ x 1))

(display (general-sum identity 1 100 inc))
(newline)

(display "==========================================")
(newline)

(define (integeral f a b)
  (define dx 0.001)
  (define (next x)
    (+ x dx))
  (define (step x)
    (* (f x) dx))
  (general-sum step a b next)
  )

(define (cube x) (* x x x))

(display (integeral identity 0.0 2.0))
(newline)
(display (integeral cube 0.0 1.0))
(newline)

(define (integeral2 f a b)
  (define n 1000000.0)
  (define h (/ (- b a) n))
  (define (next x)
    (+ x 1)
    )
  (define (term i)
    (if (even? i)
      (* 2.0 (f (+ a (* i h))))
      (* 4.0 (f (+ a (* i h))))
      )
    )
  (/ (* h (+ (f a) (general-sum term 1 n next) (f b))) 3.0)
  )

(display "==========================================")
(newline)
(display (integeral2 identity 0.0 2.0))
(newline)
(display (integeral2 cube 0.0 1.0))
(newline)

; 习题1.30 见本文件 general-sum
