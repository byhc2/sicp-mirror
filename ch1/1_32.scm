#!/usr/bin/guile
!#

; 习题1.32

(define (accumulate combiner init-value term a b next)
  (define (accumulate-iter acc cur)
    (if (> cur b)
      acc
      (accumulate-iter (combiner acc (term cur)) (next cur))
      )
    )
  (accumulate-iter init-value a)
  )

(define (plus a b) (+ a b))
(define (prod a b) (* a b))
(define (identity x) x)
(define (inc x) (+ x 1))

(define (sum a b)
  (accumulate plus 0 identity a b inc)
  )

(define (factorial a b)
  (accumulate prod 1 identity a b inc)
  )

(display "1到100和 ")
(display (sum 1 100))
(newline)
(display "1到10积 ")
(display (factorial 1 10))
(newline)
