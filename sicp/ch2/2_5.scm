#!/usr/bin/guile
!#

; 习题2.5

(define (custom-exp b e)
  (define (iter p a n)
    (if (<= n 0)
      p
      (if (even? n)
        (begin
          (iter p (* a a) (/ n 2.0))
          )
        (begin
          (iter (* p a) a (- n 1))
          )
        )
      )
    )
  (iter 1 b e)
  )

(define (custom-cons a b)
  (* (custom-exp 2 a) (custom-exp 3 b))
  )

(define (max-divide x n)
  (define (max-divide-iter e a)
    (if (= (remainder a n) 0)
      (max-divide-iter (+ e 1) (/ a n))
      e
      )
    )
  (max-divide-iter 0 x)
  )

(define (custom-car x)
  (max-divide x 2)
  )

(define (custom-cdr x)
  (max-divide x 3)
  )

(display (custom-cons 2 3))
(newline)
(display (max-divide 16 2))
(newline)
(display (max-divide 9 3))
(newline)

(display "=============================")
(newline)
(display (custom-car (custom-cons 71 89)))
(newline)
(display (custom-cdr (custom-cons 71 89)))
(newline)

(display "=============================")
(newline)
(display (custom-car (custom-cons 0 89)))
(newline)
(display (custom-cdr (custom-cons 0 89)))
(newline)
