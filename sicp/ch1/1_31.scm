#!/usr/bin/guile
!#

; 习题1.31

(define (general-product term a b next)
  (define (general-product-iter prod cur)
    (if (> cur b)
      prod
      (general-product-iter (* prod (term cur)) (next cur))
      )
    )
  (general-product-iter 1 a)
  )

(define (identity x) x)
(define (inc x) (+ x 1))

(display "1到10阶乘 ")
(display (general-product identity 1 10 inc))
(newline)

(define (pi-term-numerator i)
  (if (odd? i)
    (+ i 1)
    (+ i 2)
    )
  )

(define (pi-term-denominator i)
  (if (odd? i)
    (+ i 2)
    (+ i 1)
    )
  )

(define (pi-term i)
  (/ (pi-term-numerator i) (pi-term-denominator i))
  )

(display "计算pi ")
(display (* 4 (general-product pi-term 1.0 10000000.0 inc)))
(newline)

; 上述代码已经是迭代过程，迭代改递归略
