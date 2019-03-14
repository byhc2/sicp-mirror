#!/usr/bin/guile
!#

; 习题1.37

(define (cont-frac n d k)
  (define (cont-frac-iter i)
    (if (>= i k)
      (d i)
      (+ (d i) (/ (n (+ i 1)) (cont-frac-iter (+ i 1.0))))
      )
    )
  (/ (n 1) (cont-frac-iter 1))
  )

; k至少需要11
(display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10000))
(newline)

; 迭代版本
(define (cont-frac2 n d k)
  (define (cont-frac-iter result i)
    (if (= i 1)
      (/ (n i) result)
      (cont-frac-iter (+ (d (- i 1)) (/ (n i) result)) (- i 1))
      )
    )
  (cont-frac-iter (d k) k)
  )

(display (cont-frac2 (lambda (i) 1.0) (lambda (i) 1.0) 10000))
(newline)
