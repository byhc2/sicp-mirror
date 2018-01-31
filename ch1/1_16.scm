#!/usr/bin/guile
!#

; 习题1.16

; 状态量思路：任何时候power-iter的参数a都满足a * power-fun(prod remain) = power-fun(b n)
; 以b的7次方为例：
; 1     b       7
; b     b       6
; b     b^2     3
; b^3   b^2     2
; b^3   b^4     1
; b^7   b^4     0

(define (power-fun b n)
  (define (odd x)
    (= (remainder x 2) 1)
    )
  (define (power-iter a prod remain)
    (if (= 0 remain)
      a
      (if (odd remain)
        (power-iter (* a prod) prod (- remain 1))
        (power-iter a (* prod prod) (/ remain 2))
        )
      )
    )
  (power-iter 1 b n)
  )

(display (power-fun 2 6))
(newline)
