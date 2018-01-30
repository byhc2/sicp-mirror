#!/usr/bin/guile
!#

; 习题1.8

(define (good-enough x y)
  (< (/ (abs (- x y)) y)
     0.0000000001
     )
  )

(define (update x y)
  (/ (+ (/ x (* y y)) (* 2 y)) 3)
  )

(define (cubic-root-iter x y y0)
  (if (good-enough (update x y) y0)
    (update x y)
    (cubic-root-iter x (update x y) y)
    )
  )

(define (cubic-root x)
  (cubic-root-iter x 1.0 0.1)
  )

(display (cubic-root 2))
(newline)
(display (cubic-root 8))
(newline)

; 习题1.9
;(define (+ a b)
;  (if (= a 0)
;    b
;    (inc (+ (dec a) b))
;    )
;  )
; 一是递归过程，因为其展开式中，需要保存调用inc的数量，随着a的增大而增大
;(define (+ a b)
;  (if (= a 0)
;    b
;    (+ (dec a) (inc b))
;    )
;  )
; 二为迭代过程，因其每一步都可以用a b两个量来表示
