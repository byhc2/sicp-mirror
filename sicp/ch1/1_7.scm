#!/usr/bin/guile
!#

; 习题1.7
; 书中的good-enough方法
; 其一需要反复计算平方
; 其二因浮点精度，对较大数，good-enough可能永不满足，无限递归
; 其三因浮点精度，对较小数，good-enough可能很快满足，结果不精确

(define (square x) (* x x))

;(define (good-enough x y)
;	(< (abs (- (square y) x))
;		0.000000000000001
;	)
;)

(define (update x y)
  (/ (+ y (/ x y)) 2.0)
  )

(define (good-enough x y y0)
  (< (/ (abs (- (update x y) y0)) y0) 0.0000000000000000000001)
  )

(define (sqrt-iter x y y0)
  (if (good-enough x y y0)
    y
    (sqrt-iter x (update x y) y)
    )
  )

(define (square-root x)
  (sqrt-iter x 1.0 0.1)
  )

(display (square-root 900000000000000000000000000000000000000000000000000000000000000000000000000000000000))
(newline)
