#!/usr/bin/guile
!#

; 习题1.15

(define (cubic x) (* x x x))

(define (_sin x)
  (define (p x)
    (- (* 3.0 x) (* 4.0 (cubic x)))
    )
  (if (< x 0.0001)
    x
    (p (_sin (/ x 3.0)))
    )
  )

(display (_sin 2))
(newline)

; 计算(_sin 12.15)调用p 5次
; 计算(_sin a)时，调用p为上取整$log_3^(10a)$次，故其时间复杂度为O(log_3^(10a))，空间复杂度O(log_3^(10a))
