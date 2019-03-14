#!/usr/bin/guile
!#

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

; 习题3.14
; 此处宜以v为全局列表，参数传递实为指针传递
(define v (list 'a 'b 'c 'd))
(define w (mystery v))
(display v)
(newline)
(display w)
(newline)

; 习题3.15 暂略
