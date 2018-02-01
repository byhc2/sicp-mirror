#!/usr/bin/guile
!#

; 习题1.23

(define (smallest-divisor n)
  (define (next x)
    (if (= x 2)
      3
      (+ x 2)
      )
    )
  (define (square x)
    (* x x)
    )
  (define (find-divisor x)
    (if (> (square x) n)
      n
      (if (= (remainder n x) 0)
	x
	(find-divisor (next x))
	)
      )
    )
  (find-divisor 2)
  )

(display (smallest-divisor 199))
(newline)
(display (smallest-divisor 1999))
(newline)
(display (smallest-divisor 19999))
(newline)

; 实际运行程序并未得到可比较的结果
; 怀疑是数字过小或机器其他任务的负载导致结果浮动
