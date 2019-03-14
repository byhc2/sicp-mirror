#!/usr/bin/guile
!#

; 习题1.12

; 计算第n层第i个
(define (cal-elem r c)
  (if (or (= c 1) (= c r))
    1
    (+ (cal-elem (- r 1) (- c 1)) (cal-elem (- r 1) c))
    )
  )

; 输出第n行
(define (output-line n)
  (define (iter i)
    (if (> i n)
      (newline)
      (begin
	(display (cal-elem n i))
	(display "  ")
	(iter (+ i 1))
	)
      )
    )
  (iter 1)
  )

; 输出n层
(define (output-triangle n)
  (define (iter i)
    (if (> i n)
      (newline)
      (begin
	(output-line i)
	(iter (+ i 1))
	)
      )
    )
  (iter 1)
  )

(output-triangle 9)
