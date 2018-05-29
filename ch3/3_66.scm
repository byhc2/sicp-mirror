#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

; 习题3.66

(define x (pairs integers integers))

(display-stream-n x 31)
(newline)

; 根据前10项输出结果
; (1 1)
; (1 2)
; (2 2)
; (1 3)
; (2 3)
; (1 4)
; (3 3)
; (1 5)
; (2 4)
; (1 6)
; 可以看到，每个(1 x)前面会出现x - 2个(a b)，其中a不是1，以及x - 1个(1 c)
; 所以，(1 100)前将出现98 + 99 = 197条
; 验证可得(1 100)确实出现在198条
;
; 输出前20项结果，观察(2 2) (3 3) (4 4)前面的项数，分别是2 6 14，大约符合2^n -
; 2的规律。猜想(5 5)前应出现2^5 - 2 = 30项，验证(5 5)确实出现在第31项。故(100
; 100)出现在2^100 - 2 + 1项：1267650600228229401496703205374
; 算到地老天荒了吧？

;(define x (pairs3 integers integers))
;(display (stream-ref x 0))
