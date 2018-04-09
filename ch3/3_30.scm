#!/usr/bin/guile
!#

; 习题3.30

(add-to-load-path ".")
(load "logical.scm")

; A、B输入信号
; S输出信号
; c最后进位
(define (ripple-carry-adder A B S c)
  (define (iter xA xB c-out xS)
    (if (not (null? xA))
        (let (c-in (make-wire))
          (full-adder (car xA) (car xB) c-in (car S) c-out)
          (iter (cdr xA) (cdr xB) c-in (cdr xS))))))

; 半加器时延为二或门时延加一非门时延
; 全加器时延为二半加器时延加一或门时延
; n位加法器时延为n全加器时延
; 代入可得
