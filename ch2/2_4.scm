#!/usr/bin/guile
!#

; 习题2.4

(define (custom-cons x y)
  (lambda (m) (m x y))
  )

(define (custom-car v)
  (v (lambda (p q) p))
  )

(define (custom-cdr v)
  (v (lambda (p q) q))
  )

; 代换模型解释
;(custom-car v)
;(custom-car (custom-cons x y))
; cons返回一函数，该函数需一参数，此参数为一双参数函数
; car/cdr调用cons返回之函数，传入一双参数函数
; (     (lambda (m) (m x y))    (lambda (p q) p)      )

(display (custom-car (custom-cons 4 2)))
(newline)
(display (custom-cdr (custom-cons 4 2)))
(newline)
