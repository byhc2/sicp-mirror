#!/usr/bin/guile
!#

(add-to-load-path ".")
(load "constrains.scm")

(define a (make-connector))
(define b (make-connector))

(define (my-squarer a b)
  (multiplier a a b)
  'ok)

(probe "Squarer" a)
(my-squarer a b)
(set-value! b 100 'user)

; 习题3.34
; 设置b的值时，不能求出a的值
; 因multiplier中，需要知道2个值
; (multiplier a a b)中，知道b，只有一个值

(newline)
