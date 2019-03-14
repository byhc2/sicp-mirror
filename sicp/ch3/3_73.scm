#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

; 积分函数
; integrand是函数f(x)自某点开始之值所成之流
; 曰f(x) f(x+dt) f(x+2dt) f(x+3dt) ...
; 积分过程，以f(x+ndt)乘以dt，求得面积流
; 曰f(x)dt f(x+dt)dt f(x+2dt)dt f(x+3dt)dt ...
; add-streams以面积流错位相加，成积分流
; f(x)dt f(x+dt)dt f(x+2dt)dt f(x+3dt)dt f(x+4dt)dt ...
;        f(x)dt    f(x+dt)dt  f(x+2dt)dt f(x+3dt)dt ...
;                  f(x)dt     f(x+dt)dt  f(x+2dt)dt ...
;                             f(x)dt     f(x+dt)dt  ...
;                                        f(x)dt     ...
; 上表每列求和得积分流
; 余以为此算法不妥。其一，积分定义为给定被积函数与自变量区间
; 求被积函数在区间上所围面积，此算法无自变量区间输入。其二，
; 积分初始量应隐含于被积函数中。其三，dt指示积分步长，与被积
; 函数所成流相关。故入参应为被积函数本身，积分上下限，积分步
; 长
; 余自定义积分函数如下
;(define (integral2 integrand-fun lower-bounds upper-bounds dt)
;  (define (step x) (cons-stream x
;                                (if (> (+ x dt) upper-bounds)
;                                    the-empty-stream
;                                    (step (+ x dt)))))
;  (define integrand (stream-map integrand-fun (step lower-bounds)))
;  (define int (let ((tmp (scale-stream integrand dt)))
;                (cons-stream (stream-car tmp) (add-streams (stream-cdr tmp) int))))
;  int)
; 使用(display-stream (integral2 (lambda (x) (* x x)) 0 1 0.00001))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define x integers)
; (display-stream-n (integral x 2 0.1) 10)

(define dt 0.0001)
(define (step x)
  (cons-stream x (step (+ x dt))))
(define dt-series (stream-map (lambda (x y) (* x y)) (step 0) (step 0)))

;(display-stream-n dt-series 10)

; 此处以dt为步长，构造了0到1之间的x^2序列
; 如0 0.0001^2 0.0002^2 ...
; 然后调用integral计算其积分值
; 随dt减小，序列最后一项无限趋近于1/3
(display-stream-n (integral dt-series 0 dt) (/ 1 dt))


; 习题3.73
(define (circuit R C dt)
  (define (Ri i-stream)
    (scale-stream i-stream R))
  (define (int-i i-stream)
    (scale-stream
      (integral i-stream (stream-car i-stream) dt) (/ 1 C)))
  (define (RC v0 i-stream)
    (stream-map (lambda (x) (+ x v0))
                (add-streams (Ri i-stream) (int-i i-stream))))
  RC)

(define v0 0.2)
(define i-stream (step 0.1))
(define RC1 (circuit 5 1 0.5))
(display-stream-n (RC1 v0 i-stream) 10)
