#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

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
