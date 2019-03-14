#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.00001) 100000)

; 习题3.77
; 错误的形式
;(define (integral2 delayed-integrand initial-value dt)
;  (let ((integrand (force delayed-integrand)))
;    (cons-stream initial-value
;                 (if (stream-null? integrand)
;                     the-empty-stream
;                     (integral2 (delay (stream-cdr integrand))
;                                (+ (* dt (stream-car integrand))
;                                   initial-value)
;                                dt)))))
; integral2应返回一流，此流用于计算dy
; dy反过来再用于integral2之delayed-integrand参数
; 故先求值delayed-integrand要求dy可计算
; 而dy可计算要求integral2返回流可计算
; 故先求值再构造流导致以上二者之一不可计算
; 应先构造流，使流中第一项可计算
(define (integral2 delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral2 (delay (stream-cdr integrand))
                                (+ (* dt (stream-car integrand))
                                   initial-value)
                                dt)))))

(define (solve2 f y0 dt)
  (define y (integral2 (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
(stream-ref (solve2 (lambda (y) y) 1 0.00001) 100000)
