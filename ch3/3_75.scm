#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

(define (sign-change-detector value last-value)
  (cond
    ((and (< last-value 0) (>= value 0)) 1)
    ((and (>= last-value 0) (< value 0)) -1)
    (else 0)))

; 习题3.75
;(define (make-zero-crossings input-stream last-value)
;  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
;    (cons-stream (sign-change-detector avpt last-value)
;                 (make-zero-crossings (stream-cdr input-stream)
;                                      avpt))))
;
;(define (zero-crossings sense-data) (make-zero-crossings sense-data 0))


; 题目要求流中每一项，与前一项均值平滑，作为新流
; 而上述算法，每次迭代avpt是：
;    x1
;    -- + x2
;     2
;    ------- + x3
;       2
;    ------------ + x4
;          2
;    -----------------
;            ...
; 第一次是x1/2，第二次是...依次类推
; 故需要增加一个参数
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream) avpt))))

(define (zero-crossings sense-data) (make-zero-crossings sense-data 0 0))

; 第一次是x1/2，第二次是(x1 + x2)/2，依次类推
