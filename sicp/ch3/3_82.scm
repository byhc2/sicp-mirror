#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

; 习题3.82
; experiment-stream构造单次实验的结果流
; (#t #t #f #f #f #t #f ...)
; operator将结果流转化为数字。第一项固定1方便相加
; ((1 1.0) (1 1.0) (1 0.0) ...)
; 上述流错位相加
; 则越靠后计算结果越精确
; 实测收敛很慢
(define (experiment-stream experiment)
  (cons-stream (experiment)
               (experiment-stream experiment)))

(define (rand-in-range x1 x2)
  (+ (* (random) (- x2 x1)) x1))

(define (monte-carlo experiment)
  (define (operator arg)
    (if (eq? #t arg)
        (cons 1 1.0)
        (cons 1 0.0)))
  (let ((exp-stream (stream-map operator (experiment-stream experiment))))
    (define int (cons-stream (stream-car exp-stream)
                             (stream-map (lambda (x y) (cons (+ (car x) (car y))
                                                             (+ (cdr x) (cdr y))))
                                         (stream-cdr exp-stream) int)))
    (stream-map (lambda (x) (/ (cdr x) (car x))) int)))

(define (estimate-integral predictor x1 x2 y1 y2)
  (define one-trial
    (lambda ()
      (let ((x (rand-in-range x1 x2))
            (y (rand-in-range y1 y2)))
        (predictor x y))))
  (monte-carlo one-trial))

(define (predicate x y)
  (< (+ (* x x) (* y y)) 1))

(display (* (stream-ref (estimate-integral predicate -1.0 1.0 -1.0 1.0)
                        10000000)
            4))
(newline)

;(display-stream-n (experiment-stream (lambda () 1)) 10)
