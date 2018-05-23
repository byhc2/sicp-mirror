#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

; 习题3.65
; 注意交错流的生成方式
(define (ln2-part n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-part (+ n 1)))))

(define x (partial-sums (ln2-part 1.0)))

; 序列1
; 1000项只精确到小数点后3位
(display (stream-ref x 10))
(newline)

; 序列2
(display (stream-ref (euler-transform x) 10))
(newline)

; 序列3
; 第9项时已经得到可表示范围内最精确数字
; 问题在于不能超过10，超过10会有问题
; 猜测是因为，序列前中后三项相等（机器表示精度导致）
; 导致加速序列后项分母为0
(display (stream-ref (accelerated-sequence euler-transform x) 9))
(newline)
