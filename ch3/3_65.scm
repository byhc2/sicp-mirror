#!/usr/bin/racket
#lang scheme

(require racket/include)
(include "stream.scm")
(include "series.scm")

; 习题3.65
; 注意交错流的生成方式
(define (ln2-part n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-part (+ n 1)))))

(define x (partial-sums (ln2-part 1.0)))

; 序列1
; 1000项只精确到小数点后2位
(stream-take x 1000)

; 序列2
