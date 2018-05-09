#!/usr/bin/racket
#lang scheme

; 习题3.58
(require racket/include)
(include "stream.scm")

; 该函数用于展开小于等于1的有理数小数部分
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(stream-take (expand 1 7 10) 10)
(stream-take (expand 3 8 10) 10)

