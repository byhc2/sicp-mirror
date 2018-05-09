#!/usr/bin/racket
#lang scheme

; 习题3.56
(require racket/include)
(include "stream.scm")

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
      (let ((s1car (stream-car s1))
            (s2car (stream-car s2)))
        (cond
          ((< s1car s2car)
           (cons-stream s1car (merge (stream-cdr s1) s2)))
          ((> s1car s2car)
           (cons-stream s2car (merge s1 (stream-cdr s2))))
          (else
           (cons-stream s1car
                        (merge (stream-cdr s1)
                               (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream 2 S) (merge (scale-stream 3 S)
                                                          (scale-stream 5 S)))))

; 习题3.57
; 使用正确的delay，需要n次计算
; 如果使用不带记忆的delay，则需要2^n - 1次加法
; 因为计算(fibs n)需要计算(fibs n-1)和(fibs n-2)
; 这两个又需要分别递归计算，结果是与普通递归计算一致，复杂度2^n - 1
