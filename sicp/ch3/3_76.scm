#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

(define (sign-change-detector value last-value)
  (cond
    ((and (< last-value 0) (>= value 0)) 1)
    ((and (>= last-value 0) (< value 0)) -1)
    (else 0)))

(define (make-zero-crossings sense-data)
  (define (sign-change-detector value last-value)
    (cond
      ((and (< last-value 0) (>= value 0)) 1)
      ((and (>= last-value 0) (< value 0)) -1)
      (else 0)))
  (stream-map sign-change-detector sense-data
              (cons-stream (stream-car sense-data)
                           sense-data)))

; 习题3.76

; 生成平滑流
(define (smooth input-stream)
  (stream-map (lambda (x y) (/ (+ x y) 2))
              input-stream
              (cons-stream (stream-car input-stream)
                           input-stream)))

(define (zero-crossings sense-data)
  (make-zero-crossings (smooth sense-data)))

(define t (integers-starting-from -3))
(display-stream-n (zero-crossings t) 10)
