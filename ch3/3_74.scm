#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

; 习题3.74
(define (zero-crossings sense-data)
  (define (sign-change-detector value last-value)
    (cond
      ((and (< last-value 0) (>= value 0)) 1)
      ((and (>= last-value 0) (< value 0)) -1)
      (else 0)))
  (stream-map sign-change-detector sense-data
              (cons-stream (stream-car sense-data)
                           sense-data)))

(define t (integers-starting-from -3))
(display-stream-n (zero-crossings t) 10)
