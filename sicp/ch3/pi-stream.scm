#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream (scale-stream (partial-sums (pi-summands 1)) 4))

;(display (stream-ref pi-stream 1000))
;(newline)
(display-stream-n pi-stream 10)

(display-stream-n (euler-transform pi-stream) 10)
;(display (stream-ref (euler-transform pi-stream) 1000))
;(newline)
