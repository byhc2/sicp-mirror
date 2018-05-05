#!/usr/bin/racket
#lang scheme

; 习题3.53
;(add-to-load-path ".")
;(load "./stream.scm")
(require racket/include)
(include "stream.scm")

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define x (cons-stream 1 (add-streams x x)))

(display (stream-ref x 0))
(newline)
(display (stream-ref x 1))
(newline)
(display (stream-ref x 2))
(newline)
(display (stream-ref x 3))
(newline)
