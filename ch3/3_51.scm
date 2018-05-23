#!/usr/bin/racket
#lang racket

; 习题3.51
(require "stream.scm")

(define (show x)
  (display-line x)
  x)

;(display (cdr (stream-enumerate-interval 1 20)))
;(display (stream-enumerate-interval 1 20))
;(display-stream (stream-enumerate-interval 1 20))
;(newline)

(define x (stream-map show (stream-enumerate-interval 0 20)))
(display (stream-ref x 5))
(display (stream-ref x 7))

