#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

; 习题3.78
(define (solve-2nd-1 a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)
(stream-ref (solve-2nd-1 1 1 0.00001 0 0) 100000)

; 习题3.80
(define (solve-2nd-2 f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)
(stream-ref (solve-2nd-2 (lambda (x y) (+ x y)) 0 0 0.00001) 100000)
