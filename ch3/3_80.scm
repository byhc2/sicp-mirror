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

(define (RLC-META R C L dt)
  (define (RLC vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1 C)))
    (define dil (add-streams (scale-stream vc (/ 1 L))
                             (scale-stream il (/ (- 0 R) L))))
    (stream-map cons vc il))
  RLC)

(define rlc1 (RLC-META 1 0.2 1 0.1))
(display-stream-n (rlc1 0.1 0.2) 100)
