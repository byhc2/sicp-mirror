#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                             int)))
  int)

(define x integers)
(display-stream-n (integral x 2 0.1) 10)
