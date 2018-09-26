#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

(define (experiment-stream experiment)
  (cons-stream (experiment)
               (experiment-stream experiment)))

(define (rand-in-range x1 x2)
  (+ (* (random) (- x2 x1)) x1))

(define (estimate-integral predictor x1 x2 y1 y2)
  (define one-trial
    (lambda ()
      (let ((x (rand-in-range x1 x2))
            (y (rand-in-range y1 y2)))
        (predictor x y))))
  (experiment-stream one-trial))

(define (predicate x y)
  (< (+ (* x x) (* y y)) 1))

(display-stream-n (estimate-integral predicate -1.0 1.0 -1.0 1.0) 100)

;(display-stream-n (experiment-stream (lambda () 1)) 10)
