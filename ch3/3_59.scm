#!/usr/bin/racket
#lang scheme

; 习题3.59
(require racket/include)
(include "stream.scm")

(define (mul-stream s1 s2)
  (stream-map * s1 s2))

(define (integrate-series s)
  (let ((coef (stream-map / ones integers)))
    (mul-stream coef s)))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (add-streams . args)
  (apply stream-map + args))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                 (scale-stream (stream-cdr s2) (stream-car s1))
                 (scale-stream (stream-cdr s1) (stream-car s2))
                 (mul-series (stream-cdr s1) (stream-cdr s2)))))


(stream-take
  (add-streams
    (mul-series sine-series sine-series)
    (mul-series cosine-series cosine-series))
  10)
