#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

(define random-init (random))

; 习题3.81
(define (random-stream num action-stream)
  (define (f m)
    (cond
      ((eq? m 'generate)
       (random))
      ((eq? m 'reset)
       (random-seed num))))
  (define r
    (cons-stream random-init
                 (stream-map f action-stream)))
  (begin
    (random-seed num)
    (stream-cdr r)))

(define c (cons-stream 'generate
                       (cons-stream 'generate
                                    (cons-stream 'reset
                                                 (cons-stream 'generate
                                                              (cons-stream 'generate
                                                                           the-empty-stream))))))

(display-stream (random-stream 100 c))
