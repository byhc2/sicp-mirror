#!/usr/bin/racket
#lang scheme

(require racket/include)
(include "stream.scm")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream
                     1
                     (mul-streams
                       (stream-cdr integers)
                       factorials)))

(display (stream-ref factorials 0))
(newline)
(display (stream-ref factorials 1))
(newline)
(display (stream-ref factorials 2))
(newline)
(display (stream-ref factorials 3))
(newline)
(display (stream-ref factorials 4))
(newline)
(display (stream-ref factorials 5))
(newline)
(display (stream-ref factorials 6))
(newline)
(display (stream-ref factorials 7))
(newline)
(display (stream-ref factorials 8))
(newline)
(display (stream-ref factorials 9))
(newline)
(display (stream-ref factorials 10))
(newline)
