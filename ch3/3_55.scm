#!/usr/bin/racket
#lang scheme

(require racket/include)
(include "stream.scm")

(define (partial-sums stream)
  (let ((d (stream-car stream)))
    (cons-stream d
                 (stream-map (lambda (x) (+ x d))
                              (partial-sums (stream-cdr stream))))))

(define si (partial-sums integers))

(display (stream-ref si 0))
(newline)
(display (stream-ref si 1))
(newline)
(display (stream-ref si 2))
(newline)
(display (stream-ref si 3))
(newline)
(display (stream-ref si 4))
(newline)
(display (stream-ref si 5))
(newline)
