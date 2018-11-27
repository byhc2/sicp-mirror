#!/usr/bin/racket
#lang scheme

(define (fun p)
  (cond ((eq? p #t) (display 'xxxxx))
        (else (display 'yyyyy)
              (newline)
              (display 'zzzzzz))))

(fun #f)
(newline)
