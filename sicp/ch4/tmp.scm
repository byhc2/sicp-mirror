#!/usr/bin/racket
#lang scheme

(define (fun x)
  (if (> x 0)
      (begin
        (display x)
        (newline)
        (fun (- x 1)))
      (newline)))

(fun 20)
