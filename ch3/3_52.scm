#!/usr/bin/guile
!#

; 习题3.52
(add-to-load-path ".")
(load "stream.scm")

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 10)))

(display seq)
(newline)
