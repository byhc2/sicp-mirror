#!/usr/bin/guile
!#

; 习题3.52
(add-to-load-path ".")
(load "stream.scm")

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; sum为0
(define y (stream-filter even? seq))
; sum为0
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
; sum为0

(stream-ref seq 7)
; sum为36
(display-stream z)
; sum为210
(newline)

(display sum)
(newline)
