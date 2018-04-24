#!/usr/bin/guile
!#

; 习题3.51
(add-to-load-path ".")
(load "stream.scm")

(define (show x)
  (display-line x)
  x)

;(display (stream-car (stream-enumerate-interval 1 20)))
;(display (stream-enumerate-interval 1 20))
(display-stream (stream-enumerate-interval 1 20))
(newline)
