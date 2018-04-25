#!/usr/bin/guile
!#

; 习题3.51
(add-to-load-path ".")
(load "stream.scm")

(define (show x)
  (display-line x)
  x)

;(display (cdr (stream-enumerate-interval 1 20)))
;(display (stream-enumerate-interval 1 20))
;(display-stream (stream-enumerate-interval 1 20))
;(newline)

(define x0 (stream-enumerate-interval 0 3))
(define x (stream-map0 show x0))
;(stream-ref x 5)
;(stream-ref x 7)

;(define x0 (stream-enumerate-interval 0 3))
;(display (car x0))
;(newline)
;
;(define x1 (force (cdr x0)))
;(display (car x1))
;(newline)
;
;(define x2 (force (cdr x1)))
;(display (car x2))
;(newline)
;
;(define x3 (force (cdr x2)))
;(display (car x3))
;(newline)
;
;(define x4 (force (cdr x3)))
;(display x4)
;(newline)
