#!/usr/bin/guile
!#

; 习题2.3

(define (rectangle w h)
  (cons w h)
  )

(define (rectangle-perimeter r)
  (+ (* (car r) 2) (* (cdr r) 2))
  )

(define (rectangle-area r)
  (* (car r) (cdr r))
  )

(display (rectangle-area (rectangle 3 4)))
(newline)
(display (rectangle-perimeter (rectangle 3 4)))
(newline)
