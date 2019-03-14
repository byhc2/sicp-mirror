#!/usr/bin/guile
!#

(define (append! x y)
  (set-cdr! (last-pair x) y))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; 习题3.12
; response1: (b)
; response2: (b c d)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

(display z)
(newline)
(display (cdr x))
(newline)

(define w (append! x y))
(display w)
(newline)

(display (cdr x))
(newline)
