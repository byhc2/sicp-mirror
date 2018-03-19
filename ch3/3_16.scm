#!/usr/bin/guile
!#

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(display (count-pairs (list 'a 'b 'c)))
(newline)

; 习题3.16

(define x (cons 'a 'b))
(define y (cons 'a 'b))
(define z (cons x y))
(set-car! x y)

(display (count-pairs z))
(newline)

(define x (cons 'a 'b))
(define y (cons x x))
(define z (cons y y))
(display (count-pairs z))
(newline)

(define x (cons 'a 'b))
(define y (cons 'a 'b))
(define z (cons x y))
(set-cdr! x y)
(set-cdr! y x)
(display (count-pairs z))
(newline)
