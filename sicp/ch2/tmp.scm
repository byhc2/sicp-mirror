#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (generic-arithmetic))

(define x (make-sparse-poly 'x '((2 1) (1 2))))
(display (equ? x x))
(newline)

(display x)
(newline)

(display (variable x))
(newline)

(display (add x x))
(newline)

(display (mul x x))
(newline)

(display (=zero? (sub x x)))
(newline)

(define y (make-sparse-poly 'x '((5 1) (0 -1))))
(define z (make-sparse-poly 'x '((2 1) (0 -1))))
(display (div y z))
(newline)
