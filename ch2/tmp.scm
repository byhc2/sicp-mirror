#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (generic-arithmetic))

(define x (make-sparse-poly 'x '((2 1) (1 2))))
(display x)
(newline)

(display (add x x))
(newline)
