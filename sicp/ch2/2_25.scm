#!/usr/bin/guile
!#

; 习题2.25

(display (car (cdr (car (cdr (cdr '(1 3 (5 7) 9)))))))
(newline)
(display (car (car '((7)))))
(newline)
(display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7)))))))))))))))))))
(newline)

; 习题2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(display (append x y))
(newline)
(display (cons x y))
(newline)
(display (list x y))
(newline)
