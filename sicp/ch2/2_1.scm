#!/usr/bin/guile
!#

; 习题2.1

(define (make-rat n d)
  (if (or (and (> n 0) (> d 0)) (and (< n 0) (> d 0)))
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))
      )
    (make-rat (- 0 n) (- 0 d))
    )
  )

(define (print-rat x)
  (begin
    (display (car x))
    (display "/")
    (display (cdr x))
    )
  )

(print-rat (make-rat 4 6))
(newline)
(print-rat (make-rat -4 6))
(newline)
(print-rat (make-rat 4 -6))
(newline)
(print-rat (make-rat -4 -6))
(newline)
