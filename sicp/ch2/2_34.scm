#!/usr/bin/guile
!#

; 习题2.33

(define (accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq) (accumulate op initial (cdr seq)))
    )
  )

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff)) 0 coefficient-sequence)
  )

(display (horner-eval 2 (list 1 1 1 1)))
(newline)
