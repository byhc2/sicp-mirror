#!/usr/bin/guile
!#

; 习题3.1
(define (make-accumulator init)
  (let ((value init))
    (lambda (x)
      (begin
        (set! value (+ value x))
        value))))

(define A (make-accumulator 5))
(display (A 10))
(newline)
(display (A 10))
(newline)
