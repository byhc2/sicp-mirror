#!/usr/bin/guile
!#

; 习题3.2
(define (make-monitored f)
  (let ((value 0))
    (lambda (x)
      (cond
        ((eq? x 'how-many-calls?) value)
        ((eq? x 'reset-count) (begin (set! value 0) 0))
        (else (begin
                (set! value (+ value 1))
                (f x)))))))

(define s (make-monitored sqrt))

(display (s 100))
(newline)
(display (s 20))
(newline)
(display (s 'how-many-calls?))
(newline)
(display (s 'reset-count))
(newline)
(display (s 'how-many-calls?))
(newline)
(display (s 10))
(newline)
