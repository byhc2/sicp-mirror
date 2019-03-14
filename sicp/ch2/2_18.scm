#!/usr/bin/guile
!#

; 习题2.18

(define (reverse_ l)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (cons (car rest) result) (cdr rest))
      )
    )
  (iter (list) l)
  )

(display (reverse_ (list 1 2 3 4)))
(newline)
