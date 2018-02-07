#!/usr/bin/guile
!#

; 习题2.17

(define (last-pair l)
  (define (iter rest)
    (if (null? (cdr rest))
      (list (car rest))
      (iter (cdr rest))
      )
    )
  (iter l)
  )

(display (last-pair (list 1 2 3 4)))
(newline)
