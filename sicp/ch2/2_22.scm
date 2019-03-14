#!/usr/bin/guile
!#

; 习题2.23

(define (foreach f r)
  (if (null? r)
    #t
    (begin
      (f (car r))
      (foreach f (cdr r))
      )
    )
  )

(foreach (lambda (x) (display x)) (list 1 2 3 4))

; 习题2.24
; (1 (2 (3 4)))
