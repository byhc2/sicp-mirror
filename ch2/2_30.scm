#!/usr/bin/guile
!#

; 习题2.30

(define (square-list tree)
  (if (not (list? tree))
    (* tree tree)
    (if (null? tree)
      tree
      (cons (square-list (car tree)) (square-list (cdr tree)))
      )
    )
  )

(display (square-list (list 1 (list 2 (list 3 4) 5) (list 6 7))))
(newline)
