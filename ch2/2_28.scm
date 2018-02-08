#!/usr/bin/guile
!#

; 习题2.28

(define (fringe items)
  (define (iter result cur)
    (if (not (list? cur))
      (append result (list cur))
      (if (null? cur)
        result
        (append
          (iter result (car cur))
          (iter result (cdr cur))
          )
        )
      )
    )
  (iter '() items)
  )

(define x (list (list 1 2) 3 4))
(display (fringe (list x x)))
(newline)
