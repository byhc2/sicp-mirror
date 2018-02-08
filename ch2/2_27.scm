#!/usr/bin/guile
!#

; 习题2.27

(define (deep-reverse items)
  (if (and (list? items) (not (null? items)))
    (let ((head (car items))
          (tail (cdr items)))
      (append (deep-reverse tail) (list (deep-reverse head)))
      )
    items
    )
  )

(display (deep-reverse (list 1 2 3 4)))
(newline)
(display (deep-reverse '((1 2) (3 4) ())))
(newline)
