#!/usr/bin/guile
!#

; 习题2.35

(define (accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq) (accumulate op initial (cdr seq)))
    )
  )

(define (enumerate-tree t)
  (if (null? t)
    (list)
    (if (not (list? t))
      (list t)
      (append (enumerate-tree (car t)) (enumerate-tree (cdr t)))
     )
    )
  )

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t)))
  )

(display (enumerate-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))
(newline)
(display (count-leaves (list 1 (list 2 (list 3 4) 5) (list 6 7))))
(newline)
