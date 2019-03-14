#!/usr/bin/guile
!#

; 习题2.31

(define (tree-map f t)
  (map (lambda (subtree) 
        (if (not (list? subtree))
          (f subtree)
          (tree-map f subtree)
          )
        )
       t)
  )

(define (square x) (* x x))
(display (tree-map square (list 1 (list 2 (list 3 4) 5) (list 6 7))))
(newline)
