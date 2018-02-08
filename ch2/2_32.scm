#!/usr/bin/guile
!#

; 习题2.32

; 集合子集划分为：
;     除首元素外元素子集
;     上条每个子集加上首元素
(define (subsets s)
  (if (null? s)
    (list (list))
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest))
      )
    )
  )

(display (subsets (list 4)))
(newline)
(display (subsets (list 1 2 3 4)))
(newline)
