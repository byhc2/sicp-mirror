#!/usr/bin/guile
!#

; 习题2.21

(define (square-list-1 items)
  (if (null? items)
    #nil
    (cons (* (car items) (car items)) (square-list-1 (cdr items)))
    )
  )

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items)
  )

(display (square-list-1 (list 1 2 3 4)))
(newline)
(display (square-list-2 (list 1 2 3 4)))
(newline)

; 习题2.22
; 盖因cons构造列表时，只能以(cons value sublist)形式构造
; 其二代码，将得到(<list> value)序对
