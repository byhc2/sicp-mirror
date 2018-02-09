#!/usr/bin/guile
!#

; 习题2.33

(define (accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq) (accumulate op initial (cdr seq)))
    )
  )

; 以下方法错误
;(define (accumulate op initial seq)
;  (if (null? seq)
;    initial
;    (accumulate op (op (car seq) initial) (cdr seq))
;    )
;  )

(display (accumulate + 0 (filter (lambda (x) (= 0 (remainder x 2))) (list 1 2 3 4))))
(newline)

(define (map2 p seq)
  (accumulate (lambda (x y) (cons (p x) y)) (list) seq)
  )

(display (map2 (lambda (x) (* x x)) (list 1 2 3 4)))
(newline)

(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1)
  )

(display (append2 (list 1 3 2) (list 2 4 5)))
(newline)

(define (length2 seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq)
  )

(display (length2 (list 1 2 3 4)))
(newline)
