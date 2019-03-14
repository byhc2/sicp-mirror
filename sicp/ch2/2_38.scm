#!/usr/bin/guile
!#

; 习题2.38

(define (fold-right op init seq)
  (if (null? seq)
    init
    (op (car seq) (fold-right op init (cdr seq)))
    )
  )

(define (fold-left op init seq)
  (if (null? seq)
    init
    (fold-left op (op (car seq) init) (cdr seq))
    )
  )

(display (fold-right + 0 (list 1 2 3)))
(newline)

(display (fold-left + 0 (list 1 2 3)))
(newline)

; 返回3/2
(display (fold-right / 1 (list 1 2 3)))
(newline)
; 返回3
(display (fold-right / 1 (list 1 2 3)))
(newline)

; 返回 (1 2 3)
(display (fold-right cons (list) (list 1 2 3)))
(newline)
; 返回 (3 2 1)
(display (fold-left cons (list) (list 1 2 3)))
(newline)

; 若要fole-right与fold-left结果相同
; 则  (op (op ... (op x0 init) x1) ... xn)
; 与  (op ... (op xn-1 (op xn init)) ... )
; 相同
; 需要运算符满足交换律，如加法、乘法等

; 习题2.39
(define (reverse2 seq)
  (fold-left cons (list) seq)
  )
(display (reverse2 (list 1 2 3)))
(newline)
