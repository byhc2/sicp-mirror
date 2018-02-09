#!/usr/bin/guile
!#

; 习题2.37

(define (accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq) (accumulate op initial (cdr seq)))
    )
  )

; 行列点积
(define (dot-product u v)
  (accumulate + 1.0 (map * u v))
  )

(define v (list 1 2 3))

(display (dot-product v v))
(newline)

; 矩阵乘列
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m)
  )

(define m (list (list 1 2 3) (list 2 3 4) (list 3 4 5) (list 4 5 6)))

(display (matrix-*-vector m v))
(newline)

(define (transpose m)
  (if (null? (car m))
    (list)
    (cons (map car m) (transpose (map cdr m)))
    )
  )

(display (transpose m))
(newline)

; 矩阵相乘
(define (matrix-*-matrix m n)
  (map (lambda (x) (matrix-*-vector m x)) (transpose n))
  )

(display (matrix-*-matrix m (transpose m)))
(newline)
