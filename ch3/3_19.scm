#!/usr/bin/guile
!#

; 通过步长不一致的遍历来确定
; 如果某个节点的car也是一个列表，则递归校验
(define (has-loop? lst)
  (define (safe-cdr x)
    (if (pair? x)
        (cdr x)
        #nil))
  (define (iter x y)
    (cond
      ((or (eq? x #nil) (eq? y #nil)) #f)
      ((eq? x y) #t)
      (else
        (or
          (if (and (pair? x) (pair? (car x)))
              (has-loop? x)
              #f)
          (if (and (pair? y) (pair? (car y)))
              (has-loop? y)
              #f)
          (iter (safe-cdr x) (safe-cdr (safe-cdr y)))))))
  (iter (safe-cdr lst) (safe-cdr (safe-cdr lst))))

; [* *]
;  | |
;  v v
;  a b
(define x (cons 'a 'b))
(display (has-loop? x))
(newline)

; [*     *]
;  |     |
;  v     v
; [* *] [* *]
;  | |   | |
;  v v   v v
;  a b   a b
(define y (cons 'a 'b))
(define z (cons x y))
(display (has-loop? z))
(newline)

;  +----+
;  |    |
;  v    |
; [* *] |
;  | |  |
;  v |  |
;  a +--+
(define u (cons 'a 'b))
(set-cdr! u u)
(display (has-loop? u))
(newline)


; 引自一个复杂图形
;  +---------------+
;  |               |
;  |        _ _    V _
;  |       |1|_|->|2|_|-----+
;  |        |      |   _ _  |
;  |        |      +->|3|X| |
;  |        |               |
;  |        |      +--------+
;  |        |      |
;  | _      V _    V _    _ _
; |4|_|<-- |5|_|->|6|_|->|7|_|-+
;           ^                  |
;           |                  |
;           +------------------+
(define v1 (cons 'a 'b))
(define v2 (cons 'a 'b))
(define v3 (cons 'a 'b))
(define v4 (cons 'a 'b))
(define v5 (cons 'a 'b))
(define v6 (cons 'a 'b))
(define v7 (cons 'a 'b))
(set-car! v1 v5)
(set-cdr! v1 v2)
(set-car! v2 v3)
(set-cdr! v2 v6)
(set-car! v4 v2)
(set-car! v5 v4)
(set-cdr! v5 v6)
(set-cdr! v6 v7)
(set-cdr! v7 v5)
(display v1)
(newline)
(display (has-loop? v1))
(newline)
