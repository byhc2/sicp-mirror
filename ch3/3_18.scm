#!/usr/bin/guile
!#

(define queue (list))

; 习题3.18
; 有向无环图检测
; eq?检测的就是指针地址

(define (has-loop? x)
  (if (not (pair? x))
      #f
      (let ((node1 (car x))
            (node2 (cdr x)))
        (cond
          ((memq node1 queue) #t)
          ((memq node2 queue) #t)
          (else (begin
                  ; 仅可将元组加入
                  (if (pair? node1)
                      (set! queue (cons node1 queue)))
                  (if (pair? node2)
                      (set! queue (cons node2 queue)))
                  (or (has-loop? node1)
                      (has-loop? node2))))))))

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

;  ------
;  |    |
;  v    |
; [* *] |
;  | |  |
;  v |  |
;  a ----
(define u (cons 'a 'b))
(set-cdr! u u)
(display (has-loop? u))
(newline)
