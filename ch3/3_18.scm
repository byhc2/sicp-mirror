#!/usr/bin/guile
!#

(define queue (list))

; 习题3.18
; 有向无环图检测
; eq?检测的就是指针地址

(define (has-loop? x)
  (if (pair? x)
      #f
      (let ((node1 (car x))
            (node2 (cdr x)))
        (cond
          ((memq node1 queue) #t)
          ((memq node2 queue) #t)
          (else (begin
                  (set! queue (cons node1 queue))
                  (set! queue (cons node2 queue))
                  (or (has-loop? node1)
                      (has-loop? node2))))))))

; [*]-->[*]-->nil
;  |     |
;  v     v
;  a     b
(define x (cons 'a 'b))
(display (has-loop? x))
(newline)

(define y (cons 'a 'b))
(define z (cons x y))
(display (has-loop? z))
(newline)

(define u (cons 'a 'b))
(set-cdr! u u)
(display (has-loop? u))
(newline)
