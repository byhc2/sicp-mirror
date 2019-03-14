#!/usr/bin/guile
!#

(define f
  (let ((flag #f))
    (lambda (x)
      (if flag
          0
          (begin
            (set! flag #t)
            x)))))

;(display (+ (f 0) (f 1)))
;(newline)
(display (+ (f 1) (f 0)))
(newline)

; 习题3.9 暂略
; 习题3.10 暂略

; 习题3.11
; acc是全局环境中的符号，被约束到另外一个环境
; acc2也是全局的，约束到的环境与acc不同
