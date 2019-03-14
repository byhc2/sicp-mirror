#!/usr/bin/guile
!#

; 习题2.77
; (magnitude z)调用时，先匹配到z的类型为complex
; 然后，会调用real-part，此时z已经被剥离了complex标签
; real-part匹配到z是rectangular类型
; 调用real-part-rectangular返回结果

; 习题2.78
; 仅需要在type-tag中调用number?等方法增加一些判断即可。此处略

; 习题2.79
; (define (equ? n1 n2)
;   (apply-generic 'euq? n1 n2))

; 以复数平面直角座标表示为例
; (put 'euq? '(complex complex) (lambda (n1 n2) (tag (equ?-complex n1 n2))))

; 习题2.80
; 以复数为例
; (put '=zero? '(complex complex) (lambda (n1 n2) (tag (=zero?-complex n1 n2))))

; 习题2.81
; a 出现无限递归
