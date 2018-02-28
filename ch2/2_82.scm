#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (generic-arithmetic))
(use-modules (scheme-number))
(use-modules (rational-number))
(use-modules (complex-number))

(install-scheme-number-package)
(install-rational-number-package)
(install-complex-number-package)

; 习题2.82
; 实际代码在generic-arithmetic.scm中
; 此处为测试

(display (apply-generic 'add 1 (make-rat 1 2)))
(newline)

; 今apply-generic强制将统一所有参数类型
; 但并非所有操作均需要统一
; 如幂函数，仅其二参数为实数，其一参数可为复数等

; 习题2.83
(define (raise-type arg)
  (let ((type (type-tag arg)))
    (cond
      ((eq? type 'scheme-number)
       ((get-coercion 'scheme-number 'rational) arg))
      ((eq? type 'rational)
       ((get-coercion 'rational 'complex) arg))
      ((eq? type 'complex) arg)
      (else (error "类型不能提升 -- RAISE-TYPE" type)))))

; 习题2.84
; 若只一二参数，则raise-type无意义
; 多参数时，现有的apply-generic可自动提升，无比要用raise-type
; 故此处暂略
