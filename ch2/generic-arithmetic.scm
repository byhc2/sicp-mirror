#!/usr/bin/guile
!#

(define-module (generic-arithmetic)
               #: export (add sub mul div equ? sine
                              zero?  ipart drop cosine
                              rpart mag ang numer
                              denom make-rat project
                              apply-generic trans-type
                              make-polynomial
                              term-list
                              make-complex-from-real-imag
                              make-complex-from-mag-ang))

(add-to-load-path ".")
(load "environ.scm")
(load "complex-number.scm")
(load "rational-number.scm")
(load "scheme-number.scm")
(load "poly.scm")

(install-scheme-number-package)
(install-rational-number-package)
(install-complex-number-package)
(install-polynomial-package)

; 习题2.82
(define (can-coercion target-type type-tags)
  (let ((type-procs (map (lambda (t) (get-coercion t target-type)) type-tags)))
    (null? (filter (lambda (x) (eq? x #f)) type-procs))))
(define (coercion-util target-type list1 list2)
  (cond ((can-coercion target-type (append list1 list2)) target-type)
    ((null? list2) #f)
    (else (let ((new-target-type (car list2))
                (new-list1 (append list1 (list target-type)))
                (new-list2 (cdr list2)))
            (coercion-util new-target-type new-list1 new-list2)))))

(define (trans-type type arg)
  (if (eq? (type-tag arg) type)
      arg
      ((get-coercion (type-tag arg) type) arg)))

(define (apply-generic op . args)
  (drop (apply do-apply-generic op args)))

(define (do-apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((target-type (coercion-util (car type-tags)
                                            '()
                                            (cdr type-tags))))
            (if target-type
                (apply do-apply-generic
                             op
                             (map (lambda (a) (trans-type target-type a)) args))
                (error "类型不能对齐 -- APPLY-GENERIC" type-tags)))))))

(define (apply-generic2 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length type-tags) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "二参数同类型 -- APPLY-GENERIC" type-tags)
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2 (apply-generic2 op (t1->t2 a1) a2))
                        (t2->t1 (apply-generic2 op a1 (t2->t1 a2)))
                        (else (error "参数类型不兼容 -- APPLY-GENERIC"
                                     type-tags)))))
                (error "参数个数非2 -- APPLY-GENERIC" type-tags))
              (error "找不到对应操作 -- APPLY-GENERIC" (list op type-tags)))))))

; 此时业已不需make-scheme-number
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (make-rat n d)
  ((get 'make 'rational) n d))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; 以下所有函数不做类型检查
; 若遇非其所用类型，则无限递归
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (ipart x) (apply-generic 'ipart x))
(define (rpart x) (apply-generic 'rpart x))
(define (mag x) (apply-generic 'mag x))
(define (ang x) (apply-generic 'ang x))
(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))
; 习题2.79
(define (equ? x y) (apply-generic 'equ? x y))
; 习题2.80
(define (zero? x) (apply-generic 'zero? x))

(put-coercion 'scheme-number 'rational
              (lambda (x) (make-rat x 1)))
(put-coercion 'scheme-number 'complex
              (lambda (x) (make-complex-from-real-imag x 0)))
(put-coercion 'rational 'complex
              (lambda (x) (make-complex-from-real-imag (div (numer x) (denom x)) 0)))


; 习题2.81
; a 对exp调用复数
;   apply-generic找不到对应实现，进入类型转换
;   因自我转换类型不变，再次调用apply-generic，相当于原调用
;   最终结果是无限递归
; b 没有解决，当某方法于某类型未定义时，会出现无限递归
; c 代码已加，当二参数类型相同，出错。然此法不能根本解决问题
;   盖因类似幂乘等算子，其二参数类型本不必相同
;   如exp(number, real)，参数一可为复数，参数二之多为实数

; 习题2.85
(define (drop arg)
  (cond
    ((or (eq? arg #t) (eq? arg #f)) arg)
    (else (let ((after-drop (project arg)))
            (if (equ? arg after-drop)
                after-drop
                (if (eq? (type-tag after-drop) 'scheme-number)
                    arg
                    (drop after-drop)))))))
; 复数->实数->整数
; 有理数->整数
(define (project arg)
  (let ((type (type-tag arg)))
    (cond
      ((eq? type 'complex) (if (equ? (ipart arg) 0) (rpart arg) arg))
      ((eq? type 'rational) (if (equ? (denom arg) 1) (numer arg) arg))
      ((eq? type 'scheme-number) (round arg))
      ((list? arg) arg)
      (else (error "类型不能下降 -- PROJECT" type)))))

; 习题2.86
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
; 暂只处理加法减法，乘除暂不能
; 因需重定义sqrt处理有理数

(define (term-list p) (apply-generic 'term-list p))
