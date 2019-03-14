#!/usr/bin/guile
!#

;(define-module (environ)
;               #: export (get put get-coercion put-coercion
;                              type-tag attach-tag contents))

(define *op-table* (make-hash-table))
(define (get op type-tags)
  (hash-ref *op-table* (list op type-tags)))
(define (put op type-tags proc)
  (hash-set! *op-table* (list op type-tags) proc))

;; 类型转换
(define *type-trans-table* (make-hash-table))
(define (get-coercion t1 t2)
  (hash-ref *type-trans-table* (list t1 t2)))
(define (put-coercion t1 t2 f)
  (hash-set! *type-trans-table* (list t1 t2) f))

;(add-to-load-path ".")
;(use-modules (generic-arithmetic))

; 习题2.78
; 此处对type-tag attach-tag contents的改动
; 几类C++模板对原始类型之偏特化
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
    ((pair? datum) (car datum))
    (else (error "bad tagged datum -- TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
    (else (cons type-tag contents))))

(define (contents datum)
  (cond ((number? datum) datum)
    ((pair? datum) (cdr datum))
    (else (error "bad tagged datum -- CONTENTS" datum))))
