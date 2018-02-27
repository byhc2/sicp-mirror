#!/usr/bin/guile
!#

(define-module (generic-arithmetic)
               #: export (get put type-tag attach-tag
                              contents apply-generic
                              add sub mul div))

(define *op-table* (make-hash-table))
(define (get op type-tags)
  (hash-ref *op-table* (list op type-tags)))
(define (put op type-tags proc)
  (hash-set! *op-table* (list op type-tags) proc))

; 习题2.78
; 此处对type-tag attach-tag contents的改动
; 几类C++模板对原始类型之偏特化
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
    ((pair? datum) car datum)
    (else (error "bad tagged datum -- TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
    (else (cons type-tag contents))))

(define (contents datum)
  (cond ((number? datum) datum)
    ((pair? datum) (car datum))
    (else (error "bad tagged datum -- CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "no method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
