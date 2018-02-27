#!/usr/bin/guile
!#

(define *op-table* (make-hash-table))
(define (get op type-tags)
  (hash-ref *op-table* (list op type-tags)))
(define (put op type-tags proc)
  (hash-set! *op-table* (list op type-tags) proc))

(define (square x) (* x x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-rectangular-package)
  (define (rpart z) (car z))
  (define (ipart z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (mag z)
    (sqrt (+ (square (rpart z))
             (square (ipart z)))))
  (define (ang z) (atan (ipart z) (rpart z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (tag x) (attach-tag 'rectangular x))
  (begin
    (put 'rpart '(rectangular) rpart)
    (put 'ipart '(rectangular) ipart)
    (put 'mag '(rectangular) mag)
    (put 'ang '(rectangular) ang)
    (put 'make-from-real-imag '(rectangular)
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang '(rectangular)
         (lambda (r a) (tag (make-from-mag-ang r a))))))

(define (install-polar-package)
  (define (mag z) (car z))
  (define (ang z) (cdr z))
  (define (rpart z) (* (mag z) (cos (ang z))))
  (define (ipart z) (* (mag z) (sin (ang z))))

  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))
  (define (make-from-mag-ang r a) (cons r a))
  (define (tag x) (attach-tag 'polar x))
  (begin
    (put 'rpart '(polar) rpart)
    (put 'ipart '(polar) ipart)
    (put 'mag '(polar) mag)
    (put 'ang '(polar) ang)
    (put 'make-from-real-imag '(polar)
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang '(polar)
         (lambda (r a) (tag (make-from-mag-ang r a))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 通用操作定义

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "no method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))

(define (rpart z) (apply-generic 'rpart z))
(define (ipart z) (apply-generic 'ipart z))
(define (mag z) (apply-generic 'mag z))
(define (ang z) (apply-generic 'ang z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag '(rectangular)) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang '(polar)) r a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-complex z1 z2)
  (make-from-real-imag (+ (rpart z1) (rpart z2))
                       (+ (ipart z1) (ipart z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (rpart z1) (rpart z2))
                       (- (ipart z1) (ipart z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (mag z1) (mag z2))
                     (* (ang z1) (ang z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (mag z1) (mag z2))
                     (- (ang z1) (ang z2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-rectangular-package)
(install-polar-package)

;(display (get 'rpart '(rectangular)))
;(newline)

(define z1 (make-from-real-imag 1 2))
(define z2 (make-from-mag-ang 3 4))
(display (add-complex z1 z2))
(newline)
