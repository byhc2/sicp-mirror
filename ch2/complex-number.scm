#!/usr/bin/guile
!#

(define-module (complex-number)
               #: export (install-complex-number-package
                           make-complex-from-real-imag
                           make-complex-from-mag-ang))

(add-to-load-path ".")
(use-modules (generic-arithmetic))

(define (square x) (* x x))

(define (install-rectangular-package)
  (define (rpart z) (car z))
  (define (ipart z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (mag z)
    (sqrt (+ (square (rpart z))
             (square (ipart z)))))
  (define (ang z) (atan (ipart z) (rpart z)))

  (define (equ? z1 z2)
    (and (= (rpart z1) (rpart z2))
         (= (ipart z1) (ipart z2))))

  (define (zero? z)
    (and (= (rpart z) 0)
         (= (ipart z) 0)))

  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (tag x) (attach-tag 'rectangular x))
  (begin
    (put 'rpart '(rectangular) rpart)
    (put 'ipart '(rectangular) ipart)
    (put 'mag '(rectangular) mag)
    (put 'ang '(rectangular) ang)
    (put 'equ? '(rectangular rectangular) equ?)
    (put 'zero? '(rectangular) zero?)
    (put 'make-from-real-imag '(rectangular)
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang '(rectangular)
         (lambda (r a) (tag (make-from-mag-ang r a))))))

(define (install-polar-package)
  (define (mag z) (car z))
  (define (ang z) (cdr z))
  (define (rpart z) (* (mag z) (cos (ang z))))
  (define (ipart z) (* (mag z) (sin (ang z))))
  (define (equ? z1 z2)
    (and (= (mag z1) (mag z2))
         (= (ang z1) (ang z2))))

  (define (zero? z)
    (and (= (mag z) 0)
         (= (ang z) 0)))

  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))
  (define (make-from-mag-ang r a) (cons r a))
  (define (tag x) (attach-tag 'polar x))
  (begin
    (put 'rpart '(polar) rpart)
    (put 'ipart '(polar) ipart)
    (put 'mag '(polar) mag)
    (put 'ang '(polar) ang)
    (put 'equ? '(polar polar) equ?)
    (put 'zero? '(polar) zero?)
    (put 'make-from-real-imag '(polar)
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang '(polar)
         (lambda (r a) (tag (make-from-mag-ang r a))))))

(define (install-complex-number-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag '(rectangular)) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang '(polar)) r a))

  (define (rpart z) (apply-generic 'rpart z))
  (define (ipart z) (apply-generic 'ipart z))
  (define (mag z) (apply-generic 'mag z))
  (define (ang z) (apply-generic 'ang z))

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
  (define (equ?-complex z1 z2)
    (apply-generic 'equ? z1 z2))

  (define (zero? z)
    (apply-generic 'zero? z))

  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex) (lambda (z1 z2) (equ?-complex z1 z2)))
  (put 'zero? '(complex) (lambda (z) (zero? z)))
  (put 'mag '(complex) mag)
  (put 'ang '(complex) ang)
  (put 'rpart '(complex) rpart)
  (put 'ipart '(complex) ipart)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (x y) (tag (make-from-mag-ang x y)))))

(install-rectangular-package)
(install-polar-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; 习题2.77
; 其实不止增加(put 'mag '(complex) mag)一行
; 还要有(define (mag x) (apply-generic 'mag x))
; apply-generic调用两次
; 第一次剥去complex类型标记，调用install-complex-number-package中mag
; 第二次剥去rectangular或者polar标记，调用最终的mag
