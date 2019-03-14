#!/usr/bin/guile
!#

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

(define (rectangular? z)
  (eq? (type-tag z) 'rectangluar))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (rpart-rectangular z) (car z))
(define (ipart-rectangular z) (cdr z))
(define (mag-rectangular z)
  (sqrt (+ (square (rpart-rectangular z))
           (square (ipart-rectangular z)))))
(define (ang-rectangular z)
  (atan (ipart-rectangular z) (rpart-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangluar (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangluar (cons (* (cos a) r) (* (sin a) r))))

(define (mag-polar z) (car z))
(define (ang-polar z) (cdr z))
(define (rpart-polar z)
  (* (mag-polar z) (cos (ang-polar z))))
(define (ipart-polar z)
  (* (mag-polar z) (sin (ang-polar z))))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;; 以上是复数两种不同表示
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 以下是复数通用方法

(define (rpart z)
  (cond
    ((rectangular? z) (rpart-rectangular (contents z)))
    ((polar? z) (rpart-polar (contents z)))
    (else (error "unknown type -- RPART" z))))

(define (ipart z)
  (cond
    ((rectangular? z) (ipart-rectangular (contents z)))
    ((polar? z) (ipart-polar (contents z)))
    (else (error "unknown type -- IPART" z))))

(define (mag z)
  (cond
    ((rectangular? z) (mag-rectangular (contents z)))
    ((polar? z) (mag-polar (contents z)))
    (else (error "unknown type -- MAG" z))))

(define (ang z)
  (cond
    ((rectangular? z) (ang-rectangular (contents z)))
    ((polar? z) (ang-polar (contents z)))
    (else (error "unknown type -- ANG" z))))

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

(define (make-from-real-imag x y) (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a) (make-from-mag-ang-polar r a))

(define (f)
  (let ((z1 (make-from-real-imag 1 2))
        (z2 (make-from-real-imag 2 3)))
    (add-complex z1 z2)))

(display (f))
(newline)
