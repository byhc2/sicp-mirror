#!/usr/bin/guile
!#

; 习题 3.17
(define checked (list))

(define (set-check x)
  (set! checked (cons x checked))
  #t)

(define (if-checked x)
  (define (do-check x lst)
    (cond
      ((null? lst) #f)
      ((eq? (car lst) x) #t)
      (else (do-check x (cdr lst)))))
  (do-check x checked))

(define (count-pairs x)
  (cond
    ((not (pair? x)) 0)
    ((if-checked x) 0)
    (else (begin
            (set-check x)
            (+ (count-pairs (car x))
               (count-pairs (cdr x))
               1)))))

(define x (cons 'a 'b))
(define y (cons 'a 'b))
(define z (cons x y))
(set-car! x y)

(display (count-pairs z))
(newline)

(define x (cons 'a 'b))
(define y (cons x x))
(define z (cons y y))
(display (count-pairs z))
(newline)

(define x (cons 'a 'b))
(define y (cons 'a 'b))
(define z (cons x y))
(set-cdr! x y)
(set-cdr! y x)
(display (count-pairs z))
(newline)
