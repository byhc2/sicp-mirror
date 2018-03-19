#!/usr/bin/guile
!#

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
