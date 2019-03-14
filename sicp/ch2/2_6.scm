#!/usr/bin/guile
!#

; 习题2.6

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x))))
  )

(define (add-2 n)
  (lambda (f) (lambda (x) (f (f ((n f) x)))))
  )

; one可定义为(add-1 zero)
; (add-1 zero)
; 代换得
; (   (lambda (f) (lambda (x) (f ((zero f) x))))    )
; (   (lambda (f) (lambda (x) (f ((zero f) x))))    )
; (   (lambda (f) (lambda (x) (f (   (lambda (x) x) x       ))))    )
; (   (lambda (f) (lambda (x) (f (x))))    )
; (   (lambda (f) (lambda (x) (f x)))    )
; 所以

(define one
  (lambda (f) (lambda (x) (f x)))
  )

; tow ==> (add-1 one)
; (    (lambda (f) (lambda (x) (f ((one f) x))))      )
; (    (lambda (f) (lambda (x) (f (f x))))      )

(define two
  (lambda (f) (lambda (x) (f (f x))))
  )

; //此处尚不明就里 TODO
(define (plus a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x))))
  )
