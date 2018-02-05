#!/usr/bin/guile
!#

; 习题2.7

(define (make-interval a b) (cons a b))
(define (lowwer-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (let (
	(new-lower (+ (lowwer-bound x) (lowwer-bound y)))
	(new-upper (+ (upper-bound x) (upper-bound y)))
	)
    (make-interval new-lower new-upper)
    )
  )

(define (mul-interval x y)
  (let (
	(p1 (* (lowwer-bound x) (lowwer-bound y)))
	(p2 (* (lowwer-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lowwer-bound y)))
	(p4 (* (upper-bound x) (upper-bound y)))
	)
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
   )
  )

(define (div-interval x y)
  (mul-interval
    x
    (make-interval
      (/ 1.0 (upper-bound y))
      (/ 1.0 (lowwer-bound x))
      )
    )
 )

(display (add-interval
	   (make-interval 2 3)
	   (make-interval 3 7)
	   )
	 )
(newline)
(display (mul-interval
	   (make-interval 2 3)
	   (make-interval 3 7)
	   )
	 )
(newline)

; 习题2.8
; a < x < b，c < y < d
; -d < -y < -c
; x - y ==> x + (-y)

(define (sub-interval x y)
  (add-interval
    x
    (make-interval (- 0 (upper-bound y)) (- 0 (lowwer-bound y)))
   )
  )

(display (sub-interval
	   (make-interval 2 3)
	   (make-interval 3 7)
	   )
	 )
(newline)

; 习题2.9
; 宽度函数 (lambda (x) (/ (- (upper-bound x) (lowwer-bound x)) 2))
; 加和函数展开为
; (make-interval
;	(+ (lowwer-bound x) (lowwer-bound y))
;	(+ (upper-bound x) (upper-bound y))
; )
; 其结果宽度为
; (/ (-
;	(+ (upper-bound x) (upper-bound y))
;	(+ (lowwer-bound x) (lowwer-bound y))
;    )
; 2)
; ==>
