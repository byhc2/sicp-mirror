#!/usr/bin/guile
!#

; 习题1.22

; runtime没有，用get-internal-real-time代之
; 1秒1000000000单位，即1纳秒

(define (runtime)
  (get-internal-real-time)
  )

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
  )

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  )

(define (search-for-primes a b)
  (define (check x)
    (if (< x b)
      (begin
	(timed-prime-test x)
	(check (+ x 2))
	)
      )
    )
  (if (odd? a)
    (check a)
    (check (+ a 1))
    )
  )

(define (smallest-divisor n)
  (define (square x)
    (* x x)
    )
  (define (find-divisor x)
    (if (> (square x) n)
      n
      (if (= (remainder n x) 0)
	x
	(find-divisor (+ x 1))
	)
      )
    )
  (find-divisor 2)
  )

(define (prime? n)
  (= n (smallest-divisor n))
  )

(search-for-primes 1000000 1000050)

; 四个区间查找时间如下
; 1009 *** 1584
; 1013 *** 1337
; 1019 *** 1347
; 
; 10007 *** 4066
; 10009 *** 3841
; 10037 *** 3768
; 
; 100003 *** 11769
; 100019 *** 11431
; 100043 *** 11432
; 
; 1000003 *** 45992
; 1000033 *** 35525
; 1000037 *** 43348
;
; 后者时间，分别是前者的2.74 2.97 3.61倍，可以近似认为都在3附近
