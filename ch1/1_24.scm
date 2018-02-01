#!/usr/bin/guile
!#

; 习题1.24

(define (expmod b e m)
  (define (square x)
    (* x x))
  (if (= e 0)
    1
    (if (even? e)
      (remainder (square (expmod b (/ e 2) m)) m)
      (remainder (* b (expmod b (- e 1) m)) m)
      )
    )
  )

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (runtime)
  (get-internal-real-time)
  )

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
  )

(define (start-prime-test n start-time)
  (if (fermat-test n)
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

(display (expmod 4 94 94))
(newline)
(display (remainder 4 94))
(newline)

(display (fermat-test 97))
(newline)
(display (fermat-test 96))
(newline)

(search-for-primes 1000 1050)
(search-for-primes 1000000 1000050)

; 时间消耗后者大约为前者2倍(1.5)，满足log(n)增长规律
; 1009 *** 2065
; 1013 *** 1745
; 1017 *** 1809
; 
; 1000003 *** 2893
; 1000033 *** 2734
; 1000037 *** 2770

; 习题1.25
; 乘法运算在数字较大时比较费时，因此fast-expt会消耗较长时间

; 习题1.26
; 使用乘法，每个expmode会展开成为两个expmod，增长阶为2^n
; 于是long(2^n)=n
