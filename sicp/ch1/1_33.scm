#!/usr/bin/guile
!#

; 习题1.33

(define (filterd-accumulate filt combiner init-value term a b next)
  (define (accumulate-iter acc cur)
    (if (> cur b)
      acc
      (accumulate-iter 
	(if (filt cur)
	  (combiner acc (term cur))
	  acc
	  )
	(next cur))
      )
    )
  (accumulate-iter init-value a)
  )

(define (prime? n)
  (and (= n (smallest-divisor n)) (not (= n 1)))
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

(define (plus a b) (+ a b))
(define (prod a b) (* a b))
(define (identity x) x)
(define (inc x) (+ x 1))

(define (relatively-prime a)
  (= (gcd a 9) 1))

(display "1到10素数和 ")
(display (filterd-accumulate prime? plus 0 identity 1 10 inc))
(newline)

(display "与9互质且比9小的数之积 ")
(display (filterd-accumulate relatively-prime prod 1 identity 2 9 inc))
(newline)

; 习题1.34 语法错误
; 因为(f f)会变成(f 2)，再变成(2 2)
