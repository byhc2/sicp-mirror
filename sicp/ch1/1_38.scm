#!/usr/bin/guile
!#

; 习题1.38

; 此与习题1.37迭代方案不同
(define (cont-frac2 n d k)
  (define (cont-frac-iter result i)
    (if (= i 0)
      result
      (cont-frac-iter (/ (n i) (+ (d i) result)) (- i 1))
      )
    )
  (cont-frac-iter (/ (n k) (d k)) k)
  )

(define (e-expand)
  (let (
	(n (lambda (i) 1.0))
	(d (lambda (i) (if (= (remainder (+ i 1) 3) 0)
			 (* (/ (+ i 1) 3) 2)
			 1
			 )
	     )
	   )
	)
    (cont-frac2 n d 1000000)
    )
  )

(display "计算e ")
(display (+ (e-expand) 2))
(newline)

(define (power-fun b n)
  (define (odd x)
    (= (remainder x 2) 1)
    )
  (define (power-iter a prod remain)
    (if (= 0 remain)
      a
      (if (odd remain)
        (power-iter (* a prod) prod (- remain 1))
        (power-iter a (* prod prod) (/ remain 2))
        )
      )
    )
  (power-iter 1 b n)
  )

(define (tan-cf x k)
  (let (
	(n (lambda (i) (if (= i 1)
			 x
			 (- 0 (power-fun x 2))
			  )
			))
	(d (lambda (i) (- (* 2.0 i) 1)))
	)
    (cont-frac2 n d k)
   )
  )

(define pi 3.1415926)
(display "计算tan(pi / 4) ")
(display (tan-cf (/ pi 4) 100))
(newline)
