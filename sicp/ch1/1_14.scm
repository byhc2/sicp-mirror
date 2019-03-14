#!/usr/bin/guile
!#

; 习题1.14

(define (value-of-coin n)
  (cond ((= n 5) 50)
	((= n 4) 25)
	((= n 3) 10)
	((= n 2) 5)
	((= n 1) 1)
	)
  )

(define (space-padding n)
  (define (iter i)
    (if (< i n)
      (begin
	(display "    ")
	(iter (+ i 1))
	)
      (display "")
      )
    )
  (iter 0)
  )

(define (cc amount kinds-of-coins recursive-depth)
  (begin
    (space-padding recursive-depth)
    (display "cc ")
    (display amount)
    (display "  ")
    (display kinds-of-coins)
    (newline)
    (cond ((= amount 0) 1)
	  ((< amount 0) 0)
	  ((= kinds-of-coins 0) 0)
	  (else (+ (cc (- amount (value-of-coin kinds-of-coins)) kinds-of-coins (+ recursive-depth 1))
		   (cc amount (- kinds-of-coins 1) (+ recursive-depth 1))
		   )
		)
	  )
    )
  )

(define (count-change n)
  (cc n 5 0)
  )

(count-change 3)
;(display (count-change 11))
(newline)

; 暂不能算出复杂度
