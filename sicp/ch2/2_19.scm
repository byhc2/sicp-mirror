#!/usr/bin/guile
!#

; 习题2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 50 25 10 5 1))

(define (no-more? l)
  (null? l)
  )

(define (first-kind-coin-value l)
  (car l)
  )

(define (except-first l)
  (cdr l)
  )

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	  (+
	    (cc (- amount (first-kind-coin-value coin-values)) coin-values)
	    (cc amount (except-first coin-values))
	   )
	  )
	)
  )

(display (cc 100 us-coins))
(newline)
(display (cc 99 us-coins))
(newline)
