#!/usr/bin/guile
!#

; 习题2.42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
	(lambda (positions) (safe? k positions)
    )
  )
