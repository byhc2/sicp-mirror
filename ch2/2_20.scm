#!/usr/bin/guile
!#

; 习题2.20

(define (reverse_ l)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (cons (car rest) result) (cdr rest))
      )
    )
  (iter (list) l)
  )

(define (same-parity x . r)
  (define (iter result rest)
    (if (null? rest)
      (reverse_ result)
      (if (= (remainder x 2) (remainder (car rest) 2))
	(iter (cons (car rest) result) (cdr rest))
	(iter result (cdr rest))
	)
      )
    )
  (iter (list) r)
  )

(display (same-parity 1 2 3 4 5 6))
(newline)
