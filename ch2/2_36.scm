#!/usr/bin/guile
!#

; 习题2.36

(define (accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq) (accumulate op initial (cdr seq)))
    )
  )

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    (list)
    (cons (accumulate op init (map car seqs))
	  (accumulate-n op init (map cdr seqs)))
    )
  )

(display (accumulate-n + 0 (list (list 1 2 3) (list 2 3 4) (list 3 4 5))))
(newline)
