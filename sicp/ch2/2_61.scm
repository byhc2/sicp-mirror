#!/usr/bin/guile
!#

; 习题2.61

; 以下基于有序表

(define (element-of-set? e s)
  (cond ((null? s) #f)
	((= e (car s)) #t)
	((> e (car s)) #f)
	(else (element-of-set? e (cdr s)))))

(define (adjoin-set e s)
  (define (insert e s)
    (cond ((null? s) (list e))
	  ((< e (car s)) (cons e s))
	  (else (cons (car s) (insert e (cdr s))))))
  (if (element-of-set? e s)
    s
    (insert e s)))

(define (intersection-set s1 s2)
  (if (or (null? s1) (null? s2))
    '()
    (let ((e1 (car s1))
	  (e2 (car s2)))
      (cond ((= e1 e2) (cons e1 (intersection-set (cdr s1) (cdr s2))))
	    ((< e1 e2) (intersection-set (cdr s1) s2))
	    (else (intersection-set s1 (cdr s2)))))))


; 习题2.62
(define (union-set s1 s2)
  (cond ((null? s1) s2)
	((null? s2) s1)
	(else (let ((e1 (car s1))
		    (e2 (car s2)))
		(cond ((< e1 e2) (cons e1 (union-set (cdr s1) s2)))
		      ((> e1 e2) (cons e2 (union-set s1 (cdr s2))))
		      (else (cons e1 (union-set (cdr s1) (cdr s2)))))))))

(display (adjoin-set 3 '(1 2 4 5)))
(newline)
(display (intersection-set '(2 3 4 5) '(1 2 4 5)))
(newline)
(display (union-set '(2 3 4 5) '(1 2 4 5)))
(newline)
