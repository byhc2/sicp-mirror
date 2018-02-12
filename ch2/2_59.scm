#!/usr/bin/guile
!#

; 习题2.59

(define (element-of-set? e s)
  (cond ((null? s) #f)
	((= e (car s)) #t)
	(else (element-of-set? e (cdr s)))))

(define (adjoin-set e s)
  (if (element-of-set? e s)
    s
    (cons e s)))

(define (intersection-set s1 s2)
  (cond ((null? s1) '())
	(else (let ((pre (intersection-set (cdr s1) s2)))
		(cond ((element-of-set? (car s1) pre) pre)
		      ((element-of-set? (car s1) s2) (cons (car s1) pre))
		      (else pre))))))


(define (union-set s1 s2)
  (cond ((null? s1) s2)
	(else (let ((pre (union-set (cdr s1) s2)))
		(if (not (element-of-set? (car s1) pre))
		  (cons (car s1) pre)
		  pre)))))

(display (intersection-set '(1 2 2 3 3 3) '(3 3 2 2 7 4)))
(newline)
(display (union-set '(1 2 2 3) '(5 7 4)))
(newline)

; 习题2.60
; 上述操作仍可适用
; 除intersection-set若要排除重复元素
; 需检测(car s1)是否在结果集合中
; 此处已修改，与书中不同
