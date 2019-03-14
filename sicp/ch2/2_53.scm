#!/usr/bin/guile
!#

; 习题2.53
; (a b c)
; ((george))
; ((y1 y2))
; (y1 y2)
; #f
; #f
; (red shoes blue socks)

; 习题2.54
(define (equal2? a b)
  (cond ((and (not (list? a)) (not (list? b))) (eq? a b))
	((and (list? a) (list? b))
	 (cond ((and (null? a) (null? b)) #t)
	       ((and (not (null? a)) (not (null? b)))
		(and (eq? (car a) (car b))
		     (equal2? (cdr a) (cdr b))))
	       (else #f)))
	 (else #f)))

(display (equal2? '() '(a)))
(newline)
(display (equal2? '2 '2))
(newline)
(display (equal2? '(a) '(a)))
(newline)
(display (equal2? '(this is a) '(this is 2)))
(newline)
(display (equal2? '(this is a) '(this is a)))
(newline)

; 习题2.55
; ''abrac... 实际是 '(quote abrac...)
