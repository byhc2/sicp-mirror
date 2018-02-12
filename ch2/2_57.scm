#!/usr/bin/guile
!#

; 习题2.57

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? e) (eq? (cadr e) '+))
(define (addend e) (car e))
(define (augend e) (caddr e))

(define (=number? a b)
  (if (and (number? a) (= a b))
    #t
    #f))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))

(define (product? e) (eq? (cadr e) '*))
(define (multiplier e) (car e))
(define (multiplicand e) (caddr e))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))

(define (exp? e) (eq? (cadr e) '**))
(define (base e) (car e))
(define (exponent e) (caddr e))

(define (make-exp b e)
  (cond ((= e 0) 1)
	((= e 1) b)
	(else (list b '** e))))

(define (deriv fun var)
  (cond ((number? fun) 0)
	((variable? fun) (if (same-variable? fun var) 1 0))
	((sum? fun) (let ((u (addend fun))
			  (v (augend fun)))
		      (make-sum (deriv u var) (deriv v var))))
	((product? fun) (let ((u (multiplier fun))
			      (v (multiplicand fun)))
			  (make-sum
			    (make-product u (deriv v var))
			    (make-product v (deriv u var)))))
	((exp? fun) (let ((b (base fun))
			  (e (exponent fun)))
		      (make-product (make-product e (make-exp b (- e 1))) (deriv b var))))
	(else (error "不能求导"))))

(display (deriv '(x ** 3) 'x))
(newline)

(display (deriv '(x + 3) 'x))
(newline)

; 习题2.58
; 不能，deriv不区分运算符优先级
; 若一组括号内为同一运算符，可修改deriv求导
