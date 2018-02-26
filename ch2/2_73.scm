#!/usr/bin/guile
!#

; 习题2.73

(define (=number? a b)
  (if (and (number? a) (= a b))
    #t
    #f))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (deriv-sum expr var)
  (make-sum (deriv (car expr) var) (deriv (cdr expr) var)))

(define (get op type-tag)
  (cond ((and (eq? op 'deriv) (eq? type-tag '+)) deriv-sum)
        ((and (eq? op 'deriv) (eq? type-tag '*)) deriv-prod)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))
        (else ((get 'deriv (operator expr)) (operands expr) var))))

(define (operator expr) (cadr expr))
(define (operands expr) (cons (car expr) (caddr expr)))

; a 数字类型无法提取出operator，当然也可以用operator来处理数字，只是将此处判断移进去而已
; b 见前代码

(display (deriv '(x + x) 'x))
(newline)
