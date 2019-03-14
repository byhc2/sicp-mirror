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

(define (make-prod m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))

(define (deriv-sum expr var)
  (make-sum (deriv (car expr) var) (deriv (cdr expr) var)))

(define (deriv-prod expr var)
  (let ((p1 (car expr))
        (p2 (cdr expr)))
    (make-sum (make-prod (deriv p1 var) p2)
              (make-prod p1 (deriv p2 var)))))

(define (make-exp b e)
  (cond ((= e 0) 1)
	((= e 1) b)
	(else (list b '** e))))

(define (deriv-exp expr var)
  (let ((base (car expr))
        (e (cdr expr)))
    (make-prod e (make-exp base (- e 1)))))

; get为强制指定
(define (get op type-tag)
  (cond ((and (eq? op 'deriv) (eq? type-tag '+)) deriv-sum)
        ((and (eq? op 'deriv) (eq? type-tag '*)) deriv-prod)
        ((and (eq? op 'deriv) (eq? type-tag '**)) deriv-exp)))

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
; c 实现幂函数
; d 无需改动，仅需变动方法表格

(display (deriv '(x + x) 'x))
(newline)
(display (deriv '(4 * x) 'x))
(newline)
(display (deriv '(x ** 8) 'x))
(newline)

; 习题2.75
; a 类似如下过程
; (define (get-record name division)
;   ((get 'get-record division) name))
;
; b 类似如下过程
; (define (get-salary name division)
;   ((get 'get-salary division) name))
; c
; (define (find-employee-record name division-db)
;   (filter (lambda (x) ((get 'get-record x) name) division-db)))
; d 只需要put一个新的get-record方法

; 习题2.75
; (define (make-from-mag-ang mag ang)
;   (define (dispatch op)
;     (cond ((eq? op 'real-part) (* (cos ang) mag))
;           ((eq? op 'imag-part) (* (sin ang) mag))
;           ((eq? op 'magnitude) mag)
;           ((eq? op 'angle) ang)
;           (else (error "unknown op -- MAKE-FROM-MAG-ANG" op))))
;   dispatch)

; 习题2.76
; 数据导向的方式，对统一组操作，新增加表示类型时比较方便，仅需要修改方法表即可
; 需要假如新操作时，数据导向方式也是很合适的，消息传递也可以。
