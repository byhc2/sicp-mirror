#!/usr/bin/racket
#lang scheme

; 为防止与解释器关键字冲突，修改部分过程名，参数名
; 关键字、过程名一般加i-前缀，参数名基本随意

(define (i-eval expr env)
  (cond ((self-evaludating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((quoted? expr) (text-of-quotation expr))
        ((assignment? expr) (eval-assignment expr env))
        ((definition? expr) (eval-definition expr env))
        ((if? expr) (eval-if expr env))
        ((lambda? expr)
         (make-procedure (lambda-parameters expr)
                         (lambda-body expr)
                         env))
        ((begin? expr)
         (eval-sequence (begin-actions expr) env))
        ((cond? expr) (i-eval (cond->if expr) env))
        ((application? expr)
         (i-apply (i-eval (operator expr) env)
                  (list-of-values (operands expr) env)))
        (else
          (i-error "Unknon expression type -- EVAL" expr))))

(define (i-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment
                          (procedure-arguments procedure)
                          arguments
                          (procedure-environment procedure))))
        (else (i-error "Unknown procedure type -- APPLY" procedure))))

;(define (list-of-values exps env)
;  (i-if (no-operands? exps)
;        '()
;        (cons (i-eval (first-operand exps) env)
;              (list-of-values (rest-operands exps) env))))
; 习题 4.1
; 使用let表达式
; 正常为从左到右，r2l为从右到左
(define (list-of-values exps env)
  (i-if (no-operands? exps)
        '()
        (let (first-value (i-eval (first-operand exps) env))
          (cons fist-value
                (list-of-values (rest-operands exps) env)))))

(define (list-of-values-r2l exps env)
  (i-if (no-operands? exps)
        '()
        (let (last-value (list-of-values (rest-operands exps) env))
          (cons (i-eval (first-operand exps) env)
                last-value))))

(define (eval-if expr env)
  (if (true? (i-eval (if-predicate expr) env))
      (i-eval (if-consequent expr) env)
      (i-eval (if-alternative expr) env)))

(define (eval-sequence expr env)
  (cond ((last-exp? expr) (i-eval (first-exp expr) env))
        (else (i-eval (first-exp expr) env)
              (eval-sequence (rest-exps expr) env))))

(define (eval-assignment expr env)
  (set-variable-value! (assignment-variable expr)
                       (i-eval (assignment-value expr) env)
                       env)
  'ok)

(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
                    (i-eval (definition-value expr) env)
                    env)
  'ok)

(define (self-evaludating? expr)
  (cond ((number? expr) #t)
        ((string? expr) #t)
        (else #f)))

(define (variable? expr) (symbol? expr))

(define (quoted? expr) (tagged-list? expr 'quote))

(define (text-of-quotation expr) (cadr expr))

(define (tagged-list? expr tag)
  (if (pair? expr)
      (eq? (car expr) tag)
      #f))

(define (assignment? expr) (tagged-list? expr 'set!))
(define (assignment-variable expr) (cadr expr))
(define (assignment-value expr) (caddr expr))
