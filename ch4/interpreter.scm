#!/usr/bin/racket
#lang scheme

; 为防止与解释器关键字冲突，修改部分过程名，参数名
; 关键字、过程名一般加i-前缀，参数明基本随意

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
          (error "Unknon expression type -- EVAL" expr))))
