; 习题 4.3

(define *op-table* (make-hash-table))
(define (get op type-tags)
  (hash-ref *op-table* (list op type-tags)))
(define (put op type-tags proc)
  (hash-set! *op-table* (list op type-tags) proc))

(define (data-driven-eval expr env)
  (cond ((self-evaludating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((get 'op (car expr)) ((get 'op (car expr)) expr env))
        ((application? expr)
         (data-driven-apply (data-driven-eval (operator expr) env)
                            (list-of-values (operands expr) env)))
        (else
          (i-error "Unknon expression type -- EVAL" expr))))

; 安装各个操作符
(define (install-data-driven-eval-op)
  (define (text-of-quotation expr env) (...))
  (define (eval-assignment expr env) (...))
  (define (eval-definition expr env) (...))
  (begin
    (put 'define eval-definition)
    (put 'set! eval-assignment)
    (put 'quote text-of-quotation)))
