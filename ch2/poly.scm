#!/usr/bin/guile
!#

(add-to-load-path ".")

; 加减以稠密表示，乘除以稀疏表示
(define (install-polynomial-package)
  (define (make-dense variable term-list)
    ((get 'make-dense '(dense)) variable term-list))
  (define (make-sparse variable term-list)
    ((get 'make-sparse '(sparse)) variable term-list))
  (define (term-list-dense p)
    ((get 'term-list-dense (list (car p))) (cdr p)))
  (define (term-list-sparse p)
    ((get 'term-list-sparse (list (car p))) (cdr p)))
  (define (variable p)
    (apply-generic 'variable p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-dense (variable p1)
                    (add-terms-dense (term-list-dense p1)
                                     (term-list-dense p2)))
        (error "多项式变量不同 -- ADD-POLY" (list p1 p2))))
  (define (add-terms-dense L1 L2)
    (map add L1 L2))
  (define (the-empty-termlist) '())
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-sparse (variable p1)
                     (mul-terms (term-list-sparse p1)
                                (term-list-sparse p2)))
        (error "多项式变量不同 -- MUL-POLY" (list p1 p2))))
  (define (empty-termlist? term-list) (null? term-list))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms-sparse (mul-term-by-all-terms (first-term L1) L2)
                          (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term (make-term-sparse (+ (order t1) (order t2))
                                         (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (make-term-sparse order coeff) (list order coeff))
  (define (add-terms-sparse L1 L2)
    (cond
      ((empty-termlist? L1) L2)
      ((empty-termlist? L2) L1)
      (else (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond
                ((> (order t1) (order t2))
                 (adjoin-term t1 (add-terms-sparse (rest-terms L1) L2)))
                ((< (order t1) (order t2))
                 (adjoin-term t2 (add-terms-sparse L1 (rest-terms L2))))
                (else (adjoin-term
                        (make-term-sparse (order t1)
                                          (add (coeff t1) (coeff t2)))
                        (add-terms-sparse (rest-terms L1) (rest-terms L2)))))))))
  (define (neg-poly p)
    (make-sparse (variable p) (neg-terms (term-list-sparse p))))
  (define (neg-terms term-list)
    (if (empty-termlist? term-list)
        (the-empty-termlist)
        (let ((first-term-order (order (first-term term-list)))
              (first-term-coeff (coeff (first-term term-list))))
          (adjoin-term (make-term-sparse first-term-order
                                         (neg first-term-coeff))
                       (neg-terms (rest-terms term-list))))))

  (define (equ?-poly p1 p2)
    (define (equ-term? L1 L2)
      (cond
        ((and (null? L1) (null? L2)) #t)
        ((not (= (length L1) (length L2))) #f)
        ((equ? (car L1) (car L2)) (equ-term? (cdr L1) (cdr L2)))
        (else #f)))
    (if (same-variable? (variable p1) (variable p2))
        (equ-term? (term-list-dense p1) (term-list-dense p2))
        (error "多项式变量不同 -- EQU?-POLY" p1 p2)))

  ; 习题2.87
  (define (=zero?-poly p)
    (define (zero-terms? L)
      (cond
        ((null? L) #t)
        ((not (=zero? (car L))) #f)
        (else (zero-terms? (cdr L)))))
    (let ((terms (term-list-dense p)))
      (zero-terms? terms)))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                        (add-terms-sparse L1
                                          (neg-terms (mul-terms
                                                       L2
                                                       (list (make-term-sparse new-o
                                                                         new-c)))))))
                  (let ((sub-result (div-terms rest-of-result L2)))
                    (let ((sub-q (car sub-result))
                          (sub-r (cadr sub-result)))
                      (list (add-terms-sparse (list (make-term-sparse new-o
                                                                      new-c))
                                              sub-q)
                            sub-r)))))))))


  ; 习题2.91
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-sparse (variable p1) (div-terms (term-list-sparse p1)
                                              (term-list-sparse p2)))
        (error "多项式变量不同 -- ADD-POLY" (list p1 p2))))

  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (attach-tag 'poly-quotient (div-poly p1 p2))))
  (put 'neg '(polynomial) (lambda (p) (tag (neg-poly p))))
  (put 'equ? '(polynomial polynomial) equ?-poly)
  ; 习题2.88
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (neg-poly p2)))))
  (put '=zero? '(polynomial) =zero?-poly)
  (put 'variable '(polynomial) variable)
  (put 'make-sparse '(polynomial)
       (lambda (v t) (tag (make-sparse v t))))
  (put 'make-dense '(polynomial)
       (lambda (v t) (tag (make-dense v t)))))

; 习题2.89
; 习题2.90
; (dense x 1 1 1 1)
(define (install-poly-dense)
  (define (make-dense variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (tag p) (attach-tag 'dense p))
  (define (term-list-dense p) (cdr p))
  (define (term-list-sparse p)
    (define (do-sparse iter cur-order terms)
      (cond
        ((< cur-order 0) iter)
        ((= 0 (car terms)) (do-sparse iter (- cur-order 1) (cdr terms)))
        (else (do-sparse (append iter (list cur-order (car terms)))
                         (cdr terms)))))
    (do-sparse '() (- (length terms) 1) terms))
  (put 'term-list-dense '(dense) term-list-dense)
  (put 'term-list-sparse '(dense) term-list-sparse)
  (put 'make-dense '(dense)
       (lambda (v t) (tag (make-dense v t))))
  (put 'variable '(dense) variable))

; (sparse x (2 2) (1 1) (0 1))
(define (install-poly-sparse)
  (define (make-sparse variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (tag p) (attach-tag 'sparse p))
  (define (term-list-sparse p) (cdr p))
  (define (term-list-dense p)
    (define (do-dense iter cur-order terms)
      (cond
        ((< cur-order 0) iter)
        ((and (null? terms) (>= cur-order 0))
         (do-dense (append iter '(0)) (- cur-order 1) terms))
        (else (let ((cur-term (car terms)))
                (if (= cur-order (car cur-term))
                    (do-dense (append iter (list (cadr cur-term))) (- cur-order 1) (cdr terms))
                    (do-dense (append iter '(0)) (- cur-order 1) terms))))))
    (do-dense '() (car (car (term-list-sparse p))) (cdr p)))

  (put 'term-list-dense '(sparse) term-list-dense)
  (put 'term-list-sparse '(sparse) term-list-sparse)
  (put 'make-sparse '(sparse)
       (lambda (v t) (tag (make-sparse v t))))
  (put 'variable '(sparse) variable))

(install-poly-sparse)
(install-poly-dense)

; 习题2.92 不能懂，暂略

; 因多项式不能自然融入数习，扩充练习：有理函数暂略
; 习题2.93 暂略
; 习题2.94 暂略
; 习题2.95 暂略
; 习题2.96 暂略
; 习题2.97 暂略
