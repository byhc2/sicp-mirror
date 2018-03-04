#!/usr/bin/guile
!#

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (adjoin-term term term-list)
(if (= (coeff term) 0)
  term-list
  (cons term term-list)))

(define (empty-termlist? term-list) (null? term-list))
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (add-terms L1 L2)
  (cond
    ((empty-termlist? L1) L2)
    ((empty-termlist? L2) L1)
    (else (let ((t1 (first-term L1))
                (t2 (first-term L2)))
            (cond
              ((> (order t1) (order t2))
               (adjoin-term t1 (add-terms (rest-terms L1) L2)))
              ((< (order t1) (order t2))
               (adjoin-term t2 (add-terms L1 (rest-terms L2))))
              (else (adjoin-term
                      (make-term (order t1)
                                 (+ (coeff t1) (coeff t2)))
                      (add-terms (rest-terms L1) (rest-terms L2)))))))))

(display (add-terms '((2 1) (1 2) (0 1)) '((2 1) (1 2) (0 1))))
