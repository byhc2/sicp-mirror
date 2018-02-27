#!/usr/bin/guile
!#

(define-module (scheme-number)
               #: export (install-scheme-number-package))

(add-to-load-path ".")
(use-modules (generic-arithmetic))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x))))

; 此时业已不需make-scheme-number
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

