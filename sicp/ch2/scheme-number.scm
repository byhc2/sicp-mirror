#!/usr/bin/guile
!#

;(define-module (scheme-number)
;               #: export (install-scheme-number-package))
;
;(add-to-load-path ".")
;(use-modules (environ))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number) (lambda (x y) (= x y)))
  (put 'sine '(scheme-number) (lambda (x) (tag (sin x))))
  (put 'cosine '(scheme-number) (lambda (x) (tag (cos x))))
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'neg '(scheme-number) (lambda (x) (tag (- 0 x))))
  (put 'make '(scheme-number) (lambda (x) (tag x))))
