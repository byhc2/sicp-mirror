#!/usr/bin/guile
!#

(define-module (rational-number)
               #: export (install-rational-number-package
                           make-rat))

(add-to-load-path ".")
(use-modules (generic-arithmetic))

(define (install-scheme-number-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (if (or (and (> n 0) (> d 0)) (and (< n 0) (> d 0)))
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))
        (make-rat (- 0 n) (- 0 d))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (+ (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (+ (denom x) (denom y))))
