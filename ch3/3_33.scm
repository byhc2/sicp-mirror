#!/usr/bin/guile
!#

(add-to-load-path ".")
(load "constrains.scm")

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(probe "Average" c)

(define (averager a b c)
  (let ((m (make-connector))
        (n (make-connector)))
    (adder a b m)
    (multiplier m n c)
    (constant 2 n)
    'ok))

(set-value! c 0 'nobody)
(set-value! a 100 'user)
(set-value! b 50 'user)

;(constant 10 a)
;(multiplier a b c)
;(set-value! a 100 'user)
;(set-value! b 50 'user)

(newline)
