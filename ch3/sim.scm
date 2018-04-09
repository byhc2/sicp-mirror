#!/usr/bin/guile
!#

(add-to-load-path ".")
(load "logical.scm")

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(set-signal! input-1 0)
(set-signal! input-2 0)
(set-signal! sum 0)
(set-signal! carry 0)

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
(set-signal! input-2 1)
(propagate)


;(define a (make-wire))
;(define b (make-wire))
;(define o (make-wire))
;(probe 'o o)
;
;(or-gate a b o)
;(set-signal! a 1)
;(propagate)
;(set-signal! b 1)
;(propagate)

(newline)
