#!/usr/bin/guile
!#

; 习题3.29

(define (inverter input output)
  (define (logical-not v)
    (if (= v 0)
        1
        0))
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input ivert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (logical-and v1 v2)
    (if (and (= v1 1) (= v2 1))
        1
        0))
  (define (and-action-procedure)
    (let ((new-value (logical-and
                       (get-signal a1)
                       (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a1 and-action-procedure)
  'ok)

; 此处定义或门延时为与门延时加二倍或门延时
(define (or-gate a1 a2 output)
  (let ((w1 (make-wire))
        (w2 (make-wire))
        (w3 (make-wire)))
    (inverter a1 w1)
    (inverter a2 w2)
    (and-gate w1 w2 w3)
    (inverter w3 output)
    'ok)
