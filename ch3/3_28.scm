#!/usr/bin/guile
!#

; 习题3.28
(define (or-gate a1 a2 output)
  (define (logical-or v1 v2)
    (if (and (= v1 0) (= v2 0))
        0
        1))
  (define (or-action-procedure)
    (let ((new-value (logical-or
                       (get-signal a1)
                       (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a1 or-action-procedure)
  'ok)
