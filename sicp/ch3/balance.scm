#!/usr/bin/guile
!#

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "余额不足"))

(display (withdraw 10))
(newline)
(display (withdraw 10))
(newline)
(display (withdraw 100))
(newline)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "余额不足"))))

(display (new-withdraw 10))
(newline)
(display (new-withdraw 10))
(newline)
(display (new-withdraw 100))
(newline)
