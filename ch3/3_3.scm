#!/usr/bin/guile
!#

; 习题3.3
; 习题3.4
(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
          balance)
        "余额不足"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define check-pass
    (let ((passwd-try 0))
      (lambda (pass m)
        (cond
          ((eq? pass passwd) (begin (set! passwd-try 0) (dispatch m)))
          ((>= passwd-try 7) (error "数次错误，报警"))
          (else (begin (set! passwd-try (+ passwd-try 1))
                  (lambda (x) "密码错误")))))))

  (define (dispatch m)
    (cond
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "命令错误 -- MAKE-ACCOUNT" m))))

  check-pass)

(define acc (make-account 100 'xxx))
(display ((acc 'xxx 'withdraw) 10))
(newline)
(display ((acc 'xxy 'withdraw) 10))
(newline)
(display ((acc 'xxy 'withdraw) 10))
(newline)
(display ((acc 'xxy 'withdraw) 10))
(newline)
(display ((acc 'xxy 'withdraw) 10))
(newline)
(display ((acc 'xxy 'withdraw) 10))
(newline)
(display ((acc 'xxy 'withdraw) 10))
(newline)
(display ((acc 'xxy 'withdraw) 10))
(newline)
(display ((acc 'xxy 'withdraw) 10))
(newline)
(display ((acc 'xxy 'withdraw) 10))
(newline)
