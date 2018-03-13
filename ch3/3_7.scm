#!/usr/bin/guile
!#

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

(define (make-joint acc new-pass passwd)
  (define check-pass
    (lambda (pass m)
      (cond
        ((eq? pass new-pass) (acc passwd m))
        (else (error "密码错误")))))
  check-pass)

(define acc1 (make-account 100 'xxx))
(define acc2 (make-joint acc1 'yyy 'xxx))
(display ((acc1 'xxx 'withdraw) 10))
(newline)
(display ((acc2 'yyy 'withdraw) 10))
(newline)
