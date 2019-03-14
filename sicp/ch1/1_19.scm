#!/usr/bin/guile
!#

; 习题1.19

(define (even x)
  (= (remainder x 2) 0)
  )

(define (fib n)
  (define (fib-iter a b p q r)
    (if (= r 0)
      b
      (if (even r)
        (fib-iter
          a
          b
          (+ (* p p) (* q q))
          (+ (* 2 p q) (* q q))
          (/ r 2)
          )
        (fib-iter
          (+ (* b q) (* a q) (* a p))
          (+ (* b p) (* a q))
          p
          q
          (- r 1)
          )
        )
      )
    )
  (fib-iter 1 0 0 1 n)
  )

(display (fib 8))
(newline)

; 习题 1.20
; 正则序求值，reminder被调用18次
; 调用序列如下，if后的#x表示为了求if的条件部分，需要调用reminder的部分
; (gcd 206 40)
; (gcd 40 (reminder 206 40))
; if #1
; (gcd (reminder 206 40) (reminder 40 (reminder 206 40)))
; if #2
; (gcd (reminder 40 (reminder 206 40)) (reminder (reminder 206 40) (reminder 40 (reminder 206 40))))
; if #4
; (gcd (reminder (reminder 206 40) (reminder 40 (reminder 206 40))) (reminder (reminder 40 (reminder 206 40)) (reminder (reminder 206 40) (reminder 40 (reminder 206 40)))))
; if #7
; (reminder (reminder 206 40) (reminder 40 (reminder 206 40)))
; 除了if条件部分调用14次外，仅最后有4次额外的reminder调用，合18次
;
; 应用序求值，reminder被调用4次
