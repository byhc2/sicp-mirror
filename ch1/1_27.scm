#!/usr/bin/guile
!#

; 习题1.27

(define (expmod b e m)
  (define (square x)
    (* x x))
  (if (= e 0)
    1
    (if (even? e)
      (remainder (square (expmod b (/ e 2) m)) m)
      (remainder (* b (expmod b (- e 1) m)) m)
      )
    )
  )

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

; 561不是质数
(display (fermat-test 561))
(newline)
(display (fermat-test 1105))
(newline)
(display (fermat-test 1729))
(newline)
(display (fermat-test 2465))
(newline)
(display (fermat-test 2821))
(newline)
(display (fermat-test 6601))
(newline)
