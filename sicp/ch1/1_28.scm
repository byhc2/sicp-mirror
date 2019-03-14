#!/usr/bin/guile
!#

; 习题1.28

(define (expmod b e m)
  (define (square x)
    (* x x))
  (define (square-with-check x)
    (if (and (= (remainder (square x) b) 1) (not (= x 1)) (not (= (- n 1))))
      0
      (square x)
      )
    )
  (if (= e 0)
    1
    (if (even? e)
      (remainder (square-with-check (expmod b (/ e 2) m)) m)
      (remainder (* b (expmod b (- e 1) m)) m)
      )
    )
  )

(define (miller-robin n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(display (miller-robin 2))
(newline)
(display (miller-robin 4))
(newline)
