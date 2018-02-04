#!/usr/bin/guile
!#

; 习题1.45

; 关于四次方根用平均阻尼不收敛问题
; 实际是可以收敛的
; 但是收敛过程是在不动点四周振荡并螺旋状接近
; 下面是x轴右侧的收敛序列
; 可以看到，每次仅收敛10e-10的数量级
; 因此要算好久
; 更高次方可能有生之年都算不出
; update x to 1.3165198275165828
; update x to 1.3165198271075376
; update x to 1.3165198266984932
; update x to 1.3165198262894502
; update x to 1.3165198258804083

(define (fixed-point x f)
  (let (
	(update-x (f x))
	(close-enough (lambda (x y) (< (abs (- x y)) 0.00001)))
	)
    (if (close-enough update-x x)
      update-x
      (begin
	; (display "update x to ")
	; (display update-x)
	; (newline)
	(fixed-point update-x f)
      )
      )
    )
  )

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2.0))
  )

(define (repeated f n)
  (define (repeated-iter g i)
    (if (> i n)
      g
      (repeated-iter (lambda (x) (f (g x))) (+ i 1))
      )
    )
  (repeated-iter (lambda (x) x) 1.0)
  )

(define (root-with-time x n times)
  (fixed-point 1.0 ((repeated average-damp times) (lambda (y) (/ x (expt y (- n 1))))))
  )

(define (nroot x n)
  (root-with-time x n (/ (log n) (log 2)))
  )

(display "3的1~10次方根 ")
(newline)
; (display (fixed-point 0.5 (lambda (x) (/ (+ x (/ 3 (* x x x))) 2))))
(display (nroot 3 1))
(newline)
(display (nroot 3 2))
(newline)
(display (nroot 3 3))
(newline)
(display (nroot 3 4))
(newline)
(display (nroot 3 5))
(newline)
(display (nroot 3 6))
(newline)
(display (nroot 3 7))
(newline)
(display (nroot 3 8))
(newline)
(display (nroot 3 9))
(newline)
(display (nroot 3 10))
(newline)
