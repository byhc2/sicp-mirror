#!/usr/bin/guile
!#

; 习题1.35
; 习题1.36

; 显然，黄金分割值是方程x^2-x-1=0的根
; 变换得到 x = 1 + 1/x
; 显然，就是求函数f(x) = 1 + 1/x的不动点

; 所有求一元方程根的问题，都可以设法把方程变换为左边只含有未知数
; 右边为未知数的函数的形式：x = f(x)
; 然后问题就转化为求函数f(x)的不动点

; 个人猜测，所谓f(x)的不动点
; 就是f(x)在平面直角座标系内图形与y=x直线的交点
; 求解交点，就是联立方程y=f(x)与y=x
; 消去y，就是x = f(x)，就是要求的方程
; 此处要特别注意f(x)关于y=x轴对称的情况
; 书中y = x/y就是此类情况
; 求不动点时，x是已知的，所求函数f(y) = a/y
; 是一个倒数函数，关于y = x轴对称
; 求不动点时，随机选取的起始点会来回振荡
; 因此，需要将方程做变换，例如两边 2y = y + x/y -> y = (y + x/y)/2

(define (fixed-point x f)
  (let (
	(update-x (f x))
	(close-enough (lambda (x y) (< (abs (- x y)) 0.00001)))
	)
    (if (close-enough update-x x)
      update-x
      (begin
	(display "update x to ")
	(display update-x)
	(newline)
	(fixed-point update-x f)
      )
      )
    )
  )

(display (fixed-point 1.0 sin))
(newline)
; (display (fixed-point 1.0 cos))
; (newline)

(display (fixed-point 1.0 (lambda (x) (+ 1 (/ 1 x)))))
(newline)

(display "====================================")
(newline)

(display (fixed-point 5.0 (lambda (x) (/ (log 1000) (log x)))))
(newline)
