#!/usr/bin/guile
!#

; 习题2.7

(define (make-interval a b) (cons a b))
(define (lowwer-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (let (
	(new-lower (+ (lowwer-bound x) (lowwer-bound y)))
	(new-upper (+ (upper-bound x) (upper-bound y)))
	)
    (make-interval new-lower new-upper)
    )
  )

(define (mul-interval x y)
  (let (
	(p1 (* (lowwer-bound x) (lowwer-bound y)))
	(p2 (* (lowwer-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lowwer-bound y)))
	(p4 (* (upper-bound x) (upper-bound y)))
	)
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
    )
  )

; 习题2.11
; 每个区间，根据其根据其端点位置，有三种情况：
;	0 < x < y
;	x < 0 < y
;	x < y < 0
; 据此来划分
(define (new-mul-interval x y)
  (let
    (
     (sx (lowwer-bound x))
     (ex (upper-bound x))
     (sy (lowwer-bound y))
     (ey (upper-bound y))
     )
    (cond
      ((< 0 sx ex)
       (cond
	 ((< 0 sy ey) (make-interval (* sx sy) (* ex ey)))
	 ((< sy 0 ey) (make-interval (* ex sy) (* ex ey)))
	 ((< sy ey 0) (make-interval (* ex sy) (* sx ey)))
	 )
       )
      ((< sx 0 ex)
       (cond
	 ((< 0 sy ey) (make-interval (* sx ey) (* ex ey)))
	 ; 超过2次乘法，不知有无他法
	 ((< sy 0 ey) (make-interval (min (* sx ey) (* sy ex)) (max (* sx sy) (* ex ey))))
	 ((< sy ey 0) (make-interval (* ex sy) (* sx sy)))
	 )
       )
      ((< sx ex 0)
       (cond
	 ((< 0 sy ey) (make-interval (* sx ey) (* ex sy)))
	 ((< sy 0 ey) (make-interval (* sx ey) (* sx sy)))
	 ((< sy ey 0) (make-interval (* ex ey) (* sx sy)))
	 )
       )
      )
    )
  )

(define (div-interval x y)
  (if (and (<= (lowwer-bound y) 0) (<= 0 (upper-bound y)))
    (error "divisor cross zero")
    (mul-interval
      x
      (make-interval
	(/ 1.0 (upper-bound y))
	(/ 1.0 (lowwer-bound x))
	)
      )
    )
  )

(display (add-interval
	   (make-interval 2 3)
	   (make-interval 3 7)
	   )
	 )
(newline)
(display (mul-interval
	   (make-interval 2 3)
	   (make-interval 3 7)
	   )
	 )
(newline)

; 习题2.8
; a < x < b，c < y < d
; -d < -y < -c
; x - y ==> x + (-y)

(define (sub-interval x y)
  (add-interval
    x
    (make-interval (- 0 (upper-bound y)) (- 0 (lowwer-bound y)))
    )
  )

(display (sub-interval
	   (make-interval 2 3)
	   (make-interval 3 7)
	   )
	 )
(newline)

; 习题2.9
; 宽度函数 (lambda (x) (/ (- (upper-bound x) (lowwer-bound x)) 2))
; 加和函数展开为
; (make-interval
;	(+ (lowwer-bound x) (lowwer-bound y))
;	(+ (upper-bound x) (upper-bound y))
; )
; 其结果宽度为
; (/ (-
;	(+ (upper-bound x) (upper-bound y))
;	(+ (lowwer-bound x) (lowwer-bound y))
;    )
; 2)
; ==>
; (/ (+
;	(- (upper-bound x) (lowwer-bound x))
;	(- (upper-bound x) (lowwer-bound x))
;    )
; 2)
; ==>
; (/ (+
;	(with-of-x)
;	(with-of-y)
;    )
; 2)
; 所以，二区间之和差之宽度是其和差之函数
; 举例说明乘除不是略

(define (make-center-width c w)
  (make-interval (- c w) (+ c w))
  )

(define (center i)
  (/ (+ (upper-bound i) (lowwer-bound i)) 2)
  )

(define (width i)
  (/ (- (upper-bound i) (lowwer-bound i)) 2)
  )

; 习题2.12
(define (make-center-percent c v)
  (make-interval (* c (- 1 v)) (* c (+ 1 v)))
  )

(define (percent i)
  (/ (width i) (center i) 0.01)
  )

; 习题2.13
; 按照中点误差来定义区间，任意两个正区间可以被表示为[ c1(1 - v1), c1(1 + v1) ]，和[ c2(1 - v2), c2(1 + v2) ]
; 则区间之积为[c1c2(1 - v1)(1 - v2),   c1c2(1 + v1)(1 + v2)]
; ==>
; [c1c2(1 - (v1 + v2) + v1v2),   c1c2(1 + (v1 + v2) + v1v2)]
; 在v1和v2很小的时候，v1v2乘积可以认为约等于0，所以新区间的误差可以认为就是(v1 + v2)

(display "===========================")
(newline)
(display (div-interval
	   (make-interval 2 4)
	   (make-interval 2 4)
	   )
	 )
(newline)

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2))
  )

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1) (div-interval one r2))
		  )
   )
  )

(display "===========================")
(newline)
(display (par1
	   (make-interval 2 4)
	   (make-interval 2 4)
	  )
	 )

(newline)
(display "===========================")
(newline)
(display (par2
	   (make-interval 2 4)
	   (make-interval 2 4)
	  )
	 )
(newline)

(display "===========================")
(newline)
(display (div-interval
	   (make-interval 0.999 1.001)
	   (make-interval 0.999 1.001)
	   )
	 )
(newline)

; 习题2.15
; 习题2.16
; 立两电阻并联，每个电阻变动在0.95到1.05欧姆
; 则实际并联电阻将在0.475到0.525欧姆范围变动
; 以octave做图亦证实
; ------------------------------
; X=linspace (0.95,1.05,100)
; Y=X
; [xx, yy]=meshgrid (X,Y)
; Z=(xx.*yy)./(xx+yy)
; mesh(X,Y,Z)
; xlabel ("x")
; ylabel ("y")
; zlabel ("z")
; ------------------------------
; 使用par2返回结果确优于par1
; 此问题盖因范围算子运算定义不完善
; 1, 该系统未定义单位元与零元
; 2, 仅加减算子在该系统上互逆，乘除不互逆（排除0元），因此(A/B) * (B/A) != 1
; 3, 常见运算规律与实数域不同，如 (A/B) * (B/A) 结果非单位元
; 2，3条或言同一事
; 具体到2.16 (1 / (1/R1 + 1/R2))实不等于(R1R2)/(R1 + R2)
; 定义完善范围运算算子问题实异常困难，此处暂不深究
; 可于谷歌学术检索 interval computation
