; 无穷级数相关定义

; 某级数不含常数项积分级数
(define (integrate-series s)
  (let ((coef (stream-map / ones integers)))
    (mul-stream coef s)))

; e^x幂级数
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

; 幂级数乘以常数
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

; 正弦，余弦幂级数
(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; 二幂级数相乘级数
;   (a0 + a1 + ... )(b0 + b1 + ...)
; = a0*b0 + a0*(b1 + b2 + ...) + b0 * (a1 + a2 + ...) + (a1 + a2 + ...)(b1 + b2 + ...)
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                 (scale-stream (stream-cdr s2) (stream-car s1))
                 (scale-stream (stream-cdr s1) (stream-car s2))
                 (mul-series (stream-cdr s1) (stream-cdr s2)))))

; 习题3.61
; 幂级数倒数
(define (invert-series s)
  (define x-series
    (cons-stream 1
                 (scale-stream
                   (mul-series (stream-cdr s) x-series)
                   -1)))
  x-series)

; 习题3.62
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "CONSTANT PART OF S2 IS 0")
      (mul-series s1 (invert-series s2))))

; 正切幂级数
(define tan-series
  (div-series sine-series cosine-series))
