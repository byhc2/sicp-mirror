#lang racket

(provide cons-stream
         the-empty-stream
         stream-null?
         stream-ref
         display-stream-n
         stream-map
         display-stream
         stream-car
         stream-cdr
         stream-enumerate-interval
         stream-filter
         integers-starting-from
         integers
         ones
         fibs
         primes
         stream-accumulate
         display-line
         mul-stream
         add-streams
         sqrt-stream
         stream-limit)

; 此cons-stream错误
; 因stream-delay为函数，则使用时参数b预先已求值
; 故并未真正实现延迟求值
; 故cons-stream与delay与force必为特殊定义
;(define (stream-delay f)
;  (lambda () (let ((already-run? #f)
;                   (result #f))
;               (if (not already-run?)
;                   (begin
;                     (set! result (f))
;                     (set! already-run? #t)
;                     result)
;                   result))))
;(define (stream-force delay-obj) (delay-obj))

;(define (cons-stream a b)
;  (cons a (stream-delay b)))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define the-empty-stream '())

(define (stream-null? s)
  (null? s))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

; 输出流s的前n项
(define (display-stream-n s n)
  (if (= n 0)
      (newline)
      (begin
        (display (stream-car s))
        (newline)
        (display-stream-n (stream-cdr s) (- n 1)))))

(define (stream-map0 proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (car s))
                   (stream-map0 proc (stream-cdr s)))))

; 习题3.50
; 暂不校验argstreams各个表长度
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond
    ((stream-null? stream) the-empty-stream)
    ((pred (stream-car stream))
     (cons-stream (stream-car stream)
                  (stream-filter pred (stream-cdr stream))))
    (else
      (stream-filter pred (stream-cdr stream)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))
(define ones (cons-stream 1 ones))

(define (divisable? x y) (= (remainder x y) 0))
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream (stream-car stream)
               (sieve (stream-filter (lambda (x) (not (divisable? x (stream-car
                                                                      stream))))
                                     (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

; 流s前n项和
(define (stream-accumulate s n)
  (define (iter sum s0 i)
    (if (> i n)
        sum
        (iter (+ sum (stream-car s0)) (stream-cdr s0) (+ i 1))))
  (iter 0 s 0))

(define (mul-stream . args)
  (apply stream-map * args))

(define (add-streams . args)
  (apply stream-map + args))

; 开方流
(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess)) 2))
; 习题3.63
; Louis代码每次需要求解流中第n个值时
; 都要重新生成流，然后计算前n-1项
; 估计复杂度为n^2，因算n需要算n-1，算n-1要算n-2
; 另
; guesses版无此问题
; 盖因计算n项时，递归引用guesses本身，而非构造新流
; 此外，因delay有记忆，可记住guesses之前结果
; 若delay无记忆，则复杂度亦为n^2
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses)

; 习题3.64
(define (stream-limit s tolerance)
  (let ((s1 (stream-ref s 0))
        (s2 (stream-ref s 1)))
    (if (< (abs (- s1 s2)) tolerance)
        s1
        (stream-limit (stream-cdr s) tolerance))))
;(define (stream-limit s tolerance)
;  (if (< (abs (- (stream-ref s 0) (stream-ref s 1))) tolerance)
;      (stream-ref s 0)
;      (stream-limit (stream-cdr s) tolerance)))
