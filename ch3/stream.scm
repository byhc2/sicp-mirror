#!/usr/bin/guile
!#

(define (stream-null? s)
  (null? s))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map0 proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map0 proc (stream-cdr s)))))

; 习题3.50
; 暂不校验argstreams各个表长度
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map car argstreams))
        (apply stream-map (cons proc (map cdr argstreams))))))

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
(define (stream-cdr stream) (stream-force (cdr stream)))

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

;(define (stream-delay f)
;  (lambda () (let ((already-run? #f)
;                   (result #f))
;               (if (not already-run?)
;                   (begin
;                     (display "-----------------")
;                     (newline)
;                     (display (f))
;                     (newline)
;                     (set! result (f))
;                     (set! already-run? #t)
;                     result)
;                   result))))
(define (stream-delay f)
  (let ((d (lambda () (f))))
    d))
(define (stream-force delay-obj) (delay-obj))

(define (cons-stream a b)
  (cons a (stream-delay b)))

(define the-empty-stream '())

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))
