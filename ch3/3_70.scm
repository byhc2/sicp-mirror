#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

; 习题3.70

(define (merge-weighted s t weight)
  (cond
    ((stream-null? s) t)
    ((stream-null? t) s)
    (else
      (let ((scar (stream-car s))
            (tcar (stream-car t)))
        (if (< (weight scar) (weight tcar))
          (cons-stream scar (merge-weighted (stream-cdr s) t weight))
          (cons-stream tcar (merge-weighted s (stream-cdr t) weight)))))))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

; a)
(define (wa p)
  (+ (list-ref p 0) (list-ref p 1)))

(define x (weighted-pairs integers integers wa))

(display-stream-n x 10)

; b)
; 题目应为，i或j均不能被2，3，5整除
(define (no-2-3-5 x)
  (not (or (eq? (remainder x 2) 0)
           (eq? (remainder x 3) 0)
           (eq? (remainder x 5) 0))))

(define (wb p)
  (let ((p0 (list-ref p 0))
        (p1 (list-ref p 1)))
    (+ (* 2 p0) (* 3 p1) (* 5 p0 p1))))

(define x2 (stream-filter
             (lambda (i) (and (no-2-3-5 (list-ref i 0))
                              (no-2-3-5 (list-ref i 1))))
             (weighted-pairs integers integers wb)))
(display-stream-n x2 10)

; 习题3.71
(define (w3-71 p)
  (let ((p10 (list-ref p 0))
        (p11 (list-ref p 1)))
    (+ (* p10 p10 p10) (* p11 p11 p11))))

(define x3 (weighted-pairs integers integers w3-71))

(define (filter-ramanujan s)
  (let ((scar (stream-car s))
        (scdar (stream-car (stream-cdr s))))
    (if (= (w3-71 scar) (w3-71 scdar))
      (cons-stream (list scar scdar (w3-71 scar))
                   (filter-ramanujan (stream-cdr (stream-cdr s))))
      (filter-ramanujan (stream-cdr s)))))

;(display-stream-n x3 10)
(display-stream-n (filter-ramanujan x3) 10)
; 第一个数及随后5个数分别为
; ((9 10) (1 12) 1729)
; ((9 15) (2 16) 4104)
; ((18 20) (2 24) 13832)
; ((19 24) (10 27) 20683)
; ((18 30) (4 32) 32832)
; ((15 33) (2 34) 39312)

; 习题3.72
(define (filter-ramanujan-3 s)
  (let ((scar (stream-car s))
        (scdar (stream-car (stream-cdr s)))
        (scddar (stream-car (stream-cdr (stream-cdr s)))))
    (if (= (w3-71 scar) (w3-71 scdar) (w3-71 scddar))
      (cons-stream (list scar scdar scddar (w3-71 scar))
                   (filter-ramanujan-3 (stream-cdr (stream-cdr (stream-cdr s)))))
      (filter-ramanujan-3 (stream-cdr s)))))

(display-stream-n (filter-ramanujan-3 x3) 10)
; 计算会很慢
; ((255 414) (228 423) (167 436) 87539319)
; ((346 428) (90 492) (11 493) 119824488)
; ((408 423) (359 460) (111 522) 143604279)
; ((315 525) (198 552) (70 560) 175959000)
; ((510 580) (339 661) (300 670) 327763000)

