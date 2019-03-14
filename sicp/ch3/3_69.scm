#!/usr/bin/racket
#lang scheme

(require "stream.scm")
(require "series.scm")

; 习题3.69

; 三元组写作
; (1 1 1) (1 1 2) (1 1 3) (1 1 4) (1 1 5) (1 1 6) ...
;         (1 2 2) (1 2 3) (1 2 4) (1 2 5) (1 2 6) ...
;                 (1 3 3) (1 3 4) (1 3 5) (1 3 6) ...
; ...
; 固定s为1，第一层
; 以s为2，第二层
;
;         (2 2 2) (2 2 3) (2 2 4) (2 2 5) (2 2 6) ...
;                 (2 3 3) (2 3 4) (2 3 5) (2 3 6) ...
; ...

; 由整体看，是一无限高三棱锥。除去第一层，剩余部分为递归
; 故可由第一层，及剩余部分组成
; 第一层，可视为二元组前拼固定元素2
(define (triples s t u)
  (cons-stream (list (stream-car s) (stream-car t) (stream-car u))
               (interleave (stream-cdr (stream-map (lambda (k) (cons (stream-car s) k))
                                                   (pairs t u)))
                           (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (check i j k)
  (define (square x) (* x x))
  (= (+ (square i) (square j)) (square k)))

(define x (stream-filter (lambda (i) (apply check i)) (triples integers integers integers)))
;(define x (stream-filter (lambda (i) #t) (triples integers integers integers)))
(display-stream-n x 10)

; 仅能生成前6组勾股数，因interleave枚举策略问题，导致大量垃圾数据
