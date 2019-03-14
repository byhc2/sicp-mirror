#!/usr/bin/racket
#lang scheme

; 习题3.6
(define (rand m . args)
  (cond
    ((eq? m 'generate)
     (if (null? args)
         (random)
         (random (car args))))
    ((eq? m 'reset)
     (begin
       (random-seed (car args))
       (current-pseudo-random-generator)))
    (else (error "未知命令"))))

(display (rand 'generate))
(newline)
(display (rand 'reset 0))
(newline)
(display (rand 'generate))
(newline)
(display (rand 'generate))
(newline)
(display (rand 'generate))
(newline)
(display (rand 'reset 0))
(newline)
(display (rand 'generate))
(newline)
(display (rand 'generate))
(newline)
(display (rand 'generate))
(newline)
