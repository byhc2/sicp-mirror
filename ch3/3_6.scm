#!/usr/bin/guile
!#

; 习题3.6
(define (rand m . args)
  (cond
    ((eq? m 'generate)
     (if (null? args)
         (random 1.0)
         (random (car args))))
    ((eq? m 'reset)
     (begin
       (set! *random-state* (seed->random-state (car args)))
       *random-state*))
    (else (error "未知命令"))))

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
