#!/usr/bin/guile
!#

; 习题3.5

(define (monte-carlo trials experiment)
  (define (iter trials-remain trials-pass)
    (cond
      ((= trials-remain 0) (/ trials-pass trials))
      ((experiment) (iter (- trials-remain 1) (+ trials-pass 1)))
      (else (iter (- trials-remain 1) trials-pass))))
  (iter trials 0))

(define (rand-in-range x1 x2)
  (+ (random (- x2 x1)) x1))

(define (estimate-integral p x1 x2 y1 y2 try)
  (define one-trail
    (lambda ()
      (let ((x (rand-in-range x1 x2))
            (y (rand-in-range y1 y2)))
        (p x y))))
  (* 4 (monte-carlo try one-trail)))

(define (predicate x y)
  (< (+ (* x x) (* y y)) 1))

(display (estimate-integral predicate -1.0 1.0 -1.0 1.0 1000000.0))
(newline)
