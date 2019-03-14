#!/usr/bin/guile
!#

; 队列表示

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with empty queue -- " queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((x (cons item '())))
    (if (empty-queue? queue)
        (begin
          (set-front-ptr! queue x)
          (set-rear-ptr! queue x)
          queue)
        (begin
          (set-cdr! (rear-ptr queue) x)
          (set-rear-ptr! queue x)
          queue))))

(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "DELETE called with empty queue")
      (let ((x (front-ptr queue)))
        (set-front-ptr! queue (cdr x))
        queue)))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))

; 习题3.21
; 于lisp而言，队列仅为简单元组，故输出为(car cdr)
; 所谓二次入队列，实为输出cdr
(define (print-queue queue)
  (define (iter x)
    (display (car x))
    (if (eq? x (rear-ptr queue))
        (display ")")
        (begin
          (display " ")
          (iter (cdr x)))))
  (display "(")
  (iter (front-ptr x)))

;(define x (make-queue))
;(insert-queue! x 4)
;(insert-queue! x 3)
;(insert-queue! x 2)
;(insert-queue! x 1)
;(print-queue x)
;(newline)
