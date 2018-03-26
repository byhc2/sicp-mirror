#!/usr/bin/guile
!#

; 习题3.22

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?) (eq? front-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (do-print-queue queue)
      (define (iter x)
        (display (car x))
        (if (eq? x rear-ptr)
            (display ")")
            (begin
              (display " ")
              (iter (cdr x)))))
      (display "(")
      (iter queue))

    (define (dispatch m)
      (cond
        ((eq? m 'empty-queue?) (empty?))
        ((eq? m 'front-queue)
         (if (empty?)
             (error "FRONT called with empty queue")
             (car front-ptr)))
        ((eq? m 'insert-queue!)
         (lambda (x)
           (let ((item (cons x '())))
             (if (empty?)
                 (begin
                   (set-front-ptr! item)
                   (set-rear-ptr! item)
                   dispatch)
                 (begin
                   (set-cdr! rear-ptr item)
                   (set-rear-ptr! item)
                   dispatch)))))
        ((eq? m 'display) (do-print-queue front-ptr))))
    dispatch))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (print-queue queue)
  (queue 'display))

(define x (make-queue))
(insert-queue! x 2)
(insert-queue! x 4)
(insert-queue! x 3)
(insert-queue! x 3)
(insert-queue! x 3)
(insert-queue! x 3)
(insert-queue! x 3)
(insert-queue! x 3)
(print-queue x)
(newline)
