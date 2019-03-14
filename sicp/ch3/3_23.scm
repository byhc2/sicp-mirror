#!/usr/bin/guile
!#

; 习题3.23
; 仅实现部分框架

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?) (eq? front-ptr '()))
    (define (set-front-ptr! x) (set! front-ptr x))
    (define (set-rear-ptr! x) (set! rear-ptr x))
    (define (print-deque)
      (define (iter x)
        (display (car x))
        (if (eq? x rear-ptr)
            (display ")")
            (begin
              (display " ")
              (iter (cdr x)))))
      (begin
        (display "(")
        (iter front-ptr)))
    (define (dispatch m)
      (cond
        ((eq? m 'empty-queue?) (empty?))
        ((eq? m 'front-deque) (front))
        ((eq? m 'rear-deque) (rear))
        ((eq? m 'insert-front)
         (lambda (x)
           (let ((item (cons x '())))
             (if (empty?)
                 (begin
                   (set-front-ptr! item)
                   (set-rear-ptr! item))
                 (begin
                   (set-cdr! item front-ptr)
                   (set-front-ptr! item))))))
        ((eq? m 'insert-rear)
         (lambda (x)
           (let ((item (cons x '())))
             (if (empty?)
                 (begin
                   (set-front-ptr! item)
                   (set-rear-ptr! item))
                 (begin
                   (set-cdr! rear-ptr item)
                   (set-rear-ptr! item))))))
        ((eq? m 'display) (print-deque))))
      dispatch))

(define (insert-front deque item)
  ((deque 'insert-front) item))
(define (insert-rear deque item)
  ((deque 'insert-rear) item))
(define (print-queue deque)
  (deque 'display))

(define x (make-deque))
(insert-front x 1)
(insert-rear x 2)
(insert-front x 3)
(insert-rear x 4)
(insert-front x 5)
(insert-rear x 6)
(print-queue x)
(newline)
