#!/usr/bin/guile
!#

; 习题2.46

(define (make-vect a b)
  (cons a b))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v . w)
  (define (add-two-vect v1 v2)
    (make-vect
      (+ (xcor-vect v1) (xcor-vect v2))
      (+ (ycor-vect v1) (ycor-vect v2))
      )
    )
  (define (add-vect-iter cur vl)
    (if (null? vl)
      cur
      (add-vect-iter (add-two-vect cur (car vl)) (cdr vl))
      )
    )
  (add-vect-iter v w)
  )

(define (sub-vect v1 v2)
  (make-vect
    (- (xcor-vect v1) (xcor-vect v2))
    (- (ycor-vect v1) (ycor-vect v2))
    )
  )

(define (scale-vect a v)
  (make-vect
    (* a (xcor-vect v))
    (* a (ycor-vect v))
    )
  )

; 习题2.47
; 此处仅以一种实现
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2)
  )

(define (origin-frame frame)
  (list-ref frame 0))
(define (edge1-frame frame)
  (list-ref frame 1))
(define (edge2-frame frame)
  (list-ref frame 2))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (scale-vect (xcor-vect v) (edge1-frame frame))
      (scale-vect (ycor-vect v) (edge2-frame frame))
      )
   )
  )

; 习题2.48

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (draw-line start end)
  #t
  )

; 习题2.49
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
	(draw-line
	  ((frame-coord-map frame) (start-segment segment))
	  ((frame-coord-map frame) (end-segment segment))
	  )
	)
      segment-list))
  )

(define pa
  (segments->painter
    (list
      (make-segment (make-vect 0 0) (make-vect 0 1))
      (make-segment (make-vect 0 1) (make-vect 1 1))
      (make-segment (make-vect 1 1) (make-vect 1 0))
      (make-segment (make-vect 1 0) (make-vect 0 0))
      )
    )
  )
(define pb
  (segments->painter
    (list
      (make-segment (make-vect 0 0) (make-vect 1 1))
      (make-segment (make-vect 0 1) (make-vect 1 0))
      )
    )
  )
(define pc
  (segments->painter
    (list
      (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
      (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
      (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
      (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
      )
    )
  )

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	  (make-frame new-origin
		      (sub-vect (m corner1) new-origin)
		      (sub-vect (m corner2) new-origin)))))))

(define (painter frame)
  (display frame))

; 习题2.50 暂略
; 习题2.51 暂略
; 习题2.52 暂略
(display ((frame-coord-map (make-frame (make-vect 0.5 0.5)
				       (make-vect 0 0.5)
				       (make-vect 0.5 0)
				       )) (make-vect 0.5 0.5)))
(newline)
