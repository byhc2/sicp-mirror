#!/usr/bin/guile
!#

; 习题2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch mm)
  (list-ref mm 0))

(define (right-branch mm)
  (list-ref mm 1))

(define (branch-length b)
  (list-ref b 0))

(define (branch-structure b)
  (list-ref b 1))

(define (total-weight mm)
  (if (not (list? mm))
    mm
    (+ (total-weight (branch-structure (left-branch mm))) (total-weight (branch-structure (right-branch mm))))
    )
  )

(define mm1 (make-mobile
              (make-branch 1 1)
              (make-branch 1 1)
              )
  )

(display (total-weight (make-mobile
                         (make-branch 2 mm1)
                         (make-branch 4 mm1)
                         )))
(newline)

(define (balanced? mm)
  (if (not (list? mm))
    #t
    (let (
          (lw (total-weight (branch-structure (left-branch mm))))
          (larm (total-weight (branch-length (left-branch mm))))
          (rw (total-weight (branch-structure (right-branch mm))))
          (rarm (total-weight (branch-length (right-branch mm))))
          )
      (and (= (* lw larm) (* rw rarm))
           (balanced? (branch-structure (left-branch mm)))
           (balanced? (branch-structure (right-branch mm)))
           )
      )
    )
  )

(display (balanced? mm1))
(newline)
