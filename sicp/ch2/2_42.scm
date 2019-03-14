#!/usr/bin/guile
!#

; 习题2.42

(define (fold-right op init seq)
  (if (null? seq)
    init
    (op (car seq) (fold-right op init (cdr seq)))
    )
  )

(define (adjoin-position row board)
  (cons row board)
  )

(define (enumerate beg end step)
  (if (> beg end)
    (list)
    (cons beg (enumerate (+ beg step) end step))
    )
  )

(define (add-rows boardset board-size)
  (fold-right append (list) (map (lambda (oneboard)
				   (map (lambda (i) (adjoin-position i oneboard))
					(enumerate 1 board-size 1))
				   ) boardset)
	      )
  )

(define (check-row board)
  (if (null? board)
    #t
    (null? (filter (lambda (x) (= x (car board))) (cdr board)))
    )
  )

(define (check-dia-up k board)
  (cond ((or (<= k 0) (null? board)) #t)
	((= k (car board)) #f)
	(else check-dia-up (- k 1) (cdr board))
	)
  )

(define (check-dia-down k board)
  (cond ((null? board) #t)
	((= k (car board)) #f)
	(else check-dia-up (+ k 1) (cdr board))
	)
  )

(define (check-dia k board)
  (define cur (car board))
  (and (check-dia-up (- cur 1) (cdr board))
       (check-dia-down (+ cur 1) (cdr board))
       )
  )

(define (safe? k board)
  (and
    (check-row board)
    (check-dia k board)
    )
  )

(define (queens n)
  (define (queen-col k)
    (if (= k 0)
      (list (list))
      (filter (lambda (board) (safe? k board))
	      (add-rows (queen-col (- k 1)) n)
	      )
      )
    )
  (queen-col n)
  )

(display (queens 5))
(newline)

; 习题2.43 略
