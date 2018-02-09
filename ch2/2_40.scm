#!/usr/bin/guile
!#

; 习题2.39

(define (prime? n)
  (= n (smallest-divisor n))
  )

(define (smallest-divisor n)
  (define (square x)
    (* x x)
    )
  (define (find-divisor x)
    (if (> (square x) n)
      n
      (if (= (remainder n x) 0)
	x
	(find-divisor (+ x 1))
	)
      )
    )
  (find-divisor 2)
  )

(define (fold-right op init seq)
  (if (null? seq)
    init
    (op (car seq) (fold-right op init (cdr seq)))
    )
  )

(define (uniq-pairs . w)
  (define (uniq-imp a b)
    (fold-right append (list)
		(map (lambda (i) (map (lambda (j) (cond
						    ((and (list? i) (list? j)) (append i j))
						    ((not (list? j)) (list i j))
						    (else (cons i j)))
					) b)) a)
		)
    )
  (define (uniq-pairs-imp l)
    (if (null? (cdr l))
      (car l)
      (uniq-imp (car l) (uniq-pairs-imp (cdr l)))
      )
    )
  (uniq-pairs-imp w)
  )

(display (uniq-pairs (list 1 2 3)))
(newline)
(display (uniq-pairs (list 1 2 3) (list 1 2) (list 4 5)))
(newline)

(define (enumerate beg end step)
  (if (> beg end)
    (list)
    (cons beg (enumerate (+ beg step) end step))
    )
  )

(display (enumerate 1 6 1))
(newline)

(define (prime-sum-pairs n)
  (filter (lambda (item) (prime? (+ (list-ref item 0) (list-ref item 1)))) (filter (lambda (item) (< (list-ref item 0) (list-ref item 1))) (uniq-pairs (enumerate 1 (- n 1) 1) (enumerate 1 n 1))))
  )

(display (prime-sum-pairs 6))
(newline)

; 习题2.41
; 此处以1<=i<j<k<=n为i j k不同
(define (sum-of-triple n s)
  (filter (lambda (x) (= (+ (list-ref x 0) (list-ref x 1) (list-ref x 2)) s))
	  (filter (lambda (x) (< (list-ref x 0) (list-ref x 1) (list-ref x 2))) (uniq-pairs (enumerate 1 n 1) (enumerate 1 n 1) (enumerate 1 n 1)))
	  )
  )

(display (sum-of-triple 10 18))
(newline)
