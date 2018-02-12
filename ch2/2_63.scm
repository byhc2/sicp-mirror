#!/usr/bin/guile
!#

; 习题2.63

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry lb rb) (list entry lb rb))

(define (element-of-set? e s)
  (cond ((null? s) #f)
	((= e (entry s)) #t)
	((< e (entry s)) (element-of-set? e (left-branch s)))
	(else (element-of-set? e (left-branch s)))))

(define (adjoin-set e s)
  (cond ((element-of-set? e s) s)
	((null? s) (make-tree e '() '()))
	((< e (entry s)) (make-tree (entry s)
				    (adjoin-set e (left-branch s))
				    (right-branch s)))
	(else (make-tree (entry s)
			 (left-branch s)
			 (adjoin-set e (right-branch s))))))

(define t1 '(5
	     (2
	      (1 () ())
	      (3 () ()))
	     (8
	      (7 () ())
	      (9 () ()))))
(display (adjoin-set 4 t1))
(newline)

; 中序遍历
(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
	    (cons (entry tree)
		  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result)
    (if (null? tree)
      result
      (copy-to-list (left-branch tree)
		    (cons (entry tree)
			  (copy-to-list (right-branch tree) result)))))
  (copy-to-list tree '()))

; a 一样的
; b 若只计tree->list-x调用，其余调用计O(1)，则二者复杂度相同，均为O(n)

(display (tree->list-1 t1))
(newline)
(display (tree->list-2 t1))
(newline)

; 习题2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
	(let ((left-tree (car left-result))
	      (non-left-elts (cdr left-result))
	      (right-size (- n left-size 1)))
	  (let ((this-entry (car non-left-elts))
		(right-result (partial-tree (cdr non-left-elts) right-size)))
	    (let ((right-tree (car right-result))
		  (remaining-elts (cdr right-result)))
	      (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

; partial-tree自底向上构造树
; 递归说法：
;   先构造一(/ n 2)大小左树
;   剩余元素，取第一为值，剩余构造右子树
;   合成
; 只论partial-tree调用次数，时间复杂度O(n)

(define l1 (list 1 2 3 4 5 6 7))
(display (list->tree l1))
(newline)

; 习题2.65
(define (union-set s1 s2)
  (define (union-set-list s1 s2)
    (cond ((null? s1) s2)
	  ((null? s2) s1)
	  (else (let ((e1 (car s1))
		      (e2 (car s2)))
		  (cond ((< e1 e2) (cons e1 (union-set-list (cdr s1) s2)))
			((> e1 e2) (cons e2 (union-set-list s1 (cdr s2))))
			(else (cons e1 (union-set-list (cdr s1) (cdr s2)))))))))
  (list->tree (union-set-list (tree->list-1 s1) (tree->list-1 s2))))

(define t2 '(5
	     (4
	      (3 () ())
	      ())
	     (7
	      (6 () ())
	      (9 () ()))))
(display (union-set t1 t2))
(newline)

; 习题2.66
; 先写在这里，不做验证
; (define (lookup k s)
;   (define (key e) (car e))
;   (define (element-of-set? e s)
;     (cond ((null? s) #f)
; 	  ((= e (key (entry s))) #t)
; 	  ((< e (key (entry s))) (element-of-set? e (left-branch s)))
; 	  (else (element-of-set? e (left-branch s)))))
