#!/usr/bin/guile
!#

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? e) (eq? (car e) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (has-symbol sym set)
  (cond ((null? set) #f)
        ((eq? sym (car set)) #t)
        (else (has-symbol sym (cdr set)))))

(define (decode bits tree)
  (define (decode-1 bit current-branch)
    (if (null? bit)
      '()
      (let ((next-branch (choose-branch (car bit) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch) (decode-1 (cdr bit) tree))
          (decode-1 (cdr bit) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit"))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((<= (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (cons (make-leaf (car pair) (cadr pair)) (make-leaf-set (cdr pairs))))))

; 习题2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(display (decode sample-message sample-tree))
(newline)

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

; 习题2.68

(define (encode-symbol sym tree)
  (define (iter cur branch)
    (if (leaf? branch)
      cur
      (let ((left-sym-set (symbols (left-branch branch)))
            (right-sym-set (symbols (right-branch branch))))
        (cond ((has-symbol sym left-sym-set) (iter (append cur '(0)) (left-branch branch)))
              ((has-symbol sym right-sym-set) (iter (append cur '(1)) (right-branch branch)))))))
  (iter '() tree))

(display (encode '(A A A) sample-tree))
(newline)
(display (encode '(A D A B B C A) sample-tree))
(newline)

; 习题2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; leaf-pairs有序
(define (successive-merge leaf-pairs)
  (cond ((null? leaf-pairs) '())
        ((= 1 (length leaf-pairs)) (car leaf-pairs))
        (else (let ((new-node (make-code-tree (car leaf-pairs) (cadr leaf-pairs))))
                (successive-merge (adjoin-set new-node (cddr leaf-pairs)))))))

(display (successive-merge (map (lambda (x) (make-leaf (car x) (cadr x))) '((A 1) (B 1) (C 3) (D 4)))))
(newline)

; 习题2.70
(define song-sym '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
(define (rank-sym pairs)
  (define (accumulate cur rest)
    (if (null? rest)
      cur
      (let ((one-sym (car (car rest)))
            (one-weight (cadr (car rest))))
      (accumulate (adjoin-set (make-leaf one-sym one-weight) cur) (cdr rest)))))
  (accumulate '() pairs))

(define the-song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(display "2.70")
(newline)
(define song-tree (successive-merge (rank-sym song-sym)))
(display (encode the-song song-tree))
(newline)
(display (decode (encode the-song song-tree) song-tree))
(newline)

; 习题2.71
; 最高频符号1比特，最低频符号n-1比特

; 习题2.72
; 编码的算法复杂度大致为O(n*log(T))，n是信息字符数，T是表的大小
