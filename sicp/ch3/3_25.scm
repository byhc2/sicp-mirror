#!/usr/bin/guile
!#

; 习题3.25

; 此题目要点在于，构造表格的递归类型，以及insert!运算需要保持封闭
;  |
;  v
; [* *]----->[*  *]---->[*  *]------>[*   *]----->
;  |          |  |       |  |         |   |
;  v          v  v       v  v         v   v
;  *table*    K  V       K  V         K   V

; 每个V类型又是一个表格（重复上述结构），或者是一个值
;  |
;  v
; [*         *]
;  |         |
;  v         v
;  *value*   <DATA>


; 两次错误的尝试
; 1, 去掉了表格的头节点
; -->[*  *]---->[*  *]------>[*   *]----->
;     |  |       |  |         |   |
;     v  v       v  v         v   v
;     K  V       K  V         K   V
; 此问题在于V的结构不好确定，无法确定是值还是子表格

; 2, 保留头节点，但是每次操作都以cdr table为参数
; 此问题和1一样，此外，insert!运算无法封闭（接受table参数，返回的是(cdr table)）

; 综上，两大要点，insert!保持封闭及递归

(define (make-table same-key?)
  (let ((local-table (list '*table*)))

    (define (find-key key records)
      (cond
        ((null? records) #f)
        ((is-value? records) #f)
        ((same-key? key (caar records)) (car records))
        (else (find-key key (cdr records)))))

    ; (*table* (2 (3 . 2)) (1 (1 . 2)))
    (define (lookup table . keys)
      (if (null? keys)
          (error "empty key list")
          (let ((subtable (find-key (car keys) (cdr table))))
            (if subtable
                (if (null? (cdr keys))
                    (if (is-value? (cdr subtable))
                        (cddr subtable)
                        (cdr subtable))
                    (apply lookup (cdr subtable) (cdr keys)))
                #f))))

    (define (make-value v)
      (cons '*value* v))
    (define (is-value? v)
      (eq? (car v) '*value*))

    (define (insert! table value . keys)
      (if (null? (cdr keys))
          (let ((record (find-key (car keys) (cdr table))))
            (if record
                (set-cdr! record (make-value value))
                (let ((v (make-value value)))
                  (set-cdr! table
                            (cons (cons (car keys) v)
                                  (cdr table))))))
          (let ((subrec (find-key (car keys) (cdr table))))
            (if subrec
                (if (is-value? (cdr subrec))
                    ; 如果是值类型
                    (set-cdr! subrec
                              (apply insert! (list '*table*) value (cdr keys)))
                    (apply insert! (cdr subrec) value (cdr keys)))
                (set-cdr! table
                          (cons
                            (cons (car keys) (apply insert! (list '*table*) value (cdr keys)))
                            (cdr table))))))
      table)

    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) (lambda (keys) (apply lookup local-table keys)))
        ((eq? m 'insert-proc!) (lambda (value keys) (apply insert! local-table
                                                           value keys)))
        ((eq? m 'disp) local-table)
        (else
          (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (lookup table . keys)
  ((table 'lookup-proc) keys))
(define (insert! table value . keys)
  ((table 'insert-proc!) value keys))
(define (disp table)
  (table 'disp))

(define xt (make-table eq?))
(insert! xt 2 1)
(insert! xt 2 1 1)
(insert! xt 5 2 3)
(insert! xt 8 2 3 5)
(insert! xt 55 2 3 5 8 13 21 34)
(display (lookup xt 1 1))
(newline)
(display (lookup xt 2 3 5 8 13 21 34))
(newline)

; 习题3.26
; 将每个列表的cdr变成一棵树

; 习题3.27
; 画图略。若以(memoize
; fib)为memo-fib，则每次均创建新表格，在新环境计算，查表无用
