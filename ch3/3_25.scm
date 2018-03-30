#!/usr/bin/guile
!#

; 习题3.25

(define (make-table same-key?)
  (let ((local-table (list '*table*)))

    (define (find-key key records)
      (cond
        ((null? records) #f)
        ((not (list? records)) records)
        ((same-key? key (caar records)) (car records))
        (else (find-key key (cdr records)))))

    ; (*table* (2 (3 . 2)) (1 (1 . 2)))
    (define (lookup table . keys)
      (if (null? keys)
          (error "empty key list")
          (let ((subtable (find-key (car keys) (cdr table))))
            (if subtable
                (if (null? (cdr keys))
                    (cdr subtable)
                    (apply lookup subtable (cdr keys)))
                #f))))

    (define (insert! table value . keys)
      (cond

        ; 说明已经到值了
        ((not (list? (cdr table)))
         (set-cdr! table (apply insert! (list (car keys)) value (cdr keys))))

        ((null? (cdr keys))
         ; key列表已经到了最后一层
         ; 但表还可以往下递归
         (let ((record (find-key (car keys) (cdr table))))
           (if record
               (set-cdr! record value)
               (set-cdr! table
                         (cons (cons (car keys) value)
                               (cdr table))))))

        (else (let ((subtable (find-key (car keys) (cdr table))))
                (if subtable
                    (apply insert! subtable value (cdr keys))
                    (set-cdr! table
                              (cons
                                (apply insert! (list (car keys)) value (cdr keys))
                                (cdr table)))))))
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
(insert! xt 2 1 1)
(insert! xt 5 2 3)
;(insert! xt 8 2 3 5)
;(display (lookup xt 1 1))
;(newline)
;(display (lookup xt 2 3 9))
;(newline)
(display (disp xt))
(newline)

;(define x '(*table* (2 (3 . 2)) (1 (1 . 2))))
;
;(define (find-key key records)
;  (display '==================)
;  (newline)
;  (display key)
;  (newline)
;  (display records)
;  (newline)
;  (display '==================)
;  (newline)
;  (cond
;    ; ((1 (1 . 2)))
;    ((null? records) #f)
;    ((eq? key (caar records)) (car records))
;    (else (find-key key (cdr records)))))
;
;(define (lookup table . keys)
;  (if (null? keys)
;      (error "empty key list")
;      (let ((subtable (find-key (car keys) (cdr table))))
;        (if subtable
;            (if (null? (cdr keys))
;                subtable
;                (apply lookup subtable (cdr keys)))
;            #f))))
;
;(display (lookup x 1 1))
;(newline)
