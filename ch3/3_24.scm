#!/usr/bin/guile
!#

; 习题3.24

(define (make-table same-key?)
  (let ((local-table (list '*table*)))

    (define (find-key key records)
      (cond
        ((null? records) #f)
        ((same-key? key (caar records)) (car records))
        (else (find-key key (cdr records)))))

    (define (lookup key-1 key-2)
      (let ((subtable (find-key key-1 (cdr local-table))))
        (if subtable
            (let ((record (find-key key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (find-key key-1 (cdr local-table))))
        (if subtable
            (let ((record (find-key key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else
          (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (lookup k1 k2 t)
  ((t 'lookup-proc) k1 k2))
(define (insert! t k1 k2 v)
  ((t 'insert-proc!) k1 k2 v))

(define xt (make-table eq?))
(insert! xt 1 1 2)
(insert! xt 1 2 3)
(insert! xt 2 3 5)
(insert! xt 3 5 8)
(display (lookup 2 3 xt))
(newline)
