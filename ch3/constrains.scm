#!/usr/bin/guile
!#

; 约束传播

(has-value? connector)
(get-value connector)
(set-value! connector new-value informant)
(forget-value! connector retractor)
(connect connector new-constraint)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond
      ((and (has-value? a1) (has-value? a2))
       (set-value! sum (+ (get-value a1) (get-value a2))
                   me))
      ((and (has-value? sum) (has-value? a1))
       (set-value! a2 (- (get-value sum) (get-value a1))
                   me))
      ((and (has-value? sum) (has-value? a2))
       (set-value! a1 (- (get-value sum) (get-value a2))
                   me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value)
       (process-new-value))
      ((eq? request 'I-lost-my-value)
       (process-forget-value))
      (else
        (error "unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier a1 a2 product)
  (define (process-new-value)
    (cond
      ((or (and (has-value? a1) (= (get-value a1) 0))
           (and (has-value? a2) (= (get-value a2) 0)))
       (set-value! product 0 me))
      ((and (has-value? a1) (has-value? a2))
       (set-value! product (* (get-value a1) (get-value a2)) me))
      ((and (has-value? product) (has-value? a1))
       (set-value! a2 (/ (get-value product) (get-value a1)) me))
      ((and (has-value? product) (has-value? a2))
       (set-value! a1 (/ (get-value product) (get-value a2)) me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value)
       (process-new-value))
      ((eq? request 'I-lost-my-value)
       (process-forget-value))
      (else
        (error "unknown request -- MULTIPLIER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value)
       (process-new-value))
      ((eq? request 'I-lost-my-value)
       (process-forget-value))
      (else
        (error "unknown request -- PROBE" request))))
  (connect connector me))

(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value newval setter)
      (cond
        ((not (has-value? me))
         (set! value newval)
         (set! informant setter)
         (for-each-except setter
                          inform-about-value
                          constraints))
        ((not (= value newval))
         (error "Contradiction" (list value newval)))
        (else 'ignored)))
    ; 只约束设置者撤销约束
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin
            (set! informant #f)
            (for-each-except retractor
                             inform-about-no-value
                             constraints))
          'ignored))
