#!/usr/bin/guile
!#

(add-to-load-path ".")
(load "fifo.scm")

(define (make-wire i)
  (let ((signal-value i)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures)))
      ;(proc))
    (define (dispatch m)
      (cond
        ((eq? m 'get-signal) signal-value)
        ((eq? m 'set-signal!) set-my-signal!)
        ((eq? m 'add-action!) accept-action-procedure!)
        (else (error "unknown operation -- WIRE " m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal w)
  (w 'get-signal))
(define (set-signal! w new-value)
  ((w 'set-signal!) new-value))
(define (add-action! wire action)
  ((wire 'add-action!) action))

(define (inverter input output)
  (define (logical-not v)
    (if (= v 0)
        1
        0))
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (logical-and v1 v2)
    (if (and (= v1 1) (= v2 1))
        1
        0))
  (define (and-action-procedure)
    (let ((new-value (logical-and
                       (get-signal a1)
                       (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (logical-or v1 v2)
    (if (and (= v1 0) (= v2 0))
        0
        1))
  (define (or-action-procedure)
    (let ((new-value (logical-or
                       (get-signal a1)
                       (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (after-delay dl action)
  (add-to-agenda! (+ dl (c-time the-agenda))
                  action
                  the-agenda)
  (display the-agenda)
  (newline)
  )

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (c-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segment agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (propagate)
  (if (empty-agenda? the-agenda)
      (begin
        (display "propagate finish")
        (newline)
        )
      (let ((first-item (first-agenda-item the-agenda)))
        (display "do propagate")
        (newline)
        (display first-item)
        (newline)
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))
      ))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display "  ")
                 (display (c-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segment agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

; 习题3.31
;(define i (make-wire))
;(define o (make-wire))
;(inverter i o)
;(set-signal! i 1)
;(propagate)
; 以上代码为例
; 行18不调用proc时
; (inverter i o)仅放invert-input到i之action-procedures
; the-agenda空
; (set-signal! i 1)设i为1，调用i之所有action-procedures，列invert-input之内lambda入the-agenda
; (propagate)调用the-agenda之lambda，设o之值为0

; 行18调用proc时
; (inverter i o)列invert-input入i之action-procedures，列invert-input之lambda入the-agenda
; the-agenda有行51之lambda
; (set-signal! i 1)设i为1，调用i之所有action-procedures，再列invert-input之内lambda入the-agenda
; the-agenda有二行51之lambda
; (propagate)调用the-agenda之lambda，设o之值为1
; (propagate)再调用the-agenda之lambda，设o之值为0

; 行18不调用proc时
; 若加probe于o上，则仅列probe于o之action-procedures内
; the-agenda无probe内容
; propagate亦无从调用probe显示

; 以上也可以此来看：
; 于逻辑数字电路，propagate如加电于电路，则电路非混沌，inverter之i
; o必一为0，一为1，或反之
; 然，初始时，make-wire所造i o俱为0
; 加电时，o须立即由0变1。此18行proc之功劳也
; 若非是，则加电后，i
; o俱为0，设i为1时，o实际无须变化，set-my-signal!之if判断无需执行
; 即set-signal!无需一一调用action-procedures
; 故连于o之probe无需执行

; 然，可于make-wire加一初始值设置线路，则probe无需先执行也
