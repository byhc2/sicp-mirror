#!/usr/bin/guile
!#

; 习题3.38
; a 1->2->3  45元
;   1->3->2  32元
;   2->1->3  45元
;   2->3->1  50元
;   3->1->2  40元
;   3->2->1  40元
; b 每人操作可分为两个步骤：
;   A 检查余额，并计算新值
;   B 写回新值
;   书中set!分三步，然其一二步分开与否对结果无影响，故合二为一
;   以Ax为第x人的A操作，Bx类似
;   Ax必在Bx之前
;   如此，有90种排列方式
;   此处不一一列举结果


; 习题3.39
; 101和121


; 习题3.40
; 书中(lambda () (set! x (* x x)))将访问两次x值
; 此与之前认为的仅一次访问有区别
; 因此构成多种结果
; (lambda () (set! x (* x x))) 读二，写回一，记R11R12W1
; (lambda () (set! x (* x x x))) 读三，写回一，记R21R22R23W2
; 交错产生35种排列
; 若W1最后完成，则R21R22R23均不受W1影响
; 仅W2与R11R12的顺序影响W1
; W2->R11->R12 ---> 1000000
; R11->W2->R12 ---> 10000
; R11->R12->W2 ---> 100
; 若W2最后完成，则R11R12不受W2影响
; 仅W1与R21R22R23顺序影响W2
; W1->R21->R22->R23 ---> 1000000
; R21->W2->R22->R23 ---> 100000
; R21->R22->W2->R23 ---> 10000
; R21->R22->R23->W2 ---> 1000
; 串行化以后，仅1000000一结果

; 习题3.41
; 没有必要，因为读取操作的先后顺序无所谓

; 习题3.42
; 此题不会。按照网上说法，若以make-serializer构造一个列表，列表中过程顺序执行来看
; 新版make-account实际上是执行了make-serializer列表中同一个元素三次
; 这三次并发不受make-serializer控制

; 习题3.43 略

; 习题3.44
; transfer不是一个原子操作，有中间状态
; exchange是一个原子操作，无中间状态

; 习题3.45
; 如此调用serialized-exchange时，将发生
; ((serializer1 (serializer2 exchange)) acc1 acc2)
; serializer1和serializer2本身会锁住acc1和acc2，然后调用exchange
; exchange内将再次试图锁住acc1和acc2，从而导致死锁

; 习题3.46 略
; 习题3.47 略
; 习题3.48 略
; 习题3.49 略
