; 习题2.44

(define (up-split painter n)
  (if (= n 0)
    painter
    (let (smaller (up-split painter (- n 1)))
      (below (beside smaller smaller) painter)
      )
    )
  )

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
	  (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
	    (bottom-right (below right right))
	    (corner (corner-split painter (- n 1))))
	(beside (below painter top-left)
		(below bottom-right corner))))))

; 习题2.45
; 此处需规定，beside为左右放置
; below为下上放置
(define (split op1 op2)
  (lambda (painter n) (if (= n 0)
			painter
			(let (smaller ((split op1 op2) painter (- n 1)))
			  (op1 painter (below smaller smaller))
			 )
		       )
    )
  )

(define right-split (split beside below))
(define up-split (split below beside))
