#!/usr/bin/guile
!#

; 习题2.86

(add-to-load-path ".")
(use-modules (generic-arithmetic))
(use-modules (scheme-number))
(use-modules (rational-number))
(use-modules (complex-number))

(install-scheme-number-package)
(install-rational-number-package)
(install-complex-number-package)

(define var1 (make-complex-from-real-imag 2 0))
(define var2 (make-rat 2 1))

;(display (drop (make-complex-from-real-imag 2 0)))
;(display (numer (make-complex-from-real-imag 2 0)) 2)
;(display (type-tag var1))
(newline)
;(display (coercion-util 'complex '() '()))
(newline)
;(display (numer var2))
(newline)
;(display (can-coercion 'complex '()))
(newline)
;(display (get 'numer '(complex)))
(newline)
(display (drop var1))
(newline)
