#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (generic-arithmetic))
(use-modules (scheme-number))
(use-modules (rational-number))
(use-modules (complex-number))

(install-scheme-number-package)
(install-rational-number-package)
(install-complex-number-package)

(display (add 8 9))
(newline)

(display (add (make-rat 3 4) (make-rat 7 8)))
(newline)

(display (add (make-complex-from-real-imag 1 1)
              (make-complex-from-real-imag 2 2)))
(newline)
(display (mul (make-complex-from-real-imag 1 1)
              (make-complex-from-real-imag 2 2)))
(newline)

(display (mag (make-complex-from-real-imag 1 1)))
(newline)

(display (equ? 4 4))
(newline)

(display (equ? (make-rat 3 4) (make-rat 6 8)))
(newline)

(display (equ? (make-rat 3 4) (make-rat 5 8)))
(newline)

(display (equ? (make-complex-from-real-imag 1 2)
               (make-complex-from-real-imag 1 2)))
(newline)

(display (zero? (make-complex-from-real-imag 0 0)))
(newline)
