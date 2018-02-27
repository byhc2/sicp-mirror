#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (generic-arithmetic))
(use-modules (scheme-number))

(install-scheme-number-package)
(display (add 8 9))
(newline)
