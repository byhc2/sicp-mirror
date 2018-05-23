#!/usr/bin/racket
#lang scheme

; 习题3.59
(require "stream.scm")
(require "series.scm")

; 习题3.60
; 可见，级数前10项和已接近1.0
(display 
  (stream-accumulate
    (add-streams
      (mul-series sine-series sine-series)
      (mul-series cosine-series cosine-series))
    10))

(newline)
