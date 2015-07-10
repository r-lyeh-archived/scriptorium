; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2015
; Placed in the Public Domain
;
; (stddev list)  ==>  real
;
; (load-from-library "stddev.scm")
;
; Return the standard deviation of the given list of values.
;
; Example:   (stddev '(1 1 2 1 1))  ==>  0.4

(load-from-library "variance.scm")

(define (stddev set)
  (sqrt (variance set)))
