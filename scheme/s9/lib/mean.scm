; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2015
; Placed in the Public Domain
;
; (mean list)  ==>  real
;
; (load-from-library "mean.scm")
;
; Mean (average) function, returns the arithmetic mean value of a
; list of reals.
;
; Example:   (mean '(1 2 3 4 5 6))  ==>  3.5

(define (mean set)
  (/ (apply + set) (length set)))
