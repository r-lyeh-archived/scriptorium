; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2015
; Placed in the Public Domain
;
; (cdf real)  ==>  real
;
; (load-from-library "cdf.scm")
;
; Compute the quantile of the given standard deviation (cumulative
; distribution function). Results will get flaky beyond 5th standard
; deviation; see ERF for details.
;
; Example:   (cdf 0)  ==>  0.5

(load-from-library "erf.scm")

(define (cdf x)
  (+ 0.5 (* 0.5 (erf (/ x (sqrt 2))))))
