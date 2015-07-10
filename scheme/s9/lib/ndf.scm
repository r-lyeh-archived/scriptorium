; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2015
; Placed in the Public Domain
;
; (ndf real)   ==>  real
; (ndf* real)  ==>  real
;
; (load-from-library "ndf.scm")
;
; Compute the probability density at the given standard deviation
; (normal distribution function). Assuming a standard normal
; distribution.
;
; (Example:)   (ndf 0)  ==>  0.4

(define (ndf x)
  (* (/ (sqrt (* 2 pi)))
     (expt 2.71828182845904523536028747135266249775724709369995
           (/ (- (* x x)) 2))))

(define (ndf* x mu sigma)
  (* (/ (* sigma (sqrt (* 2 pi))))
     (expt 2.71828182845904523536028747135266249775724709369995
           (- (/ (* (- x mu) (- x mu)) (* 2 (* sigma sigma)))))))
