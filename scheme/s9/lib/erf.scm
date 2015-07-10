; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2015
; Placed in the Public Domain
;
; (erf real)  ==>  real
;
; (load-from-library "erf.scm")
;
; Gauss error function. Used to compute the integral of the normal
; distribution function (NDF).
;
; Using an Abramowitz-Stegun approximation with a maximum error of
; 3 * 10^-7, i.e. results will start to get flaky beyond the 5th
; standard deviation.
;
; Example:   (erf 0)  ==>  0.0

(define (erf x)
  (if (< x 0)
      (- (erf (- x)))
      (- 1 (/ (expt (+ 1 (* 0.0705230784 x)
                         (* 0.0422820123 (* x x))
                         (* 0.0092705272 (* x x x))
                         (* 0.0001520143 (* x x x x))
                         (* 0.0002765672 (* x x x x x))
                         (* 0.0000430638 (* x x x x x x)))
                    16)))))
    
