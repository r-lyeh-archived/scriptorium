; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2015
; Placed in the Public Domain
;
; (variance list)  ==>  real
;
; (load-from-library "variance.scm")
;
; Return the variance of the given list of values.
;
; Example:   (variance '(1 1 2 1 1))  ==>  0.16

(load-from-library "mean.scm")

(define (variance set)
  (- (/ (apply + (map (lambda (x) (* x x)) set))
        (length set))
     ((lambda (x) (* x x)) (mean set))))
