; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (hyper integer1 integer2 integer3)  ==>  integer
;
; Compute A hyper(N) B, where N=INTEGER1, A=INTEGER2, and B=INTEGER3.
;
; Example:   (hyper 4 3 3)  ==>  7625597484987

(define (hyper n a b)
  (cond ((= n 0) (+ 1 b))
        ((= n 1) (+ a b))
        ((= b 1) a)
        ((= n 2) (* a b))
        ((= n 3) (expt a b))
        ((= n 4) (expt a (hyper n a (- b 1))))
        ((> n 4) (hyper (- n 1) a (hyper n a (- b 1))))))
