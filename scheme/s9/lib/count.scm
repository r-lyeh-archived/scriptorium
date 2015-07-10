; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2010
; Placed in the Public Domain
;
; (count pair)  ==>  integer
;
; Count atomic members of a pair.
;
; Example:   (count '(a (b (c)) d . e))  ==>  5

(define (count x)
  (cond ((null? x) 0)
        ((pair? x)
          (+ (count (car x))
             (count (cdr x))))
        (else 1)))
