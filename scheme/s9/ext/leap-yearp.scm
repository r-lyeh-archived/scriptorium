; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (leap-year? integer)  ==>  boolean
;
; Return #T if INTEGER represents a leap year and otherwise return #F.
;
; Example:   (leap-year? 2000)  ==>  #t
;            (leap-year? 2003)  ==>  #f

(define (leap-year? x)
  (or (zero? (remainder x 400))
      (and (zero? (remainder x 4))
           (not (zero? (remainder x 100))))))
