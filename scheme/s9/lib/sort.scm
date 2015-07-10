; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (sort procedure^2 list)  ==>  list
;
; Sort lists. PROCEDURE^2 is a binary predicate that describes the
; desired order. The original list is not changed.
;
; Example:   (sort <= '(5 3 7 9 1))  ==>  (1 3 5 7 9)

(load-from-library "mergesort.scm")

(define sort mergesort)
