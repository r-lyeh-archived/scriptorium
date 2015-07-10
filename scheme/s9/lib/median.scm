; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2015
; Placed in the Public Domain
;
; (median list)  ==>  real
;
; (load-from-library "median.scm")
;
; Median (middle value) function, returns the value in the middle of
; a sorted list of values. The input list does not have to be sorted.
;
; Example:   (mean '(1 2 3 4 5 6))  ==>  3.5

(load-from-library "mergesort")

(define (median set)
  (let* ((vs (mergesort <= set))
         (k  (length set))
         (i  (quotient k 2)))
    (if (even? k)
        (/ (+ (list-ref vs i) (list-ref vs (- i 1))) 2)
        (list-ref vs i))))
