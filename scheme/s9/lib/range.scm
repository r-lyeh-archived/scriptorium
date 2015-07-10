; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2015
; Placed in the Public Domain
;
; (range list)  ==>  list
;
; (load-from-library "range.scm")
;
; Return a list containing the minimal and maximal value of a set.
; (- (cadr (range set)) (car (range set))) is the statistical range
; of a set of samples.
;
; Example:   (range '(1 2 3 4 5))  ==>  (1 5)

(load-from-library "mergesort")

(define (range set)
  (let* ((vs  (mergesort <= set))
         (min (car vs))
         (max (car (reverse! vs))))
    (list min max)))
