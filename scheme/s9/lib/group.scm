; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (group list integer)  ==>  list
;
; Group the elements of LIST in tuples of INTEGER elements
; each (except for the last tuple, which may contain fewer
; elements). Return a list of the resulting tuples. The
; elements appear in the output list in the same order as
; in the input list.
;
; INTEGER must be positive.
;
; Example:   (group '(1 2 3 4 5) 2)  ==>  ((1 2) (3 4) (5))
;            (group '(1 2 3 4 5) 5)  ==>  ((1 2 3 4 5))

(load-from-library "take.scm")

(define (group a n)
  (let group ((a a)
              (k (length a))
              (r '()))
    (if (< n k)
        (group (list-tail a n)
               (- k n)
               (cons (take a n) r))
        (reverse! (cons a r)))))
