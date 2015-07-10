; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2012
; Placed in the Public Domain
;
; (choose integer1 integer2)  ==>  integer
;
; Return the binomial coefficient ("n choose k") of INTEGER1 (n)
; and INTEGER2 (k), i.e. the number of unique k-tuples that can be
; choosen without repetition from a set of n elements. INTEGER2 (k)
; must be positive.
;
; Example:   (choose 23 1)  ==>  23
;            (choose 23 2)  ==>  253
;            (choose 23 3)  ==>  1771

(define (choose x y)
  (define (product x y)
    (if (= x y)
        x
        (* x (product (+ x 1) y))))
  (quotient (product (+ 1 (- x y)) x)
            (product 1 y)))
